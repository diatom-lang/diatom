use std::cell::RefCell;
use std::fmt::Write;
use std::{ffi::OsStr, rc::Rc};

use ahash::AHashSet;
use either::Either;

mod error;
pub mod gc;
mod prelude;
mod register_table;
mod string_pool;

mod state;
use crate::vm::op::{OpGe, OpLe, OpLt, OpMakeTable, OpNe, OpSetAttr};
use crate::{
    diagnostic::{Diagnostic, Loc},
    frontend::{
        parser::ast::{Const, Expr, OpInfix, OpPrefix, Stmt},
        Ast, Parser,
    },
    vm::{
        error::VmError,
        op::{
            OpAdd, OpAllocReg, OpAnd, OpBranchFalse, OpBranchTrue, OpCallClosure, OpDiv, OpDummy,
            OpEq, OpGt, OpIDiv, OpJump, OpLoadConstant, OpMakeClosure, OpMove, OpMul, OpNeg, OpNot,
            OpOr, OpPow, OpRem, OpRet, OpSub, OpYield,
        },
        Instruction, Ip, Vm, VmInst,
    },
    DiatomValue, IoWrite,
};
pub use gc::{GcObject, Reg};
pub use state::State;

use error::ErrorCode;
pub use gc::Gc;
use prelude::impl_prelude;
pub use register_table::Capture;
use register_table::{ConstantValue, Loop, RegisterTable};

#[derive(Clone)]
pub struct FutureJump {
    previous_insert_offset: usize,
    condition_reg: Option<(usize, bool)>,
    previous_inst_offset: usize,
    loc: Loc,
}

impl FutureJump {
    /// Insert jump from current loc to previous
    pub fn patch_backward(self, func: &mut Func, current_insert_offset: usize) {
        let inserted = current_insert_offset - self.previous_insert_offset;
        let inst_offset = self.previous_inst_offset + inserted;
        let jump_offset = inst_offset as i64 - func.insts.len() as i64;
        let op = if let Some((reg, on_false)) = self.condition_reg {
            if on_false {
                VmInst::OpBranchFalse(OpBranchFalse {
                    loc: self.loc,
                    condition: reg,
                    offset: jump_offset,
                })
            } else {
                VmInst::OpBranchTrue(OpBranchTrue {
                    loc: self.loc,
                    condition: reg,
                    offset: jump_offset,
                })
            }
        } else {
            VmInst::OpJump(OpJump {
                loc: self.loc,
                offset: jump_offset,
            })
        };
        func.insts.push(op);
    }

    /// Insert jump from previous to current
    pub fn patch_forward(self, func: &mut Func, current_insert_offset: usize) {
        let inserted = current_insert_offset - self.previous_insert_offset;
        let inst_offset = self.previous_inst_offset + inserted;
        let jump_offset = func.insts.len() as i64 - inst_offset as i64;
        let op = if let Some((reg, on_false)) = self.condition_reg {
            if on_false {
                VmInst::OpBranchFalse(OpBranchFalse {
                    loc: self.loc,
                    condition: reg,
                    offset: jump_offset,
                })
            } else {
                VmInst::OpBranchTrue(OpBranchTrue {
                    loc: self.loc,
                    condition: reg,
                    offset: jump_offset,
                })
            }
        } else {
            VmInst::OpJump(OpJump {
                loc: self.loc,
                offset: jump_offset,
            })
        };
        func.insts[inst_offset] = op;
    }
}

pub struct Func {
    pub name: String,
    pub parameters: usize,
    pub insts: Vec<VmInst>,
}

/// # The Diatom Interpreter
///
/// High performance interpreter for the diatom programming language. This interpreter compiles
/// diatom source code into byte code and executes the byte code with carefully tuned virtual
/// machine. Our benchmark shows it can reach up to **2x** the execution speed of Lua 5.4 .
///
/// # Example
///
/// ## 1. Run a piece of code
/// ```
/// use diatom::Interpreter;
///
/// // Create a new instance of interpreter
/// let mut interpreter = Interpreter::new(std::io::stdout());
/// // Execute source code
/// let output = interpreter.exec(
///     "print$('Hello, world!')",
///     Default::default(),
///     false).unwrap();
/// ```
///
/// ## 2. Add call back to the interpreter
/// ```
/// use std::{cell::Cell, rc::Rc};
/// use diatom::{DiatomValue, Interpreter};
///
/// // this value will be modified
/// let value = Rc::new(Cell::new(0));
/// let value_capture = value.clone();
///
/// let mut interpreter = Interpreter::new(std::io::stdout());
/// // add a callback named "set_value"
/// interpreter.add_extern_function("set_value", move |_state, parameters, _out| {
///     if parameters.len() != 1 {
///         return Err("Expected 1 parameter!".to_string());
///     }
///     match parameters[0] {
///         DiatomValue::Int(i) => {
///             value_capture.set(i);
///             Ok(DiatomValue::Unit)
///         }
///         _ => Err("Invalid type".to_string()),
///     }
/// });
///
/// // change value to 5
/// interpreter.exec("set_value$(5)", Default::default(), false).unwrap();
/// assert_eq!(value.get(), 5);
/// ```
pub struct Interpreter<Buffer: IoWrite> {
    registers: RegisterTable,
    scopes: Vec<AHashSet<String>>,
    byte_code: Vec<Func>,
    vm: Vm,
    gc: Gc<Buffer>,
    out: Buffer,
}

impl<Buffer: IoWrite> Interpreter<Buffer> {
    /// Create a new interpreter instance
    pub fn new(buffer: Buffer) -> Self {
        let main = Func {
            name: "main".to_string(),
            parameters: 0,
            insts: vec![],
        };
        let mut interpreter = Self {
            registers: RegisterTable::new(0),
            scopes: vec![AHashSet::new()],
            byte_code: vec![main],
            vm: Vm::new(),
            gc: Gc::new(),
            out: buffer,
        };
        impl_prelude(&mut interpreter);
        interpreter
    }

    /// Register an external rust function
    ///
    /// This function does not accept due to potential recursive calls on a FnMut would violating
    /// borrow rules. You may want to use interior mutability if Fn is not flexible enough.
    /// External function should **NEVER PANIC**, otherwise it will crush the virtual machine.
    ///
    /// # External function parameters:
    /// * `State` - Access state and heap memory of the virtual machine.
    /// * `[DiatomValue]` - Parameters passed. The function is expected to check type and the
    /// number of parameters it received.
    /// * `Buffer` - Output buffer
    ///
    /// # External function return value:
    /// * Return a single unboxed value as return value. If the function does not intended to
    /// return anything, return an unit type `DiatomValue::Unit`.
    /// * If any unrecoverable error happens, return an `Err(String)` that illustrates the error.
    /// This will cause virtual machine to enter **panic mode** and stop execution.
    /// * If return value is `DiatomValue::Str` or `DiatomValue::Ref`, the reference id is checked.
    /// An invalid id would cause virtual machine to enter **panic mode** and stop execution.
    ///
    /// # Examples:
    /// ```
    /// use std::io::Write;
    /// use diatom::{Interpreter, DiatomValue};
    ///
    /// let buffer = Vec::<u8>::new();
    /// let mut interpreter = Interpreter::new(buffer);
    /// interpreter.add_extern_function(
    ///     "hello_world",
    ///     |state, parameters, out| {
    ///         if !parameters.is_empty(){
    ///             Err("Too many parameters!".to_string())
    ///         }else{
    ///             write!(out, "Hello, world!");
    ///             Ok(DiatomValue::Unit)
    ///         }
    ///     }
    /// );
    ///
    /// interpreter.exec("hello_world$()", Default::default(), false).unwrap();
    /// let output = interpreter.replace_buffer(Vec::<u8>::new());
    /// let output = String::from_utf8(output).unwrap();
    /// assert_eq!(output, "Hello, world!")
    /// ```
    pub fn add_extern_function<F>(&mut self, name: impl Into<String>, f: F)
    where
        F: Fn(&mut State<Buffer>, &[DiatomValue], &mut Buffer) -> Result<DiatomValue, String>
            + 'static,
    {
        let f = GcObject::NativeFunction(Rc::new(RefCell::new(f)));
        let gc_id = self.gc.alloc(f);
        let reg = Reg::Ref(gc_id);
        let reg_id = self.registers.declare_variable(name.into(), None);
        self.gc.alloc_reg_file(reg_id + 1);
        self.gc.write_reg(reg_id, reg);
    }

    /// Check if input is completeness
    ///
    /// Incomplete input usually contains unclosed parentheses, quotes or open expression.
    pub fn verify_input_completeness(&self, code: impl AsRef<str>) -> bool {
        let mut parser = Parser::new();
        let ast = parser.parse_str(OsStr::new("<interactive>"), code.as_ref());
        !ast.input_can_continue()
    }

    /// Show decompiled byte code for given source code.
    ///
    /// If compilation failed, `Err` will be returned.
    pub fn decompile(
        &mut self,
        code: impl AsRef<str>,
        source: &OsStr,
        color: bool,
    ) -> Result<String, String> {
        self.compile(code, source, color)?;
        let mut decompiled = String::new();
        for Func {
            name,
            parameters,
            insts,
        } in self.byte_code.iter()
        {
            writeln!(decompiled, "Function: {name}\nParameters: {parameters}").unwrap();
            writeln!(decompiled, "Body:").unwrap();
            let pad = insts.len().ilog10() as usize + 1;
            for (n, inst) in insts.iter().enumerate() {
                write!(decompiled, "    {n: <pad$} ").unwrap();
                inst.decompile(&mut decompiled, &self.gc);
            }
            writeln!(decompiled).unwrap();
        }
        Ok(decompiled)
    }

    /// Replace output buffer and get the old one
    pub fn replace_buffer(&mut self, buffer: Buffer) -> Buffer {
        std::mem::replace(&mut self.out, buffer)
    }

    fn compile(
        &mut self,
        code: impl AsRef<str>,
        source: &OsStr,
        color: bool,
    ) -> Result<Ast, String> {
        let mut parser = Parser::new();
        let mut ast = parser.parse_str(source, code.as_ref());
        if ast.diagnoser.error_count() > 0 {
            return Err(ast.diagnoser.render(color));
        }

        let registers_prev = self.registers.clone();
        self.vm.set_ip(Ip {
            func_id: 0,
            inst: self.byte_code[0].insts.len(),
        });

        self.registers.current_start_loc = self.byte_code[0].insts.len();
        self.registers.insert_offset = 0;

        let return_value = self.compile_ast(&mut ast).map_err(|_| {
            // restore variable table if compile failed
            self.registers = registers_prev;
            ast.diagnoser.render(color)
        })?;

        // return after main
        self.byte_code[0].insts.push(VmInst::OpYield(OpYield {
            show_id: return_value,
        }));

        //
        self.byte_code[0].insts.insert(
            self.registers.current_start_loc,
            VmInst::OpAllocReg(OpAllocReg {
                n_reg: self.registers.assigned,
            }),
        );
        Ok(ast)
    }

    /// Run a piece of diatom source code
    ///
    /// # Parameters
    /// * `code` - Source code
    /// * `source` - name of source code file or where it is from
    /// * `color` - render output with ansi color. Set to false if you do not want to print the
    /// output to terminal.
    ///
    /// # Return
    /// * Return the output of the program
    /// * If compilation failed or error occurs durning execution, an `Err(String)` that
    /// illustrates the error is returned.
    pub fn exec(
        &mut self,
        code: impl AsRef<str>,
        source: &OsStr,
        color: bool,
    ) -> Result<(), String> {
        let mut ast = self.compile(code, source, color)?;
        match self.vm.exec(&self.byte_code, &mut self.gc, &mut self.out) {
            VmError::Yield(_) => Ok(()),
            error_code => {
                let diagnostic = Diagnostic::from(error_code);
                ast.diagnoser.push(diagnostic);
                Err(ast.diagnoser.render(color))
            }
        }
    }

    pub(crate) fn exec_repl(&mut self, code: impl AsRef<str>, color: bool) -> Result<(), String> {
        let mut ast = self.compile(code, OsStr::new("<interactive>"), color)?;
        match self.vm.exec(&self.byte_code, &mut self.gc, &mut self.out) {
            VmError::Yield(None) => Ok(()),
            VmError::Yield(Some(reg_id)) => {
                let reg = self.gc.read_reg(reg_id);
                match reg {
                    // omit unit output in repl
                    Reg::Unit => Ok(()),
                    Reg::Bool(b) => writeln!(self.out, "{b}"),
                    Reg::Int(i) => writeln!(self.out, "{i}"),
                    Reg::Float(f) => writeln!(self.out, "{f}"),
                    Reg::Str(sid) => writeln!(self.out, "{}", self.gc.get_string_by_id(*sid)),
                    Reg::Ref(r) => match &self.gc[*r] {
                        GcObject::Closure {
                            func_id,
                            parameters: _,
                            reg_size: _,
                            captured: _,
                        } => {
                            writeln!(self.out, "Closure[{func_id}]")
                        }
                        GcObject::NativeFunction(f) => {
                            write!(self.out, "External function@{:p}", f.as_ptr())
                        }
                        GcObject::Table(t) => {
                            let mut table = "{".to_string();
                            for (i, (k, v)) in t.attributes.iter().enumerate() {
                                let content = match v {
                                    Reg::Unit => "()".to_string(),
                                    Reg::Bool(b) => format!("{b}"),
                                    Reg::Int(i) => format!("{i}"),
                                    Reg::Float(f) => format!("{f}"),
                                    Reg::Str(sid) => {
                                        format!("'{}'", self.gc.get_string_by_id(*sid))
                                    }
                                    Reg::Ref(r) => format!("Ref@<{r}>"),
                                };
                                if i == t.attributes.len() - 1 {
                                    write!(table, "{k} = {content}").unwrap();
                                } else {
                                    write!(table, "{k} = {content}, ").unwrap();
                                }
                            }
                            write!(table, "}}").unwrap();
                            writeln!(self.out, "{table}")
                        }
                    },
                }
                .map_err(|err| {
                    let error_code = VmError::IoError {
                        loc: None,
                        error: err,
                    };
                    let diagnostic = Diagnostic::from(error_code);
                    ast.diagnoser.push(diagnostic);
                    ast.diagnoser.render(color)
                })
            }
            error_code => {
                let diagnostic = Diagnostic::from(error_code);
                ast.diagnoser.push(diagnostic);
                Err(ast.diagnoser.render(color))
            }
        }
    }

    /// if compile succeeded, return last expression's reg id
    fn compile_ast(&mut self, ast: &mut Ast) -> Result<Option<usize>, ()> {
        let mut return_value = None;
        let mut has_error = false;
        for (i, stmt) in ast.statements.iter().enumerate() {
            match self.compile_stmt(stmt, i != ast.statements.len() - 1) {
                Ok(ret) => return_value = ret,
                Err(code) => {
                    has_error = true;
                    ast.diagnoser.push(Diagnostic::from(code));
                }
            }
        }

        if has_error {
            Err(())
        } else {
            Ok(return_value.map(|(reg, _)| reg))
        }
    }

    /// Compile a statement
    /// Return value is already properly freed
    fn compile_stmt(
        &mut self,
        stmt: &Stmt,
        discard: bool,
    ) -> Result<Option<(usize, bool)>, ErrorCode> {
        let mut return_value = None;
        match stmt {
            Stmt::Expr {
                loc: _,
                expr:
                    Expr::Infix {
                        loc,
                        op: OpInfix::Assign,
                        lhs,
                        rhs,
                    },
            } => self.compile_assignment(lhs, rhs, loc.clone())?,
            Stmt::Expr { loc: _, expr } => {
                let (reg_id, tmp) = self.compile_expr(expr, discard)?;
                return_value = Some((reg_id, tmp));
            }
            Stmt::Loop {
                loc,
                condition,
                body,
            } => {
                let jump_to_start = FutureJump {
                    previous_insert_offset: self.registers.insert_offset,
                    condition_reg: None,
                    previous_inst_offset: self.get_current_func().insts.len(),
                    loc: loc.clone(),
                };
                let branch_inst = if let Some(condition) = condition {
                    let (condition_reg, tmp) = self.compile_expr(condition, false)?;
                    if tmp {
                        self.registers.free_intermediate(condition_reg);
                    }
                    self.get_current_func().insts.push(VmInst::OpDummy(OpDummy));
                    Some(FutureJump {
                        previous_insert_offset: self.registers.insert_offset,
                        condition_reg: Some((condition_reg, false)),
                        previous_inst_offset: self.get_current_func().insts.len() - 1,
                        loc: condition.get_loc(),
                    })
                } else {
                    None
                };
                self.enter_block();
                let current_inst = self.get_current_func().insts.len();
                self.registers.loops.push(Loop {
                    start_inst_offset: current_inst,
                    start_insert_offset: self.registers.insert_offset,
                    breaks: vec![],
                });
                for stmt in body.iter() {
                    self.compile_stmt(stmt, true).map_err(|err| {
                        self.leave_block();
                        err
                    })?;
                }
                let breaks = self.registers.loops.pop().unwrap().breaks;
                self.leave_block();

                // patch jump to loop start
                let current_insert_offset = self.registers.insert_offset;
                jump_to_start.patch_backward(self.get_current_func(), current_insert_offset);

                // patch breaks
                breaks.into_iter().for_each(|jump| {
                    let current_insert_offset = self.registers.insert_offset;
                    jump.patch_forward(self.get_current_func(), current_insert_offset);
                });

                // patch branch out of loop
                if let Some(jump) = branch_inst {
                    let current_insert_offset = self.registers.insert_offset;
                    jump.patch_forward(self.get_current_func(), current_insert_offset);
                };
            }
            Stmt::Continue { loc } => {
                let Loop {
                    start_inst_offset,
                    start_insert_offset,
                    breaks: _,
                } = self
                    .registers
                    .loops
                    .last()
                    .ok_or(ErrorCode::ContinueOutsideLoop(loc.clone()))?;
                let jump = FutureJump {
                    previous_insert_offset: *start_insert_offset,
                    condition_reg: None,
                    previous_inst_offset: *start_inst_offset,
                    loc: loc.clone(),
                };
                let current_insert_offset = self.registers.insert_offset;
                jump.patch_backward(self.get_current_func(), current_insert_offset)
            }
            Stmt::Break { loc } => {
                self.registers
                    .loops
                    .last()
                    .ok_or(ErrorCode::BreakOutsideLoop(loc.clone()))?;
                let insert_offset = self.registers.insert_offset;
                let inst_offset = self.get_current_func().insts.len();
                let jump = FutureJump {
                    previous_insert_offset: insert_offset,
                    condition_reg: None,
                    previous_inst_offset: inst_offset,
                    loc: loc.clone(),
                };
                self.registers.loops.last_mut().unwrap().breaks.push(jump);
                self.get_current_func().insts.push(VmInst::OpDummy(OpDummy));
            }
            Stmt::Return { loc, value } => {
                if self.registers.prev.is_none() {
                    return Err(ErrorCode::ReturnOutsideFunction(loc.clone()));
                }
                let return_reg = if let Some(expr) = value {
                    let (reg, tmp) = self.compile_expr(expr, false)?;
                    if tmp {
                        self.registers.free_intermediate(reg);
                    }
                    reg
                } else {
                    let (reg, _) = self.compile_constant(&Const::Unit, loc.clone())?;
                    reg
                };
                self.get_current_func().insts.push(VmInst::OpRet(OpRet {
                    return_reg,
                    loc: loc.clone(),
                }))
            }
            Stmt::For {
                loc: _,
                loop_variable: _,
                iterator: _,
                body: _,
            } => todo!(),
            Stmt::Def {
                loc,
                name,
                parameters,
                body,
            } => {
                let (func_id, parameters, capture, reg_size) =
                    self.compile_closure(parameters, either::Right(body), loc.clone())?;
                let reg_f_id = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpMakeClosure(OpMakeClosure {
                        loc: loc.clone(),
                        func_id,
                        parameters,
                        rd: reg_f_id,
                        capture,
                        reg_size,
                    }));

                // declare variable
                let id = if let Some((id, depth, loc)) = self.registers.lookup_variable(name) {
                    // variable is captured
                    // make a local copy and register capture info
                    if depth > 0 {
                        let local_id = self.registers.declare_variable(name, loc);
                        self.registers.capture.push(Capture {
                            rd: local_id,
                            rs: id,
                            depth,
                        });
                        local_id
                    } else {
                        id
                    }
                } else {
                    self.scopes.last_mut().unwrap().insert(name.clone());
                    self.registers.declare_variable(name, Some(loc.clone()))
                };
                self.registers.free_intermediate(reg_f_id);
                self.get_current_func().insts.push(VmInst::OpMove(OpMove {
                    loc: loc.clone(),
                    rs: reg_f_id,
                    rd: id,
                }));
            }
            Stmt::Error => unreachable!(),
        }
        Ok(return_value)
    }

    fn compile_expr(&mut self, expr: &Expr, discard: bool) -> Result<(usize, bool), ErrorCode> {
        match expr {
            Expr::Prefix { loc, op, rhs } => {
                let (rhs_id, rhs_tmp) = self.compile_expr(rhs, false)?;
                if rhs_tmp {
                    self.registers.free_intermediate(rhs_id)
                };
                Ok(self.compile_prefix(op, rhs_id, loc.clone()))
            }
            Expr::Infix {
                loc,
                op: OpInfix::Assign,
                lhs: _,
                rhs: _,
            } => Err(ErrorCode::InvalidAssignment(loc.clone())),
            Expr::Infix { loc, op, lhs, rhs } => {
                let (lhs_id, lhs_tmp) = self.compile_expr(lhs, false)?;
                let (rhs_id, rhs_tmp) = self.compile_expr(rhs, false)?;
                let ret = self.compile_infix(op, lhs_id, rhs_id, loc.clone());
                if lhs_tmp {
                    self.registers.free_intermediate(lhs_id);
                };
                if rhs_tmp {
                    self.registers.free_intermediate(rhs_id);
                };
                Ok(ret)
            }
            Expr::Id { loc, name } => match self.registers.lookup_variable(name) {
                Some((id, depth, loc)) => {
                    // variable is captured
                    // make a local copy and register capture info
                    if depth > 0 {
                        let local_id = self.registers.declare_captured_variable(name, loc);
                        self.registers.capture.push(Capture {
                            rd: local_id,
                            rs: id,
                            depth,
                        });
                        Ok((local_id, false))
                    } else {
                        Ok((id, false))
                    }
                }
                None => Err(ErrorCode::NameNotDefined(loc.clone(), name.clone())),
            },
            Expr::Parentheses { loc: _, content } => self.compile_expr(content, discard),
            Expr::Const { loc, value } => Ok(self.compile_constant(value, loc.clone()))?,
            Expr::Error => unreachable!(),
            Expr::Block { loc, body } => {
                self.enter_block();
                let mut ret = None;
                for (i, stmt) in body.iter().enumerate() {
                    let reg = self
                        .compile_stmt(stmt, i != body.len() - 1)
                        .map_err(|err| {
                            self.leave_block();
                            err
                        })?;
                    ret = reg;
                }
                self.leave_block();
                match (ret, discard) {
                    (_, true) => Ok((usize::MAX, true)),
                    (Some(ret), false) => Ok(ret),
                    (None, false) => {
                        let rd = self.compile_constant(&Const::Unit, loc.clone())?;
                        Ok(rd)
                    }
                }
            }
            Expr::If {
                loc,
                conditional,
                default,
            } => {
                assert!(!conditional.is_empty());
                let mut branch_op: Option<FutureJump> = None;
                let mut jump_to_ends = vec![];
                let ret = if discard {
                    None
                } else {
                    Some(self.registers.declare_intermediate())
                };
                for (condition, body) in conditional {
                    // patch branch
                    if let Some(jump) = branch_op {
                        let current_insert_offset = self.registers.insert_offset;
                        jump.patch_forward(self.get_current_func(), current_insert_offset);
                    }
                    // compile condition
                    let (condition_reg, tmp) = self.compile_expr(condition, false)?;
                    if tmp {
                        self.registers.free_intermediate(condition_reg);
                    }
                    let insert_offset = self.registers.insert_offset;
                    let inst_offset = self.get_current_func().insts.len();
                    branch_op = Some(FutureJump {
                        previous_insert_offset: insert_offset,
                        condition_reg: Some((condition_reg, true)),
                        previous_inst_offset: inst_offset,
                        loc: condition.get_loc(),
                    });
                    self.get_current_func().insts.push(VmInst::OpDummy(OpDummy));

                    // compile body
                    self.enter_block();
                    // return unit for empty body
                    if body.is_empty() && !discard {
                        // load a unit value
                        let (reg, _) = self.compile_constant(&Const::Unit, loc.clone())?;
                        // move unit value
                        self.get_current_func().insts.push(VmInst::OpMove(OpMove {
                            loc: loc.clone(),
                            rs: reg,
                            rd: ret.unwrap(),
                        }));
                    }
                    for (i, stmt) in body.iter().enumerate() {
                        if !discard && i == body.len() - 1 {
                            let ret_this = self.compile_stmt(stmt, false)?;
                            // move return value to return reg
                            if let Some((reg, tmp)) = ret_this {
                                if tmp {
                                    self.registers.free_intermediate(reg)
                                };
                                self.get_current_func().insts.push(VmInst::OpMove(OpMove {
                                    loc: stmt.get_loc(),
                                    rs: reg,
                                    rd: ret.unwrap(),
                                }))
                            } else {
                                // load a unit value
                                let (reg, _) =
                                    self.compile_constant(&Const::Unit, stmt.get_loc())?;
                                // move unit value
                                self.get_current_func().insts.push(VmInst::OpMove(OpMove {
                                    loc: stmt.get_loc(),
                                    rs: reg,
                                    rd: ret.unwrap(),
                                }))
                            }
                        } else {
                            self.compile_stmt(stmt, true)?;
                        }
                    }
                    self.leave_block();
                    let insert_offset = self.registers.insert_offset;
                    let inst_offset = self.get_current_func().insts.len();
                    jump_to_ends.push(FutureJump {
                        previous_insert_offset: insert_offset,
                        condition_reg: None,
                        previous_inst_offset: inst_offset,
                        loc: loc.clone(),
                    });
                    self.get_current_func().insts.push(VmInst::OpDummy(OpDummy));
                }
                // patch branch
                if let Some(jump) = branch_op {
                    let current_insert_offset = self.registers.insert_offset;
                    jump.patch_forward(self.get_current_func(), current_insert_offset);
                }

                // compile default else body
                if let Some(body) = default {
                    self.enter_block();
                    if body.is_empty() && !discard {
                        // load a unit value
                        let (reg, _) = self.compile_constant(&Const::Unit, loc.clone())?;
                        // move unit value
                        self.get_current_func().insts.push(VmInst::OpMove(OpMove {
                            loc: loc.clone(),
                            rs: reg,
                            rd: ret.unwrap(),
                        }))
                    }
                    for (i, stmt) in body.iter().enumerate() {
                        if !discard && i == body.len() - 1 {
                            let ret_this = self.compile_stmt(stmt, false)?;
                            // move return value to return reg
                            if let Some((reg, tmp)) = ret_this {
                                if tmp {
                                    self.registers.free_intermediate(reg)
                                };
                                self.get_current_func().insts.push(VmInst::OpMove(OpMove {
                                    loc: stmt.get_loc(),
                                    rs: reg,
                                    rd: ret.unwrap(),
                                }))
                            } else {
                                // load a unit value
                                let (reg, _) =
                                    self.compile_constant(&Const::Unit, stmt.get_loc())?;
                                // move unit value
                                self.get_current_func().insts.push(VmInst::OpMove(OpMove {
                                    loc: stmt.get_loc(),
                                    rs: reg,
                                    rd: ret.unwrap(),
                                }))
                            }
                        } else {
                            self.compile_stmt(stmt, true)?;
                        }
                    }
                    self.leave_block();
                }
                // patch all jumps
                jump_to_ends.into_iter().for_each(|jump| {
                    let current_insert_offset = self.registers.insert_offset;
                    jump.patch_forward(self.get_current_func(), current_insert_offset);
                });

                Ok(ret.map(|reg| (reg, true)).unwrap_or((usize::MAX, false)))
            }
            Expr::Call {
                loc,
                lhs,
                parameters,
            } => {
                let mut para_regs = vec![];
                for para in parameters {
                    let result = self.compile_expr(para, false)?;
                    para_regs.push(result);
                }
                let (lhs, lhs_tmp) = self.compile_expr(lhs, false)?;
                if lhs_tmp {
                    self.registers.free_intermediate(lhs);
                }
                let rd = if discard {
                    None
                } else {
                    Some(self.registers.declare_intermediate())
                };
                self.get_current_func()
                    .insts
                    .push(VmInst::OpCallClosure(OpCallClosure {
                        reg_id: lhs,
                        parameters: para_regs.iter().map(|(reg, _)| *reg).collect(),
                        write_back: rd,
                        loc: loc.clone(),
                    }));
                para_regs.into_iter().for_each(|(reg, tmp)| {
                    if tmp {
                        self.registers.free_intermediate(reg)
                    }
                });
                Ok((rd.unwrap_or(usize::MAX), true))
            }
            Expr::Index {
                loc: _,
                lhs: _,
                rhs: _,
            } => todo!(),
            Expr::Fn {
                loc,
                parameters,
                body,
            } => {
                let (func_id, parameters, capture, reg_size) =
                    self.compile_closure(parameters, either::Left(body), loc.clone())?;
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpMakeClosure(OpMakeClosure {
                        loc: loc.clone(),
                        func_id,
                        parameters,
                        rd,
                        capture,
                        reg_size,
                    }));
                Ok((rd, true))
            }
            Expr::Module { loc: _, path: _ } => todo!(),
        }
    }

    fn compile_assignment(&mut self, lhs: &Expr, rhs: &Expr, loc: Loc) -> Result<(), ErrorCode> {
        if let Expr::Id { loc: id_loc, name } = lhs {
            // declare variable
            let id = if let Some((id, depth, loc)) = self.registers.lookup_variable(name) {
                // variable is captured
                // make a local copy and register capture info
                if depth > 0 {
                    let local_id = self.registers.declare_variable(name, loc);
                    self.registers.capture.push(Capture {
                        rd: local_id,
                        rs: id,
                        depth,
                    });
                    local_id
                } else {
                    id
                }
            } else {
                self.scopes.last_mut().unwrap().insert(name.clone());
                self.registers.declare_variable(name, Some(id_loc.clone()))
            };
            let (rhs, tmp) = self.compile_expr(rhs, false)?;
            if tmp {
                self.registers.free_intermediate(rhs);
            }
            self.get_current_func().insts.push(VmInst::OpMove(OpMove {
                loc,
                rs: rhs,
                rd: id,
            }));
            Ok(())
        } else {
            Err(ErrorCode::CannotAssign(lhs.get_loc()))
        }
    }

    fn compile_constant(&mut self, constant: &Const, loc: Loc) -> Result<(usize, bool), ErrorCode> {
        let constant = match constant {
            Const::Unit => self
                .registers
                .get_or_alloc_constant(ConstantValue::Unit)
                .map_err(|reg| (reg, Reg::Unit)),
            Const::Int(i) => self
                .registers
                .get_or_alloc_constant(ConstantValue::Int(*i))
                .map_err(|reg| (reg, Reg::Int(*i))),
            Const::Float(f) => self
                .registers
                .get_or_alloc_constant(ConstantValue::Float((*f).to_bits()))
                .map_err(|reg| (reg, Reg::Float(*f))),
            Const::Str(s) => self
                .registers
                .get_or_alloc_constant(ConstantValue::Str(s.clone()))
                .map_err(|reg| {
                    let sid = self.gc.string_pool().alloc(s.clone());
                    (reg, Reg::Str(sid))
                }),
            Const::Bool(b) => self
                .registers
                .get_or_alloc_constant(ConstantValue::Bool(*b))
                .map_err(|reg| (reg, Reg::Bool(*b))),
            Const::List(_) => todo!(),
            Const::Table(pairs) => {
                let reg_id = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpMakeTable(OpMakeTable { rd: reg_id }));
                for (name, expr, loc) in pairs.iter() {
                    let (value, tmp) = self.compile_expr(expr, false)?;
                    if tmp {
                        self.registers.free_intermediate(value);
                    }
                    self.get_current_func()
                        .insts
                        .push(VmInst::OpSetAttr(OpSetAttr {
                            loc: loc.clone(),
                            rs: value,
                            rd: reg_id,
                            attrs: vec![name.clone()],
                        }));
                }
                return Ok((reg_id, true));
            }
        };
        Ok(match constant {
            Ok(reg_id) => (reg_id, false),
            Err((reg_id, reg)) => {
                let insert_loc = self.registers.current_start_loc;
                self.registers.insert_offset += 1;
                self.get_current_func().insts.insert(
                    insert_loc,
                    VmInst::OpLoadConstant(OpLoadConstant {
                        constant: reg,
                        rd: reg_id,
                        loc,
                    }),
                );
                (reg_id, false)
            }
        })
    }

    fn compile_infix(&mut self, op: &OpInfix, lhs: usize, rhs: usize, loc: Loc) -> (usize, bool) {
        match op {
            OpInfix::Assign => unreachable!(),
            OpInfix::Range => todo!(),
            OpInfix::Or => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpOr(OpOr { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::And => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpAnd(OpAnd { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Eq => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpEq(OpEq { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Ne => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpNe(OpNe { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Ge => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpGe(OpGe { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Gt => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpGt(OpGt { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Lt => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpLt(OpLt { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Le => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpLe(OpLe { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Plus => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpAdd(OpAdd { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Minus => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpSub(OpSub { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Mul => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpMul(OpMul { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Div => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpDiv(OpDiv { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::DivFloor => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpIDiv(OpIDiv { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Rem => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpRem(OpRem { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Exp => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpPow(OpPow { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Comma => todo!(),
            OpInfix::Member => todo!(),
        }
    }

    fn compile_prefix(&mut self, op: &OpPrefix, rhs: usize, loc: Loc) -> (usize, bool) {
        match op {
            OpPrefix::Not => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpNot(OpNot { loc, lhs: rhs, rd }));
                (rd, true)
            }
            OpPrefix::Neg => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpNeg(OpNeg { loc, lhs: rhs, rd }));
                (rd, true)
            }
        }
    }

    /// Return (func_id, parameters len, captured_regs, reg_size)
    fn compile_closure(
        &mut self,
        parameters: &[(String, Loc)],
        body: Either<&Expr, &[Stmt]>,
        loc: Loc,
    ) -> std::result::Result<(usize, usize, Vec<Capture>, usize), ErrorCode> {
        let func_id = self.byte_code.len();
        self.byte_code.push(Func {
            name: format!("Anonymous@{func_id}"),
            parameters: parameters.len(),
            insts: vec![],
        });
        self.registers.enter_function(func_id);
        for (para, loc) in parameters.iter() {
            if let Some((_, _, loc_prev)) = self.registers.lookup_variable(para) {
                return Err(ErrorCode::ParameterShadowing {
                    previous: loc_prev,
                    parameter: loc.clone(),
                    name: para.clone(),
                });
            } else {
                self.registers.declare_variable(para, Some(loc.clone()));
            }
        }
        let result = match body {
            Either::Left(body) => self.compile_expr(body, false).map_err(|err| {
                self.registers.leave_function();
                err
            })?,
            Either::Right(body) => {
                let mut ret = None;
                if body.is_empty() {
                    // load a unit value
                    let (reg, _) = self.compile_constant(&Const::Unit, loc.clone())?;
                    ret = Some((reg, false));
                }

                for (i, stmt) in body.iter().enumerate() {
                    if i == body.len() - 1 {
                        let ret_this = self.compile_stmt(stmt, false)?;
                        // move return value to return reg
                        if let Some(ret_this) = ret_this {
                            ret = Some(ret_this);
                        } else {
                            // load a unit value
                            let (reg, _) = self.compile_constant(&Const::Unit, stmt.get_loc())?;
                            ret = Some((reg, false));
                        }
                    } else {
                        self.compile_stmt(stmt, true)?;
                    }
                }
                ret.unwrap()
            }
        };
        // return expression value
        self.get_current_func().insts.push(VmInst::OpRet(OpRet {
            return_reg: result.0,
            loc,
        }));
        let reg_size = self.registers.assigned;
        let captured_regs = self.registers.leave_function();
        Ok((func_id, parameters.len(), captured_regs, reg_size))
    }

    fn get_current_func(&mut self) -> &mut Func {
        let id = self.registers.func_id;
        &mut self.byte_code[id]
    }

    fn enter_block(&mut self) {
        self.scopes.push(AHashSet::new());
    }

    fn leave_block(&mut self) {
        let scope = self.scopes.pop().unwrap();
        for name in scope.into_iter() {
            self.registers.variables.remove(&name);
        }
    }
}

#[cfg(test)]
mod tests;
