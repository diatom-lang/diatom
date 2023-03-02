use crate::frontend::parser::ast::ImportItem;
use crate::gc::{Gc, GcObject, PrimitiveMeta, Reg, Table};
use std::ffi::OsStr;
use std::fmt::Write;
use std::io;
use std::marker::PhantomData;
use std::path::PathBuf;
use std::sync::Arc;

use ahash::{AHashMap, AHashSet};
use codespan_reporting::diagnostic::Label;

mod error;
mod register_table;
mod scanner;
pub mod std_core;

pub mod ffi;
use crate::file_manager::FileManager;
use crate::vm::op::{
    OpGe, OpGetTable, OpGetTuple, OpImport, OpIndex, OpIs, OpLe, OpLt, OpMakeList, OpMakeTable,
    OpMakeTuple, OpNe, OpSaveModule, OpSetIndex, OpSetMeta, OpSetTable, OpSetTuple,
};
use crate::{
    ffi::{DiatomValue, State},
    file_manager::{Diagnostic, Loc},
    frontend::{
        parser::ast::{Const, Expr, OpInfix, OpPrefix, Stmt},
        Parser,
    },
    vm::{
        error::VmError,
        op::{
            OpAdd, OpAllocReg, OpBranchFalse, OpBranchTrue, OpCall, OpDiv, OpDummy, OpEq, OpGt,
            OpIDiv, OpJump, OpMakeClosure, OpMove, OpMul, OpNeg, OpNot, OpPow, OpRem, OpRet, OpSub,
            OpYield,
        },
        Instruction, Vm, VmInst,
    },
    IoWrite,
};

use error::ErrorCode;
pub use register_table::Capture;
use register_table::{ConstantValue, Loop, RegisterTable};

use self::scanner::{CaptureScanner, ConstScanner};
use self::std_core::{Extension, ExtensionKind, StdCore};

#[derive(Clone)]
pub struct FutureJump {
    condition_reg: Option<(usize, bool)>,
    inst_offset: usize,
    loc: Loc,
}

impl FutureJump {
    /// Insert jump from current loc to previous
    pub fn patch_backward(self, func: &mut Func) {
        let jump_offset = self.inst_offset as i64 - func.insts.len() as i64;
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
    pub fn patch_forward(self, func: &mut Func) {
        let jump_offset = func.insts.len() as i64 - self.inst_offset as i64;
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
        func.insts[self.inst_offset] = op;
    }
}

pub struct Func {
    pub id: usize,
    pub parameters: usize,
    pub insts: Vec<VmInst>,
}

pub struct Interpreter<Buffer: IoWrite, LibCore: StdCore> {
    registers: RegisterTable,
    scopes: Vec<AHashSet<String>>,
    byte_code: Vec<Func>,
    vm: Vm,
    gc: Gc<Buffer>,
    out: Buffer,
    file_manager: FileManager,
    color: bool,
    repl: bool,
    search_path: Vec<PathBuf>,
    marker: PhantomData<LibCore>,
}

impl<Buffer: IoWrite, LibCore: StdCore> Interpreter<Buffer, LibCore> {
    /// Create a new interpreter instance
    pub fn new(buffer: Buffer) -> Self {
        Self::init(buffer, false)
    }

    /// Enable or disable REPL mode (print last value to output buffer)
    pub fn repl(&mut self, repl: bool) -> &mut Self {
        self.repl = repl;
        self
    }

    /// Add module search path
    pub fn with_search_path(&mut self, path: PathBuf) -> Result<(), io::Error> {
        let path = path.canonicalize()?;
        self.search_path.push(path);
        Ok(())
    }

    fn init(buffer: Buffer, color: bool) -> Self {
        let main = Func {
            id: 0,
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
            file_manager: FileManager::new(),
            color,
            repl: false,
            search_path: vec![],
            marker: PhantomData::default(),
        };
        // Initialize int and float meta table
        [
            ("Int", PrimitiveMeta::Int),
            ("Float", PrimitiveMeta::Float),
            ("List", PrimitiveMeta::List),
            ("String", PrimitiveMeta::Str),
        ]
        .into_iter()
        .for_each(|(name, t)| {
            let int = interpreter.registers.declare_variable(name, None);
            interpreter.gc.alloc_reg_file(int + 1);
            interpreter.gc.set_main_reg_size(int + 1);
            interpreter
                .gc
                .write_reg(int, Reg::Ref(interpreter.gc.get_meta(t)));
        });

        // Load prelude extension
        interpreter
            .load_ext(LibCore::prelude_extension())
            .map_err(|_| ())
            .unwrap();

        // Execute prelude files
        LibCore::prelude_files().iter().for_each(|(name, code)| {
            if let Err(err) = interpreter.exec(code, name, true) {
                print!("{err}");
                panic!("Standard library failed to load: `{name}`");
            }
        });

        interpreter
    }

    /// Enable ansi colored error message
    pub fn with_color(buffer: Buffer) -> Self {
        Self::init(buffer, true)
    }

    fn traverse_ext(
        &mut self,
        path_stack: &mut Vec<String>,
        Extension { name, kind }: Extension<Buffer>,
    ) {
        // Invalid names
        if name.contains('/') {
            return;
        }
        path_stack.push(name);
        match kind {
            ExtensionKind::ForeignFunctions(functions) => {
                let mut table = Table {
                    attributes: Default::default(),
                    meta_table: None,
                };
                functions.into_iter().for_each(|(name, f)| {
                    let table_key = self.gc.get_or_insert_table_key(name);
                    let f = self.gc.alloc_obj(GcObject::NativeFunction(f));
                    table.attributes.insert(table_key, Reg::Ref(f));
                });
                let table = self.gc.alloc_obj(GcObject::Table(table));
                let mut path = PathBuf::new();
                path_stack.iter().for_each(|name| path.push(name));
                path.set_extension("dm");
                let fid = self.file_manager.add_file(path, "".to_string());
                self.gc.new_module(fid);
                self.gc.set_module_return(fid, table);
            }
            ExtensionKind::File(code) => {
                let mut path = PathBuf::new();
                path_stack.iter().for_each(|name| path.push(name));
                path.set_extension("dm");
                let fid = self.file_manager.add_file(path, code);
                self.gc.new_module(fid);
            }
            ExtensionKind::SubExtensions(exts) => {
                exts.into_iter()
                    .for_each(|ext| self.traverse_ext(path_stack, ext));
            }
        }
        path_stack.pop();
    }

    pub fn load_ext(&mut self, extension: Extension<Buffer>) -> Result<(), Extension<Buffer>> {
        if self.file_manager.is_ext_name(&extension.name) || extension.name.contains('/') {
            return Err(extension);
        } else {
            self.file_manager.new_ext(extension.name.clone());
        }
        // Prevent modules being collected while registering
        self.gc.pause();
        let mut path_stack = vec![];
        self.traverse_ext(&mut path_stack, extension);
        self.gc.resume();
        Ok(())
    }

    /// Directly declare external function as variable
    pub fn impl_extern_function<F>(&mut self, name: impl Into<String>, f: F)
    where
        F: Fn(&mut State<Buffer>, &[DiatomValue], &mut Buffer) -> Result<DiatomValue, String>
            + 'static
            + Send
            + Sync,
    {
        let f = GcObject::NativeFunction(Arc::new(f));
        let gc_id = self.gc.alloc_obj(f);
        let reg = Reg::Ref(gc_id);
        let reg_id = self.registers.declare_variable(name.into(), None);
        self.gc.alloc_reg_file(reg_id + 1);
        self.gc.set_main_reg_size(reg_id + 1);
        self.gc.write_reg(reg_id, reg);
    }

    /// Check if input is completeness
    ///
    /// Incomplete input usually contains unclosed parentheses, quotes or open expression.
    pub fn verify_input_completeness(&self, code: impl AsRef<str>) -> bool {
        let mut file_manager = FileManager::new();
        let mut parser = Parser::new(&mut file_manager, &self.search_path);
        let _ = parser.parse_file(OsStr::new(""), code.as_ref());
        !file_manager.input_can_continue()
    }

    /// Show decompiled byte code for given source code.
    ///
    /// If compilation failed, `Err` will be returned.
    pub fn decompile(
        &mut self,
        code: impl AsRef<str>,
        source: impl AsRef<OsStr>,
        is_phony: bool,
    ) -> Result<String, String> {
        self.compile(code, source.as_ref(), is_phony)?;
        let mut decompiled = String::new();
        for Func {
            id,
            parameters,
            insts,
        } in self.byte_code.iter()
        {
            writeln!(decompiled, "Function: Func@{id}\nParameters: {parameters}").unwrap();
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
        is_phony: bool,
    ) -> Result<(), String> {
        self.file_manager.clear_diagnoses();
        let mut parser = Parser::new(&mut self.file_manager, &self.search_path);
        let fid = if is_phony {
            parser.parse_file_phony(source, code.as_ref())
        } else {
            parser.parse_file(source, code.as_ref())
        };
        if self.file_manager.error_count() > 0 {
            return Err(self.file_manager.render(self.color));
        }

        let registers_prev = self.registers.clone();
        // clear all executed code
        self.byte_code[0].insts.clear();
        self.vm.reset_ip();

        let ast = self.file_manager.get_ast(fid);
        let return_value = self.compile_ast(&ast).map_err(|_| {
            // restore variable table if compile failed
            self.registers = registers_prev;
            self.file_manager.render(self.color)
        })?;

        // return after main
        self.byte_code[0].insts.push(VmInst::OpYield(OpYield {
            show_id: return_value,
        }));

        // Alloc registers
        self.byte_code[0].insts.insert(
            0,
            VmInst::OpAllocReg(OpAllocReg {
                n_reg: self.registers.assigned,
            }),
        );

        self.gc.set_main_reg_size(self.registers.assigned);

        Ok(())
    }

    /// Run a piece of diatom source code
    ///
    /// # Parameters
    /// * `code` - Source code
    /// * `source` - name of source code file or where it is from
    /// * `is_phony` - Whether source is a real path or a place holder
    ///
    /// # Return
    /// * Return the output of the program
    /// * If compilation failed or error occurs durning execution, an `Err(String)` that
    /// illustrates the error is returned.
    pub fn exec(
        &mut self,
        code: impl AsRef<str>,
        source: impl AsRef<OsStr>,
        is_phony: bool,
    ) -> Result<(), String> {
        self.compile(code, source.as_ref(), is_phony)?;
        match self.vm.exec(&self.byte_code, &mut self.gc, &mut self.out) {
            (VmError::Yield(Some(reg_id)), _) if self.repl => {
                let reg = self.gc.read_reg(reg_id);
                match reg {
                    Reg::Unit => Ok(()),
                    _ => {
                        let content = self.gc.print(reg);
                        writeln!(self.out, "{content}").map_err(|err| {
                            let error_code = VmError::IoError {
                                loc: None,
                                error: err,
                            };
                            self.file_manager.add_diagnostic(error_code.into(), false);
                            self.file_manager.render(self.color)
                        })
                    }
                }
            }
            (VmError::Yield(_), _) => Ok(()),
            (error, trace) => {
                trace.into_iter().rev().for_each(|loc| {
                    self.file_manager.add_diagnostic(
                        Diagnostic::error()
                            .with_message("Trace back")
                            .with_labels(vec![Label::primary(loc.fid, loc)]),
                        false,
                    )
                });
                self.file_manager.add_diagnostic(error.into(), false);
                Err(self.file_manager.render(self.color))
            }
        }
    }

    /// if compile succeeded, return last expression's reg id
    fn compile_ast(&mut self, ast: &[Stmt]) -> Result<Option<usize>, ()> {
        let mut return_value = None;
        let mut has_error = false;

        // scan all constant values
        let func_id = self.registers.func_id;
        let mut const_scanner = ConstScanner {
            register_table: &mut self.registers,
            gc: &mut self.gc,
            insts: &mut self.byte_code[func_id].insts,
        };
        ast.iter().for_each(|stmt| const_scanner.scan_stmt(stmt));

        for (i, stmt) in ast.iter().enumerate() {
            match self.compile_stmt(stmt, i != ast.len() - 1, None) {
                Ok(ret) => return_value = ret,
                Err(code) => {
                    has_error = true;
                    self.file_manager
                        .add_diagnostic(Diagnostic::from(code), false);
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
        target: Option<usize>,
    ) -> Result<Option<(usize, bool)>, ErrorCode> {
        let mut return_value = None;
        match stmt {
            Stmt::Expr {
                loc: _,
                expr:
                    Expr::Infix {
                        op: OpInfix::Assign,
                        lhs,
                        rhs,
                        ..
                    },
            } => self.compile_assignment(lhs, rhs)?,
            Stmt::Expr { loc: _, expr } => {
                let (reg_id, tmp) = self.compile_expr(expr, discard, target)?;
                return_value = Some((reg_id, tmp));
            }
            Stmt::Loop {
                loc,
                condition,
                body,
            } => {
                let jump_to_start = FutureJump {
                    condition_reg: None,
                    inst_offset: self.get_current_insts().len(),
                    loc: loc.clone(),
                };
                let branch_inst = if let Some(condition) = condition {
                    let (condition_reg, tmp) = self.compile_expr(condition, false, None)?;
                    if tmp {
                        self.registers.free_intermediate(condition_reg);
                    }
                    self.get_current_insts().push(VmInst::OpDummy(OpDummy));
                    Some(FutureJump {
                        condition_reg: Some((condition_reg, false)),
                        inst_offset: self.get_current_insts().len() - 1,
                        loc: condition.get_loc(),
                    })
                } else {
                    None
                };
                self.enter_block();
                let current_inst = self.get_current_insts().len();
                self.registers.loops.push(Loop {
                    start_inst_offset: current_inst,
                    breaks: vec![],
                });
                for stmt in body.iter() {
                    self.compile_stmt(stmt, true, None).map_err(|err| {
                        self.leave_block();
                        err
                    })?;
                }
                let breaks = self.registers.loops.pop().unwrap().breaks;
                self.leave_block();

                // patch jump to loop start
                jump_to_start.patch_backward(self.get_current_func());

                // patch breaks
                breaks
                    .into_iter()
                    .for_each(|jump| jump.patch_forward(self.get_current_func()));

                // patch branch out of loop
                if let Some(jump) = branch_inst {
                    jump.patch_forward(self.get_current_func())
                }
            }
            Stmt::Continue { loc } => {
                let Loop {
                    start_inst_offset,
                    breaks: _,
                } = self
                    .registers
                    .loops
                    .last()
                    .ok_or(ErrorCode::ContinueOutsideLoop(loc.clone()))?;
                let jump = FutureJump {
                    condition_reg: None,
                    inst_offset: *start_inst_offset,
                    loc: loc.clone(),
                };
                jump.patch_backward(self.get_current_func())
            }
            Stmt::Break { loc } => {
                self.registers
                    .loops
                    .last()
                    .ok_or(ErrorCode::BreakOutsideLoop(loc.clone()))?;
                let jump = FutureJump {
                    condition_reg: None,
                    inst_offset: self.get_current_insts().len(),
                    loc: loc.clone(),
                };
                self.registers.loops.last_mut().unwrap().breaks.push(jump);
                self.get_current_insts().push(VmInst::OpDummy(OpDummy));
            }
            Stmt::Return { loc, value } => {
                if self.registers.prev.is_none() {
                    return Err(ErrorCode::ReturnOutsideFunction(loc.clone()));
                }
                let return_reg = if let Some(expr) = value {
                    let (reg, tmp) = self.compile_expr(expr, false, None)?;
                    if tmp {
                        self.registers.free_intermediate(reg);
                    }
                    reg
                } else {
                    let (reg, _) = self.compile_constant(&Const::Unit, None)?;
                    reg
                };
                self.get_current_insts()
                    .push(VmInst::OpRet(OpRet { return_reg }))
            }
            Stmt::For {
                loc,
                loop_variable,
                iterator,
                body,
            } => {
                let iter = self.registers.gen_sym();
                // iter = iterator.__iter()
                let loop_init_expr = Expr::Infix {
                    loc: iterator.get_loc(),
                    op: OpInfix::Assign,
                    lhs: Box::new(Expr::Id {
                        loc: iterator.get_loc(),
                        name: iter.clone(),
                    }),
                    rhs: Box::new(Expr::Call {
                        loc: iterator.get_loc(),
                        lhs: Box::new(Expr::Infix {
                            loc: iterator.get_loc(),
                            op: OpInfix::Member,
                            lhs: iterator.clone(),
                            rhs: Box::new(Expr::Id {
                                loc: iterator.get_loc(),
                                name: "__iter".to_string(),
                            }),
                        }),
                        parameters: vec![],
                    }),
                };
                let loop_init_stmt = Stmt::Expr {
                    loc: iterator.get_loc(),
                    expr: loop_init_expr,
                };
                self.compile_stmt(&loop_init_stmt, true, None)?;

                let mut loop_body = vec![];
                let loop_sym = self.registers.gen_sym();
                // loop body
                // loop_sym = iter.__next()
                loop_body.push(Stmt::Expr {
                    loc: loc.clone(),
                    expr: Expr::Infix {
                        loc: loc.clone(),
                        op: OpInfix::Assign,
                        lhs: Box::new(Expr::Id {
                            loc: iterator.get_loc(),
                            name: loop_sym.clone(),
                        }),
                        rhs: Box::new(Expr::Call {
                            loc: loop_variable.get_loc(),
                            lhs: Box::new(Expr::Infix {
                                loc: iterator.get_loc(),
                                op: OpInfix::Member,
                                lhs: Box::new(Expr::Id {
                                    loc: loop_variable.get_loc(),
                                    name: iter,
                                }),
                                rhs: Box::new(Expr::Id {
                                    loc: loop_variable.get_loc(),
                                    name: "__next".to_string(),
                                }),
                            }),
                            parameters: vec![],
                        }),
                    },
                });
                //if loop_sym is Option::None then
                //    break
                //else
                //    x = loop_sym.value
                //    Body
                //end
                let if_cond = Expr::Infix {
                    loc: loop_variable.get_loc(),
                    op: OpInfix::Is,
                    lhs: Box::new(Expr::Id {
                        loc: loop_variable.get_loc(),
                        name: loop_sym.clone(),
                    }),
                    rhs: Box::new(Expr::Infix {
                        loc: loop_variable.get_loc(),
                        op: OpInfix::DoubleColon,
                        lhs: Box::new(Expr::Id {
                            loc: loop_variable.get_loc(),
                            name: "Option".to_string(),
                        }),
                        rhs: Box::new(Expr::Id {
                            loc: loop_variable.get_loc(),
                            name: "None".to_string(),
                        }),
                    }),
                };

                let mut default = vec![Stmt::Expr {
                    loc: loop_variable.get_loc(),
                    expr: Expr::Infix {
                        loc: loop_variable.get_loc(),
                        op: OpInfix::Assign,
                        lhs: loop_variable.clone(),
                        rhs: Box::new(Expr::Infix {
                            loc: loop_variable.get_loc(),
                            op: OpInfix::Member,
                            lhs: Box::new(Expr::Id {
                                loc: loop_variable.get_loc(),
                                name: loop_sym,
                            }),
                            rhs: Box::new(Expr::Id {
                                loc: loop_variable.get_loc(),
                                name: "value".to_string(),
                            }),
                        }),
                    },
                }];
                default.extend(body.clone());

                loop_body.push(Stmt::Expr {
                    loc: loop_variable.get_loc(),
                    expr: Expr::If {
                        loc: loop_variable.get_loc(),
                        conditional: vec![(if_cond, vec![Stmt::Break { loc: loc.clone() }])],
                        default: Some(default),
                    },
                });
                let stmt = Stmt::Loop {
                    loc: loc.clone(),
                    condition: None,
                    body: loop_body,
                };
                self.compile_stmt(&stmt, discard, target)?;
            }
            Stmt::Def {
                loc,
                variable,
                parameters,
                body,
            } => {
                let expr = Expr::Infix {
                    loc: loc.clone(),
                    op: OpInfix::Assign,
                    lhs: variable.clone(),
                    rhs: Box::new(Expr::Fn {
                        loc: loc.clone(),
                        parameters: parameters.clone(),
                        body: Box::new(Expr::Block {
                            loc: loc.clone(),
                            body: body.clone(),
                        }),
                    }),
                };
                self.compile_stmt(
                    &Stmt::Expr {
                        loc: loc.clone(),
                        expr,
                    },
                    discard,
                    target,
                )?;
            }
            Stmt::Import {
                loc,
                fid,
                items,
                direct_import_mod,
            } => {
                self.gc.new_module(*fid);
                let body = self.file_manager.get_ast(*fid);
                let body = Expr::Block {
                    loc: loc.clone(),
                    body: body.to_vec(),
                };
                // Make a new closure
                let func_id = self.byte_code.len();
                self.byte_code.push(Func {
                    id: func_id,
                    parameters: 0,
                    insts: vec![],
                });
                self.registers.enter_function(func_id);

                let func_id = self.registers.func_id;
                // scan all constant values
                let mut const_scanner = ConstScanner {
                    register_table: &mut self.registers,
                    gc: &mut self.gc,
                    insts: &mut self.byte_code[func_id].insts,
                };
                const_scanner.scan_expr(&body);

                // Capture prelude
                let mut capture_scanner = CaptureScanner {
                    register_table: &mut self.registers,
                    gc: &mut self.gc,
                    insts: &mut self.byte_code[func_id].insts,
                    overridden: AHashMap::new(),
                };
                LibCore::prelude_names()
                    .iter()
                    .for_each(|name| capture_scanner.scan_name(name));

                let result = match &body {
                    Expr::Infix {
                        op: OpInfix::Assign,
                        lhs,
                        rhs,
                        ..
                    } => {
                        self.compile_assignment(lhs, rhs).map_err(|err| {
                            self.registers.leave_function();
                            err
                        })?;
                        (0, false)
                    }
                    body => self.compile_expr(body, false, None).map_err(|err| {
                        self.registers.leave_function();
                        err
                    })?,
                };
                // return expression value
                self.get_current_insts().push(VmInst::OpRet(OpRet {
                    return_reg: result.0,
                }));
                let reg_size = self.registers.assigned;
                let captured_regs = self.registers.leave_function();

                let module_reg = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpMakeClosure(OpMakeClosure {
                        loc: loc.clone(),
                        func_id,
                        parameters: 0,
                        rd: module_reg,
                        capture: captured_regs,
                        reg_size,
                    }));

                // Call import closure
                let start = self.registers.prepare_for_call(0);
                self.get_current_insts().push(VmInst::OpImport(OpImport {
                    start,
                    fid: *fid,
                    module_reg,
                    loc: loc.clone(),
                }));
                // Save module
                self.get_current_insts()
                    .push(VmInst::OpSaveModule(OpSaveModule {
                        loc: loc.clone(),
                        fid: *fid,
                        module_reg,
                    }));
                // Load imported variables
                items.iter().for_each(|ImportItem { loc, alias, path }| {
                    let name = alias.as_ref().unwrap_or(path.last().unwrap());
                    let item_reg_id =
                        if let Some((id, depth, _)) = self.registers.lookup_variable(name) {
                            assert!(depth == 0);
                            id
                        } else {
                            self.scopes.last_mut().unwrap().insert(name.clone());
                            self.registers.declare_variable(name, Some(loc.clone()))
                        };
                    self.get_current_insts().push(VmInst::OpMove(OpMove {
                        rs: module_reg,
                        rd: item_reg_id,
                    }));
                    if !direct_import_mod {
                        path.iter().for_each(|attr| {
                            let key_id = self.gc.get_or_insert_table_key(attr);
                            self.get_current_insts()
                                .push(VmInst::OpGetTable(OpGetTable {
                                    loc: loc.clone(),
                                    rs: item_reg_id,
                                    rd: item_reg_id,
                                    attr: key_id,
                                }))
                        });
                    }
                });

                self.registers.free_intermediate(module_reg);
            }
            Stmt::Error => unreachable!(),
        }
        Ok(return_value)
    }

    fn compile_expr(
        &mut self,
        expr: &Expr,
        discard: bool,
        target: Option<usize>,
    ) -> Result<(usize, bool), ErrorCode> {
        match expr {
            Expr::Prefix { loc, op, rhs } => {
                let (rhs_id, rhs_tmp) = self.compile_expr(rhs, false, target)?;
                if rhs_tmp {
                    self.registers.free_intermediate(rhs_id)
                };
                Ok(self.compile_prefix(op, rhs_id, loc.clone(), target))
            }
            Expr::Infix {
                loc,
                op: OpInfix::Comma,
                lhs,
                rhs,
            } => {
                let mut items = vec![rhs.as_ref()];
                let mut left = lhs.as_ref();
                while let Expr::Infix {
                    loc: _,
                    op: OpInfix::Comma,
                    lhs,
                    rhs,
                } = left
                {
                    items.push(rhs);
                    left = lhs;
                }
                items.push(left);
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpMakeTuple(OpMakeTuple {
                        rd,
                        size: items.len(),
                    }));

                for (idx, item) in items.into_iter().rev().enumerate() {
                    let (rs, tmp) = self.compile_expr(item, false, None)?;
                    if tmp {
                        self.registers.free_intermediate(rs);
                    }
                    self.get_current_func()
                        .insts
                        .push(VmInst::OpSetTuple(OpSetTuple {
                            loc: loc.clone(),
                            rs,
                            rd,
                            idx,
                        }))
                }

                Ok((rd, target.is_none()))
            }
            Expr::Infix {
                loc,
                op: OpInfix::Range,
                lhs,
                rhs,
            } => {
                // Range(lhs, rhs)
                let expr = Expr::Call {
                    loc: loc.clone(),
                    lhs: Box::new(Expr::Id {
                        loc: loc.clone(),
                        name: "Range".to_string(),
                    }),
                    parameters: vec![lhs.as_ref().clone(), rhs.as_ref().clone()],
                };
                self.compile_expr(&expr, false, target)
            }
            Expr::OpenRange { loc, lhs } => {
                // Range(lhs, rhs)
                let rhs = Expr::Const {
                    loc: loc.clone(),
                    value: Const::Int(i64::MAX),
                };
                let expr = Expr::Call {
                    loc: loc.clone(),
                    lhs: Box::new(Expr::Id {
                        loc: loc.clone(),
                        name: "Range".to_string(),
                    }),
                    parameters: vec![lhs.as_ref().clone(), rhs],
                };
                self.compile_expr(&expr, false, target)
            }
            Expr::Infix {
                loc,
                op: OpInfix::Member | OpInfix::DoubleColon,
                lhs,
                rhs,
            } => {
                let (lhs, tmp) = self.compile_expr(lhs, false, None)?;
                if tmp {
                    self.registers.free_intermediate(lhs);
                }
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                let op = match rhs.as_ref() {
                    Expr::Id { loc: _, name } => VmInst::OpGetTable(OpGetTable {
                        loc: loc.clone(),
                        rs: lhs,
                        rd,
                        attr: self.gc.get_or_insert_table_key(name),
                    }),
                    Expr::Const {
                        loc: _,
                        value: Const::Int(i),
                    } => VmInst::OpGetTuple(OpGetTuple {
                        loc: loc.clone(),
                        rs: lhs,
                        rd,
                        idx: *i as usize,
                    }),
                    expr => {
                        return Err(ErrorCode::InvalidMember(expr.get_loc()));
                    }
                };

                self.get_current_insts().push(op);
                Ok((rd, target.is_none()))
            }
            Expr::Infix {
                loc,
                op: OpInfix::LArrow,
                lhs,
                rhs,
            } => {
                if matches!(
                    lhs.as_ref(),
                    Expr::Const {
                        value: Const::Table(_),
                        ..
                    }
                ) {
                    let (lhs_id, lhs_tmp) = self.compile_expr(lhs, false, target)?;
                    let (rhs_id, rhs_tmp) = self.compile_expr(rhs, false, None)?;
                    self.get_current_insts().push(VmInst::OpSetMeta(OpSetMeta {
                        rs: rhs_id,
                        rd: lhs_id,
                        loc: loc.clone(),
                    }));
                    if rhs_tmp {
                        self.registers.free_intermediate(rhs_id);
                    }
                    Ok((lhs_id, target.is_none() && lhs_tmp))
                } else {
                    Err(ErrorCode::MetaNotAllowed(loc.clone()))
                }
            }
            // prevent use assignment as expression
            Expr::Infix {
                loc,
                op: OpInfix::Assign,
                lhs: _,
                rhs: _,
            } => Err(ErrorCode::InvalidAssignment(loc.clone())),
            // short circuit and
            Expr::Infix {
                loc,
                op: OpInfix::And,
                lhs,
                rhs,
            } => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.compile_expr(lhs, false, Some(rd))?;
                let br_true_to_end = FutureJump {
                    condition_reg: Some((rd, true)),
                    inst_offset: self.get_current_insts().len(),
                    loc: loc.clone(),
                };
                self.compile_expr(rhs, false, Some(rd))?;
                br_true_to_end.patch_forward(self.get_current_func());
                if target.is_none() {
                    self.registers.free_intermediate(rd);
                }
                Ok((rd, target.is_none()))
            }
            // short circuit or
            Expr::Infix {
                loc,
                op: OpInfix::Or,
                lhs,
                rhs,
            } => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.compile_expr(lhs, false, Some(rd))?;
                let br_true_to_end = FutureJump {
                    condition_reg: Some((rd, false)),
                    inst_offset: self.get_current_insts().len(),
                    loc: loc.clone(),
                };
                self.compile_expr(rhs, false, Some(rd))?;
                br_true_to_end.patch_forward(self.get_current_func());
                if target.is_none() {
                    self.registers.free_intermediate(rd);
                }
                Ok((rd, target.is_none()))
            }
            Expr::Infix { loc, op, lhs, rhs } => {
                let (lhs_id, lhs_tmp) = self.compile_expr(lhs, false, None)?;
                let (rhs_id, rhs_tmp) = self.compile_expr(rhs, false, None)?;
                let ret = self.compile_infix(op, lhs_id, rhs_id, loc.clone(), target);
                if lhs_tmp {
                    self.registers.free_intermediate(lhs_id);
                };
                if rhs_tmp {
                    self.registers.free_intermediate(rhs_id);
                };
                Ok(ret)
            }
            Expr::Id { loc, name } => match self.registers.lookup_variable(name) {
                Some((id, depth, _)) => {
                    assert!(depth == 0);
                    Ok(if let Some(target) = target {
                        self.get_current_func()
                            .insts
                            .push(VmInst::OpMove(OpMove { rs: id, rd: target }));
                        (target, false)
                    } else {
                        (id, false)
                    })
                }
                None => Err(ErrorCode::NameNotDefined(loc.clone(), name.clone())),
            },
            Expr::Parentheses { loc: _, content } => self.compile_expr(content, discard, target),
            Expr::Const { value, .. } => Ok(self.compile_constant(value, target))?,
            Expr::Error => unreachable!(),
            Expr::Block { body, .. } => {
                self.enter_block();
                let mut ret = None;
                for (i, stmt) in body.iter().enumerate() {
                    let reg = self
                        .compile_stmt(stmt, i != body.len() - 1, target)
                        .map_err(|err| {
                            self.leave_block();
                            err
                        })?;
                    ret = reg;
                }
                self.leave_block();
                match (ret, discard) {
                    (_, true) => Ok((usize::MAX, false)),
                    (Some((ret, tmp)), false) if target.is_some() => {
                        if tmp {
                            self.registers.free_intermediate(ret);
                        }
                        let target = target.unwrap();
                        self.get_current_insts().push(VmInst::OpMove(OpMove {
                            rs: ret,
                            rd: target,
                        }));
                        Ok((target, false))
                    }
                    (Some(ret), false) => Ok(ret),
                    (None, false) => {
                        let rd = self.compile_constant(&Const::Unit, target)?;
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
                    Some(target.unwrap_or_else(|| self.registers.declare_intermediate()))
                };
                for (condition, body) in conditional {
                    // patch branch
                    if let Some(jump) = branch_op {
                        jump.patch_forward(self.get_current_func())
                    }
                    // compile condition
                    let (condition_reg, tmp) = self.compile_expr(condition, false, None)?;
                    if tmp {
                        self.registers.free_intermediate(condition_reg);
                    }
                    branch_op = Some(FutureJump {
                        condition_reg: Some((condition_reg, true)),
                        inst_offset: self.get_current_insts().len(),
                        loc: condition.get_loc(),
                    });
                    self.get_current_insts().push(VmInst::OpDummy(OpDummy));

                    // compile body
                    self.enter_block();
                    // return unit for empty body
                    if body.is_empty() && !discard {
                        // load a unit value
                        self.compile_constant(&Const::Unit, ret)?;
                    }
                    for (i, stmt) in body.iter().enumerate() {
                        if !discard && i == body.len() - 1 {
                            let ret_this = self.compile_stmt(stmt, false, ret)?;
                            // move return value to return reg
                            if let Some((reg, tmp)) = ret_this {
                                if tmp {
                                    self.registers.free_intermediate(reg)
                                };
                            } else {
                                // load a unit value
                                self.compile_constant(&Const::Unit, ret)?;
                            }
                        } else {
                            self.compile_stmt(stmt, true, None)?;
                        }
                    }
                    self.leave_block();

                    jump_to_ends.push(FutureJump {
                        condition_reg: None,
                        inst_offset: self.get_current_insts().len(),
                        loc: loc.clone(),
                    });
                    self.get_current_insts().push(VmInst::OpDummy(OpDummy));
                }
                // patch branch
                if let Some(jump) = branch_op {
                    jump.patch_forward(self.get_current_func())
                }

                // compile default else body
                if let Some(body) = default {
                    self.enter_block();
                    if body.is_empty() && !discard {
                        // load a unit value
                        self.compile_constant(&Const::Unit, ret)?;
                    }
                    for (i, stmt) in body.iter().enumerate() {
                        if !discard && i == body.len() - 1 {
                            let ret_this = self.compile_stmt(stmt, false, ret)?;
                            // move return value to return reg
                            if let Some((reg, tmp)) = ret_this {
                                if tmp {
                                    self.registers.free_intermediate(reg)
                                };
                            } else {
                                // load a unit value
                                self.compile_constant(&Const::Unit, ret)?;
                            }
                        } else {
                            self.compile_stmt(stmt, true, None)?;
                        }
                    }
                    self.leave_block();
                }
                // patch all jumps
                jump_to_ends
                    .into_iter()
                    .for_each(|jump| jump.patch_forward(self.get_current_func()));

                Ok(ret
                    .map(|reg| (reg, target.is_none()))
                    .unwrap_or((usize::MAX, false)))
            }
            Expr::Call {
                loc,
                lhs,
                parameters,
            } => {
                let is_member_call = if let Expr::Infix {
                    op: OpInfix::Member,
                    rhs,
                    ..
                } = lhs.as_ref()
                {
                    matches!(rhs.as_ref(), Expr::Id { .. })
                } else {
                    false
                };
                let (lhs_id, frame_start) = if is_member_call {
                    if let Expr::Infix {
                        op: OpInfix::Member,
                        loc,
                        lhs,
                        rhs,
                    } = lhs.as_ref()
                    {
                        // add lhs as first parameter
                        let lhs_id = self.registers.declare_intermediate();
                        let frame_start = self.registers.prepare_for_call(parameters.len() + 1);
                        self.compile_expr(lhs, false, Some(frame_start))?;
                        let op = match rhs.as_ref() {
                            Expr::Id { loc: _, name } => VmInst::OpGetTable(OpGetTable {
                                loc: loc.clone(),
                                rs: frame_start,
                                rd: lhs_id,
                                attr: self.gc.get_or_insert_table_key(name),
                            }),
                            _ => unreachable!(),
                        };
                        self.get_current_insts().push(op);
                        for (i, para) in parameters.iter().enumerate() {
                            self.compile_expr(para, false, Some(frame_start + i + 1))?;
                        }
                        self.registers.free_intermediate(lhs_id);
                        (lhs_id, frame_start)
                    } else {
                        unreachable!()
                    }
                } else {
                    let (lhs_id, lhs_tmp) = self.compile_expr(lhs, false, None)?;
                    let frame_start = self.registers.prepare_for_call(parameters.len());
                    for (i, para) in parameters.iter().enumerate() {
                        self.compile_expr(para, false, Some(frame_start + i))?;
                    }
                    if lhs_tmp {
                        self.registers.free_intermediate(lhs_id);
                    }
                    (lhs_id, frame_start)
                };
                let rd = if discard {
                    None
                } else {
                    Some(target.unwrap_or_else(|| self.registers.declare_intermediate()))
                };
                self.get_current_insts().push(VmInst::OpCall(OpCall {
                    reg_id: lhs_id,
                    parameters: parameters.len() + if is_member_call { 1 } else { 0 },
                    start: frame_start,
                    write_back: rd,
                    loc: loc.clone(),
                }));
                (frame_start..frame_start + parameters.len() + if is_member_call { 1 } else { 0 })
                    .into_iter()
                    .for_each(|reg| self.registers.free_intermediate(reg));
                Ok((rd.unwrap_or(usize::MAX), target.is_none()))
            }
            Expr::Index { loc, lhs, rhs } => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());

                self.compile_expr(lhs, false, Some(rd))?;
                let (rhs_id, rhs_tmp) = self.compile_expr(rhs, false, None)?;
                self.get_current_insts().push(VmInst::OpIndex(OpIndex {
                    loc: loc.clone(),
                    lhs: rd,
                    rhs: rhs_id,
                    rd,
                }));
                if rhs_tmp {
                    self.registers.free_intermediate(rhs_id);
                }
                Ok((rd, target.is_none()))
            }
            Expr::Fn {
                loc,
                parameters,
                body,
            } => {
                let (func_id, parameters, capture, reg_size) =
                    self.compile_closure(parameters, body)?;
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
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
                Ok((rd, target.is_none()))
            }
        }
    }

    fn compile_assignment(&mut self, lhs: &Expr, rhs: &Expr) -> Result<(), ErrorCode> {
        match lhs {
            Expr::Id { loc: id_loc, name } => {
                // declare variable
                let id = if let Some((id, depth, _)) = self.registers.lookup_variable(name) {
                    assert!(depth == 0);
                    id
                } else {
                    self.scopes.last_mut().unwrap().insert(name.clone());
                    self.registers.declare_variable(name, Some(id_loc.clone()))
                };
                let (rhs, tmp) = self.compile_expr(rhs, false, Some(id))?;
                if tmp {
                    self.registers.free_intermediate(rhs);
                }
                Ok(())
            }
            Expr::Index {
                lhs: rd,
                rhs: idx,
                loc,
            } => {
                let (rd_id, rd_tmp) = self.compile_expr(rd, false, None)?;
                let (idx_id, idx_tmp) = self.compile_expr(idx, false, None)?;
                let (rs_id, rs_tmp) = self.compile_expr(rhs, false, None)?;
                if rd_tmp {
                    self.registers.free_intermediate(rd_id);
                }
                if idx_tmp {
                    self.registers.free_intermediate(idx_id);
                }
                if rs_tmp {
                    self.registers.free_intermediate(rs_id);
                }
                self.get_current_insts()
                    .push(VmInst::OpSetIndex(OpSetIndex {
                        loc: loc.clone(),
                        rs: rs_id,
                        idx: idx_id,
                        rd: rd_id,
                    }));
                Ok(())
            }
            Expr::Infix {
                op: OpInfix::Member,
                lhs: rd,
                rhs: idx,
                loc,
            } => {
                let (rd_id, rd_tmp) = self.compile_expr(rd, false, None)?;
                let (rs_id, rs_tmp) = self.compile_expr(rhs, false, None)?;
                if rd_tmp {
                    self.registers.free_intermediate(rd_id);
                }
                if rs_tmp {
                    self.registers.free_intermediate(rs_id);
                }
                match idx.as_ref() {
                    Expr::Id { name, .. } => {
                        let name = self.gc.get_or_insert_table_key(name);
                        self.get_current_insts()
                            .push(VmInst::OpSetTable(OpSetTable {
                                loc: loc.clone(),
                                rs: rs_id,
                                rd: rd_id,
                                attr: name,
                            }));
                    }
                    Expr::Const {
                        value: Const::Int(i),
                        ..
                    } => {
                        assert!(*i >= 0);
                        self.get_current_insts()
                            .push(VmInst::OpSetTuple(OpSetTuple {
                                loc: loc.clone(),
                                rs: rs_id,
                                rd: rd_id,
                                idx: *i as usize,
                            }));
                    }
                    expr => return Err(ErrorCode::CannotAssign(expr.get_loc())),
                }
                Ok(())
            }
            _ => Err(ErrorCode::CannotAssign(lhs.get_loc())),
        }
    }

    fn compile_constant(
        &mut self,
        constant: &Const,
        target: Option<usize>,
    ) -> Result<(usize, bool), ErrorCode> {
        let constant = match constant {
            Const::Unit => self
                .registers
                .get_or_alloc_constant(ConstantValue::Unit)
                .unwrap(),
            Const::Int(i) => self
                .registers
                .get_or_alloc_constant(ConstantValue::Int(*i))
                .unwrap(),
            Const::Float(f) => self
                .registers
                .get_or_alloc_constant(ConstantValue::Float((*f).to_bits()))
                .unwrap(),
            Const::Str(s) => self
                .registers
                .get_or_alloc_constant(ConstantValue::Str(s.clone()))
                .unwrap(),
            Const::Bool(b) => self
                .registers
                .get_or_alloc_constant(ConstantValue::Bool(*b))
                .unwrap(),
            Const::List(list) => {
                let mut items = vec![];
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                for expr in list {
                    let item = self.compile_expr(expr, false, None)?;
                    items.push(item);
                }
                self.get_current_insts()
                    .push(VmInst::OpMakeList(OpMakeList {
                        rd,
                        items: items.iter().map(|(id, _)| *id).collect(),
                    }));
                items.into_iter().for_each(|(id, tmp)| {
                    if tmp {
                        self.registers.free_intermediate(id);
                    }
                });
                return Ok((rd, target.is_none()));
            }
            Const::Table(pairs) => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpMakeTable(OpMakeTable { rd }));
                for (attr, expr, loc) in pairs.iter() {
                    let (value, tmp) = self.compile_expr(expr, false, None)?;
                    if tmp {
                        self.registers.free_intermediate(value);
                    }
                    let attr = self.gc.get_or_insert_table_key(attr);
                    self.get_current_func()
                        .insts
                        .push(VmInst::OpSetTable(OpSetTable {
                            loc: loc.clone(),
                            rs: value,
                            rd,
                            attr,
                        }));
                }
                return Ok((rd, target.is_none()));
            }
        };
        if let Some(target) = target {
            self.get_current_insts().push(VmInst::OpMove(OpMove {
                rs: constant,
                rd: target,
            }));
            Ok((target, false))
        } else {
            Ok((constant, false))
        }
    }

    fn compile_infix(
        &mut self,
        op: &OpInfix,
        lhs: usize,
        rhs: usize,
        loc: Loc,
        target: Option<usize>,
    ) -> (usize, bool) {
        match op {
            OpInfix::Is => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpIs(OpIs { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::Or | OpInfix::And => unreachable!(),
            OpInfix::Eq => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpEq(OpEq { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::Ne => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpNe(OpNe { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::Ge => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpGe(OpGe { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::Gt => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpGt(OpGt { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::Lt => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpLt(OpLt { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::Le => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpLe(OpLe { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::Plus => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpAdd(OpAdd { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::Minus => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpSub(OpSub { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::Mul => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpMul(OpMul { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::Div => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpDiv(OpDiv { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::DivFloor => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpIDiv(OpIDiv { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::Rem => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpRem(OpRem { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::Exp => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpPow(OpPow { loc, lhs, rhs, rd }));
                (rd, target.is_none())
            }
            OpInfix::Assign
            | OpInfix::Range
            | OpInfix::Comma
            | OpInfix::Member
            | OpInfix::DoubleColon
            | OpInfix::LArrow => {
                unreachable!()
            }
        }
    }

    fn compile_prefix(
        &mut self,
        op: &OpPrefix,
        rhs: usize,
        loc: Loc,
        target: Option<usize>,
    ) -> (usize, bool) {
        match op {
            OpPrefix::Not => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpNot(OpNot { loc, lhs: rhs, rd }));
                (rd, target.is_none())
            }
            OpPrefix::Neg => {
                let rd = target.unwrap_or_else(|| self.registers.declare_intermediate());
                self.get_current_func()
                    .insts
                    .push(VmInst::OpNeg(OpNeg { loc, lhs: rhs, rd }));
                (rd, target.is_none())
            }
        }
    }

    /// Return (func_id, parameters len, captured_regs, reg_size)
    fn compile_closure(
        &mut self,
        parameters: &[(String, Loc)],
        body: &Expr,
    ) -> std::result::Result<(usize, usize, Vec<Capture>, usize), ErrorCode> {
        let func_id = self.byte_code.len();
        self.byte_code.push(Func {
            id: func_id,
            parameters: parameters.len(),
            insts: vec![],
        });
        self.registers.enter_function(func_id);
        for (para, loc) in parameters.iter() {
            self.registers.declare_variable(para, Some(loc.clone()));
        }

        let func_id = self.registers.func_id;
        // scan all constant values
        let mut const_scanner = ConstScanner {
            register_table: &mut self.registers,
            gc: &mut self.gc,
            insts: &mut self.byte_code[func_id].insts,
        };
        const_scanner.scan_expr(body);

        // scan all captured variable (include nested closure)
        let mut capture_scanner = CaptureScanner {
            register_table: &mut self.registers,
            gc: &mut self.gc,
            insts: &mut self.byte_code[func_id].insts,
            overridden: AHashMap::new(),
        };
        capture_scanner.scan_expr(body);

        let result = match body {
            Expr::Infix {
                op: OpInfix::Assign,
                lhs,
                rhs,
                ..
            } => {
                self.compile_assignment(lhs, rhs).map_err(|err| {
                    self.registers.leave_function();
                    err
                })?;
                (0, false)
            }
            body => self.compile_expr(body, false, None).map_err(|err| {
                self.registers.leave_function();
                err
            })?,
        };
        // return expression value
        self.get_current_insts().push(VmInst::OpRet(OpRet {
            return_reg: result.0,
        }));
        let reg_size = self.registers.assigned;
        let captured_regs = self.registers.leave_function();
        Ok((func_id, parameters.len(), captured_regs, reg_size))
    }

    fn get_current_func(&mut self) -> &mut Func {
        let id = self.registers.func_id;
        &mut self.byte_code[id]
    }

    fn get_current_insts(&mut self) -> &mut Vec<VmInst> {
        let id = self.registers.func_id;
        &mut self.byte_code[id].insts
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
