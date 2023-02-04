use std::cell::RefCell;
use std::fmt::Write;
use std::{ffi::OsStr, rc::Rc};

use ahash::{AHashMap, AHashSet};

mod error;
mod prelude;

use crate::{
    diagnostic::{Diagnostic, Loc},
    frontend::{
        parser::ast::{Const, Expr, OpInfix, OpPrefix, Stmt},
        Ast, Parser,
    },
    vm::{
        error::VmError,
        op::{
            OpAdd, OpAllocReg, OpAnd, OpBranch, OpCallClosure, OpDiv, OpDummy, OpEq, OpGt, OpIDiv,
            OpJump, OpLoadConstant, OpMakeClosure, OpMove, OpMul, OpNeg, OpNot, OpOr, OpPow, OpRem,
            OpRet, OpSub, OpYield,
        },
        GcObject, Instruction, Ip, Reg, Vm, VmInst,
    },
    DiatomValue,
};

use self::error::ErrorCode;
use self::prelude::impl_prelude;

#[derive(Clone, Hash, PartialEq, Eq)]
enum ConstantValue {
    Unit,
    Bool(bool),
    Int(i64),
    // Float must be transmuted in order to compare
    Float(u64),
    Str(String),
}

#[derive(Clone)]
pub struct Capture {
    pub rd: usize,
    pub rs: usize,
    pub depth: usize,
}

#[derive(Clone)]
struct RegisterTable {
    prev: Option<Box<RegisterTable>>,
    variables: AHashMap<String, (usize, Option<Loc>)>,
    free: Vec<usize>,
    assigned: usize,
    func_id: usize,
    constant_table: AHashMap<ConstantValue, usize>,
    /// Location to insert load constant
    current_start_loc: usize,
    /// Count how many load constant inserted
    insert_offset: usize,
    /// captured variables
    capture: Vec<Capture>,
}

impl RegisterTable {
    fn new(func_id: usize) -> Self {
        Self {
            prev: None,
            variables: AHashMap::default(),
            free: vec![],
            assigned: 0,
            func_id,
            constant_table: AHashMap::default(),
            current_start_loc: 0,
            insert_offset: 0,
            capture: vec![],
        }
    }

    fn declare_variable(&mut self, name: impl AsRef<str>, loc: Option<Loc>) -> usize {
        let id = self.declare_intermediate();
        self.variables.insert(name.as_ref().to_string(), (id, loc));
        id
    }

    /// (reg_id, depth, loc)
    fn lookup_variable_(&self, name: &str, depth: usize) -> Option<(usize, usize, Option<Loc>)> {
        let var = self.variables.get(name);
        match var {
            Some((id, loc)) => Some((*id, depth, loc.clone())),
            None => match &self.prev {
                Some(prev) => prev.lookup_variable_(name, depth + 1),
                None => None,
            },
        }
    }

    /// Return (reg_id, depth, loc)
    pub fn lookup_variable(&self, name: impl AsRef<str>) -> Option<(usize, usize, Option<Loc>)> {
        self.lookup_variable_(name.as_ref(), 0)
    }

    fn declare_intermediate(&mut self) -> usize {
        self.free.pop().unwrap_or_else(|| {
            self.assigned += 1;
            self.assigned - 1
        })
    }

    fn free_intermediate(&mut self, id: usize) {
        if self.assigned >= id {
            self.free.push(id)
        }
    }

    /// Return ok is constant is in table, otherwise alloc a new register for this constant
    fn get_or_alloc_constant(&mut self, constant: ConstantValue) -> Result<usize, usize> {
        match self.constant_table.get(&constant) {
            Some(id) => Ok(*id),
            None => {
                // must alloc new register to prevent
                // already compiled Instruction
                // overwrite constant value
                let id = self.assigned;
                self.assigned += 1;
                self.constant_table.insert(constant, id);
                Err(id)
            }
        }
    }

    fn enter_function(&mut self, func_id: usize) {
        let old = std::mem::replace(self, RegisterTable::new(func_id));
        self.prev = Some(Box::new(old));
    }

    fn leave_function(&mut self) -> Vec<Capture> {
        let prev = *std::mem::take(&mut self.prev).unwrap();
        let pop = std::mem::replace(self, prev);
        pop.capture
    }
}

pub struct Func {
    pub name: String,
    pub parameters: usize,
    pub insts: Vec<VmInst>,
}

pub struct Interpreter {
    registers: RegisterTable,
    scopes: Vec<AHashSet<String>>,
    byte_code: Vec<Func>,
    vm: Vm,
}

impl Interpreter {
    pub fn new() -> Self {
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
        };
        impl_prelude(&mut interpreter);
        interpreter
    }

    pub fn add_extern_function<F>(&mut self, name: String, f: F)
    where
        F: FnMut(&mut Vm, &[DiatomValue]) -> Result<DiatomValue, String> + 'static,
    {
        let f = GcObject::NativeFunction(Rc::new(RefCell::new(f)));
        let gc_id = self.vm.gc_mut().alloc(f);
        let reg = Reg::Ref(gc_id);
        let reg_id = self.registers.declare_variable(name, None);
        self.vm.gc_mut().alloc_reg_file(reg_id + 1);
        self.vm.gc_mut().write_reg(reg_id, reg);
    }

    pub fn verify_input_completeness(&self, code: impl AsRef<str>) -> bool {
        let mut parser = Parser::new();
        let ast = parser.parse_str(OsStr::new("<interactive>"), code.as_ref());
        !ast.input_can_continue()
    }

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
                inst.decompile(&mut decompiled, &self.vm);
            }
            writeln!(decompiled).unwrap();
        }
        Ok(decompiled)
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

    pub fn exec(
        &mut self,
        code: impl AsRef<str>,
        source: &OsStr,
        color: bool,
    ) -> Result<String, String> {
        let mut ast = self.compile(code, source, color)?;
        match self.vm.exec(&self.byte_code) {
            VmError::Yield => Ok(self.vm.take_output()),
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
                let start_label = self.get_current_func().insts.len();
                let before_test_inserted = self.registers.insert_offset;
                let branch_inst = if let Some(condition) = condition {
                    let (condition_reg, tmp) = self.compile_expr(condition, false)?;
                    if tmp {
                        self.registers.free_intermediate(condition_reg);
                    }
                    self.get_current_func().insts.push(VmInst::OpDummy(OpDummy));
                    Some((
                        self.get_current_func().insts.len() - 1,
                        condition_reg,
                        condition.get_loc(),
                    ))
                } else {
                    None
                };
                let after_test_inserted = self.registers.insert_offset;
                self.enter_block();
                for stmt in body.iter() {
                    self.compile_stmt(stmt, true).map_err(|err| {
                        self.leave_block();
                        err
                    })?;
                }
                self.leave_block();

                // number of load const inserted after compile block
                let inserted = self.registers.insert_offset - before_test_inserted;
                let jump_loc = self.get_current_func().insts.len();

                self.get_current_func().insts.push(VmInst::OpJump(OpJump {
                    loc: loc.clone(),
                    offset: start_label as i64 + inserted as i64 - jump_loc as i64,
                }));

                // jump out of loop
                let inserted = self.registers.insert_offset - after_test_inserted;
                if let Some((inst, reg, loc)) = branch_inst {
                    self.get_current_func().insts[inst + inserted] = VmInst::OpBranch(OpBranch {
                        condition: reg,
                        loc,
                        offset: self.get_current_func().insts.len() as i64
                            - inst as i64
                            - inserted as i64,
                    })
                };
            }
            Stmt::Continue { loc: _ } => todo!(),
            Stmt::Break { loc: _ } => todo!(),
            Stmt::Return { loc: _, value: _ } => todo!(),
            Stmt::For {
                loc: _,
                loop_variable: _,
                iterator: _,
                body: _,
            } => todo!(),
            Stmt::Def {
                loc: _,
                name: _,
                parameters: _,
                body: _,
            } => todo!(),
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
                        let local_id = self.registers.declare_variable(name, loc);
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
            Expr::Const { loc, value } => Ok(self.compile_constant(value, loc.clone())),
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
                        let rd = self.compile_constant(&Const::Unit, loc.clone());
                        Ok(rd)
                    }
                }
            }
            Expr::If {
                loc: _,
                conditional: _,
                default: _,
            } => todo!(),
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
                    self.compile_closure(parameters, body, loc.clone())?;
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
            // register to local block
            if self.registers.lookup_variable(name).is_none() {}
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

    fn compile_constant(&mut self, constant: &Const, loc: Loc) -> (usize, bool) {
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
                    let sid = self.vm.gc_mut().string_pool().alloc(s.clone());
                    (reg, Reg::Str(sid))
                }),
            Const::Bool(b) => self
                .registers
                .get_or_alloc_constant(ConstantValue::Bool(*b))
                .map_err(|reg| (reg, Reg::Bool(*b))),
            Const::List(_) => todo!(),
            Const::Table(_) => todo!(),
        };
        match constant {
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
        }
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
                self.get_current_func().insts.push(VmInst::OpEq(OpEq {
                    loc: loc.clone(),
                    lhs,
                    rhs,
                    rd,
                }));
                self.get_current_func()
                    .insts
                    .push(VmInst::OpNot(OpNot { loc, lhs: rd, rd }));
                (rd, true)
            }
            OpInfix::Ge => {
                let rd1 = self.registers.declare_intermediate();
                self.get_current_func().insts.push(VmInst::OpGt(OpGt {
                    loc: loc.clone(),
                    lhs,
                    rhs,
                    rd: rd1,
                }));
                let rd2 = self.registers.declare_intermediate();
                self.get_current_func().insts.push(VmInst::OpEq(OpEq {
                    loc: loc.clone(),
                    lhs,
                    rhs,
                    rd: rd2,
                }));
                self.registers.free_intermediate(rd1);
                self.registers.free_intermediate(rd2);
                let rd = self.registers.declare_intermediate();
                self.get_current_func().insts.push(VmInst::OpOr(OpOr {
                    loc,
                    lhs: rd1,
                    rhs: rd2,
                    rd,
                }));
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
                let rd1 = self.registers.declare_intermediate();
                self.get_current_func().insts.push(VmInst::OpGt(OpGt {
                    loc: loc.clone(),
                    lhs,
                    rhs,
                    rd: rd1,
                }));
                let rd2 = self.registers.declare_intermediate();
                self.get_current_func().insts.push(VmInst::OpEq(OpEq {
                    loc: loc.clone(),
                    lhs,
                    rhs,
                    rd: rd2,
                }));
                self.registers.free_intermediate(rd1);
                self.registers.free_intermediate(rd2);
                let rd = self.registers.declare_intermediate();
                self.get_current_func().insts.push(VmInst::OpOr(OpOr {
                    loc: loc.clone(),
                    lhs: rd1,
                    rhs: rd2,
                    rd,
                }));
                self.get_current_func()
                    .insts
                    .push(VmInst::OpNot(OpNot { loc, lhs: rd, rd }));
                (rd, true)
            }
            OpInfix::Le => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func().insts.push(VmInst::OpGt(OpGt {
                    loc: loc.clone(),
                    lhs,
                    rhs,
                    rd,
                }));
                self.get_current_func()
                    .insts
                    .push(VmInst::OpNot(OpNot { loc, lhs: rd, rd }));
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
        body: &Expr,
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
        let result = self.compile_expr(body, false).map_err(|err| {
            self.registers.leave_function();
            err
        })?;
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

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
