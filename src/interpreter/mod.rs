use std::ffi::OsStr;
use std::fmt::Write;

use ahash::{AHashMap, AHashSet};

mod error;

use crate::{
    diagnostic::{Diagnostic, Loc},
    frontend::{
        parser::ast::{Const, Expr, Expr_, OpInfix, OpPrefix, Stmt, Stmt_},
        Ast, Parser,
    },
    vm::{
        error::VmError,
        op::{
            OpAdd, OpAllocReg, OpAnd, OpBranch, OpDiv, OpDummy, OpEq, OpGt, OpIDiv, OpJump,
            OpLoadConstant, OpMove, OpMul, OpNeg, OpNot, OpOr, OpPow, OpRem, OpSub, OpYield,
        },
        Instruction, Ip, Reg, Vm, VmInst,
    },
};

use self::error::ErrorCode;

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
struct RegisterTable {
    prev: Option<Box<RegisterTable>>,
    variables: AHashMap<String, usize>,
    free: Vec<usize>,
    assigned: usize,
    func_id: usize,
    constant_table: AHashMap<ConstantValue, usize>,
}

impl RegisterTable {
    fn new() -> Self {
        Self {
            prev: None,
            variables: AHashMap::default(),
            free: vec![],
            assigned: 0,
            func_id: 0,
            constant_table: AHashMap::default(),
        }
    }

    fn declare_or_get_variable(&mut self, name: impl AsRef<str>) -> (usize, usize) {
        let exist = self.lookup_variable(name.as_ref());
        if let Ok(ret) = exist {
            return ret;
        }
        let id = self.declare_intermediate();
        self.variables.insert(name.as_ref().to_string(), id);
        (id, 0)
    }

    /// Return (reg_id, func_id)
    fn lookup_variable(&self, name: impl AsRef<str>) -> Result<(usize, usize), ()> {
        let var = self.variables.get(name.as_ref());
        match var {
            Some(id) => Ok((*id, self.func_id)),
            None => match &self.prev {
                Some(prev) => prev.lookup_variable(name),
                None => Err(()),
            },
        }
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
}

pub struct Func {
    pub name: String,
    pub parameters: usize,
    pub insts: Vec<VmInst>,
    pub captures: Vec<usize>,
}

pub struct Interpreter {
    registers: RegisterTable,
    scopes: Vec<AHashSet<String>>,
    byte_code: Vec<Func>,
    /// Location to insert load constant
    current_start_loc: usize,
    /// Count how many load constant inserted
    insert_offset: usize,
    vm: Vm,
}

impl Interpreter {
    pub fn new() -> Self {
        let main = Func {
            name: "main".to_string(),
            parameters: 0,
            insts: vec![],
            captures: vec![],
        };
        Self {
            registers: RegisterTable::new(),
            scopes: vec![AHashSet::new()],
            byte_code: vec![main],
            current_start_loc: 0,
            insert_offset: 0,
            vm: Vm::new(),
        }
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
            captures: _,
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
        self.current_start_loc = self.byte_code[0].insts.len();
        self.insert_offset = 0;

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
            0,
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
        for stmt in ast.statements.iter() {
            match self.compile_stmt(stmt) {
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
            Ok(return_value)
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<Option<usize>, ErrorCode> {
        let Stmt { loc, val: stmt } = stmt;
        let mut return_value = None;
        match stmt {
            Stmt_::Expr(Expr {
                loc,
                val: Expr_::Infix(OpInfix::Assign, lhs, rhs),
            }) => self.compile_assignment(lhs, rhs, loc.clone())?,
            Stmt_::Expr(expr) => {
                let (reg_id, tmp) = self.compile_expr(expr)?;
                return_value = Some(reg_id);
                if tmp {
                    self.registers.free_intermediate(reg_id)
                }
            }
            Stmt_::Continue => todo!(),
            Stmt_::Break => todo!(),
            Stmt_::Return(_) => todo!(),
            Stmt_::Data(_, _, _) => todo!(),
            Stmt_::Loop(condition, body) => {
                let start_label = self.get_current_func().insts.len();
                let before_test_inserted = self.insert_offset;
                let branch_inst = if let Some(condition) = condition {
                    let (condition_reg, tmp) = self.compile_expr(condition)?;
                    if tmp {
                        self.registers.free_intermediate(condition_reg);
                    }
                    self.get_current_func().insts.push(VmInst::OpDummy(OpDummy));
                    Some((
                        self.get_current_func().insts.len() - 1,
                        condition_reg,
                        condition.loc.clone(),
                    ))
                } else {
                    None
                };
                let after_test_inserted = self.insert_offset;
                self.enter_block();
                for stmt in body.iter() {
                    self.compile_stmt(stmt).map_err(|err| {
                        self.leave_block();
                        err
                    })?;
                }
                self.leave_block();

                // number of load const inserted after compile block
                let inserted = self.insert_offset - before_test_inserted;
                let jump_loc = self.get_current_func().insts.len();

                self.get_current_func().insts.push(VmInst::OpJump(OpJump {
                    loc: loc.clone(),
                    offset: start_label as i64 + inserted as i64 - jump_loc as i64,
                }));

                // jump out of loop
                let inserted = self.insert_offset - after_test_inserted;
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
            Stmt_::For(_, _, _) => todo!(),
            Stmt_::Def(_, _, _, _) => todo!(),
            Stmt_::Error => unreachable!(),
        }
        Ok(return_value)
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(usize, bool), ErrorCode> {
        let Expr { loc, val: expr } = expr;
        match expr {
            Expr_::Block(_) => todo!(),
            Expr_::If(_) => todo!(),
            Expr_::Prefix(op, lhs) => {
                let (lhs_id, lhs_tmp) = self.compile_expr(lhs)?;
                if lhs_tmp {
                    self.registers.free_intermediate(lhs_id)
                };
                Ok(self.compile_prefix(op, lhs_id, loc.clone()))
            }
            Expr_::Call(_, _) => todo!(),
            Expr_::Index(_, _) => todo!(),
            Expr_::Construct(_, _) => todo!(),
            Expr_::Infix(OpInfix::Assign, _, _) => Err(ErrorCode::InvalidAssignment(loc.clone())),
            Expr_::Infix(op, lhs, rhs) => {
                let (lhs_id, lhs_tmp) = self.compile_expr(lhs)?;
                let (rhs_id, rhs_tmp) = self.compile_expr(rhs)?;
                let ret = self.compile_infix(op, lhs_id, rhs_id, loc.clone());
                if lhs_tmp {
                    self.registers.free_intermediate(lhs_id);
                };
                if rhs_tmp {
                    self.registers.free_intermediate(rhs_id);
                };
                Ok(ret)
            }
            Expr_::Fn(_, _) => todo!(),
            Expr_::Id(id) => match self.registers.lookup_variable(id) {
                Ok((id, _)) => Ok((id, false)),
                Err(()) => Err(ErrorCode::NameNotDefined(loc.clone(), id.clone())),
            },
            Expr_::Parentheses(expr) => self.compile_expr(expr),
            Expr_::Const(c) => Ok(self.compile_constant(c, loc.clone())),
            Expr_::Case(_, _) => todo!(),
            Expr_::Module(_) => todo!(),
            Expr_::Error => unreachable!(),
        }
    }

    fn compile_assignment(&mut self, lhs: &Expr, rhs: &Expr, loc: Loc) -> Result<(), ErrorCode> {
        if let Expr {
            val: Expr_::Id(id),
            loc: _,
        } = lhs
        {
            // register to local block
            if self.registers.lookup_variable(id).is_err() {
                self.scopes.last_mut().unwrap().insert(id.clone());
            }
            // declare variable
            let (id, _) = self.registers.declare_or_get_variable(id);
            let (rhs, tmp) = self.compile_expr(rhs)?;
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
            Err(ErrorCode::CannotAssign(lhs.loc.clone()))
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
            Const::Set(_) => todo!(),
            Const::Dict(_, _) => todo!(),
        };
        match constant {
            Ok(reg_id) => (reg_id, false),
            Err((reg_id, reg)) => {
                let insert_loc = self.current_start_loc;
                self.insert_offset += 1;
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

    fn compile_prefix(&mut self, op: &OpPrefix, lhs: usize, loc: Loc) -> (usize, bool) {
        match op {
            OpPrefix::Not => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpNot(OpNot { loc, lhs, rd }));
                (rd, true)
            }
            OpPrefix::Neg => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(VmInst::OpNeg(OpNeg { loc, lhs, rd }));
                (rd, true)
            }
        }
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
