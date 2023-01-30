use std::ffi::OsStr;

use ahash::AHashMap;

mod error;

use crate::{
    diagnostic::{Diagnostic, Loc},
    frontend::{
        parser::ast::{Const, Expr, Expr_, OpInfix, OpPrefix, Stmt, Stmt_},
        Ast, Parser,
    },
    vm::{
        op::{
            OpAdd, OpAnd, OpDiv, OpEq, OpIDiv, OpLoadConstant, OpLt, OpMove, OpMul, OpNeg, OpNot,
            OpOr, OpPow, OpRem, OpSub,
        },
        Instruction, Ip, Reg, Vm,
    },
};

use self::error::ErrorCode;

#[derive(Clone)]
struct RegisterTable {
    prev: Option<Box<RegisterTable>>,
    variables: AHashMap<String, usize>,
    free: Vec<usize>,
    assigned: usize,
}

impl RegisterTable {
    fn new() -> Self {
        Self {
            prev: None,
            variables: AHashMap::default(),
            free: vec![],
            assigned: 0,
        }
    }

    fn declare_or_get_variable(&mut self, name: impl AsRef<str>) -> (usize, usize) {
        let exist = self.lookup_variable(name.as_ref(), 0);
        if let Ok(ret) = exist {
            return ret;
        }
        let id = self.declare_intermediate();
        self.variables.insert(name.as_ref().to_string(), id);
        (id, 0)
    }

    /// Return (reg_id, depth)
    fn lookup_variable(&self, name: impl AsRef<str>, depth: usize) -> Result<(usize, usize), ()> {
        let var = self.variables.get(name.as_ref());
        match var {
            Some(id) => Ok((*id, depth)),
            None => match &self.prev {
                Some(prev) => prev.lookup_variable(name, depth + 1),
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
}

pub struct Func {
    pub name: String,
    pub parameters: usize,
    pub insts: Vec<Box<dyn Instruction>>,
    pub captures: Vec<usize>,
}

pub struct Interpreter {
    registers: RegisterTable,
    compile_func_stack: Vec<usize>,
    byte_code: Vec<Func>,
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
            compile_func_stack: vec![0],
            byte_code: vec![main],
            vm: Vm::new(),
        }
    }

    pub fn verify_input_completeness(&self, code: impl AsRef<str>) -> bool {
        let mut parser = Parser::new();
        let ast = parser.parse_str(OsStr::new("<interactive>"), code.as_ref());
        !ast.input_can_continue()
    }

    pub fn exec(&mut self, code: impl AsRef<str>, color: bool) -> Result<String, String> {
        let mut parser = Parser::new();
        let mut ast = parser.parse_str(OsStr::new("<interactive>"), code.as_ref());
        if ast.diagnoser.error_count() > 0 {
            return Err(ast.diagnoser.render(color));
        }

        let registers_prev = self.registers.clone();
        self.vm.set_ip(Ip {
            func_id: 0,
            inst: self.byte_code[0].insts.len(),
        });

        let return_value = self.compile_ast(&mut ast).map_err(|_| {
            // restore variable table if compile failed
            self.registers = registers_prev;
            ast.diagnoser.render(color)
        })?;
        self.vm.exec(&self.byte_code).map_err(|error_code| {
            let diagnostic = Diagnostic::from(error_code);
            ast.diagnoser.push(diagnostic);
            ast.diagnoser.render(color)
        })?;

        return_value
            .map(|reg_id| {
                let reg = self.vm.gc().read_reg(reg_id);
                match reg {
                    Reg::Unit => "()".to_string(),
                    Reg::Bool(b) => format!("{b}"),
                    Reg::Int(i) => format!("{i}"),
                    Reg::Float(f) => format!("{f}"),
                    Reg::Str(sid) => self.vm.gc().get_string_by_id(sid).to_string(),
                    Reg::Ref(r) => format!("Object@<{r}>"),
                }
            })
            .ok_or_else(String::new)
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
        let Stmt { loc: _, val: stmt } = stmt;
        let return_value: Option<_>;
        match stmt {
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
            Stmt_::Loop(_, _) => todo!(),
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
            Expr_::Infix(op, lhs, rhs) => {
                if let OpInfix::Assign = op {
                    self.compile_assignment(lhs, rhs, loc.clone())
                } else {
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
            }
            Expr_::Fn(_, _) => todo!(),
            Expr_::Id(id) => match self.registers.lookup_variable(id, 0) {
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

    fn compile_assignment(
        &mut self,
        lhs: &Expr,
        rhs: &Expr,
        loc: Loc,
    ) -> Result<(usize, bool), ErrorCode> {
        if let Expr {
            val: Expr_::Id(id),
            loc: _,
        } = lhs
        {
            let (id, _) = self.registers.declare_or_get_variable(id);
            let (rhs, tmp) = self.compile_expr(rhs)?;
            if tmp {
                self.registers.free_intermediate(rhs);
            }
            self.get_current_func().insts.push(Box::new(OpMove {
                loc: loc.clone(),
                rs: rhs,
                rd: id,
            }));
            let ret = self.registers.declare_intermediate();
            self.get_current_func().insts.push(Box::new(OpLoadConstant {
                constant: Reg::Unit,
                rd: ret,
                loc,
            }));
            Ok((ret, true))
        } else {
            Err(ErrorCode::CannotAssign(lhs.loc.clone()))
        }
    }

    fn compile_constant(&mut self, constant: &Const, loc: Loc) -> (usize, bool) {
        let rd = self.registers.declare_intermediate();
        let constant = match constant {
            Const::Unit => Reg::Unit,
            Const::Int(i) => Reg::Int(*i),
            Const::Float(f) => Reg::Float(*f),
            Const::Str(s) => {
                let sid = self.vm.gc_mut().string_pool().add(s.clone());
                Reg::Str(sid)
            }
            Const::Bool(b) => Reg::Bool(*b),
            Const::List(_) => todo!(),
            Const::Set(_) => todo!(),
            Const::Dict(_, _) => todo!(),
        };
        self.get_current_func()
            .insts
            .push(Box::new(OpLoadConstant { constant, rd, loc }));
        (rd, true)
    }

    fn compile_infix(&mut self, op: &OpInfix, lhs: usize, rhs: usize, loc: Loc) -> (usize, bool) {
        match op {
            OpInfix::Assign => unreachable!(),
            OpInfix::Range => todo!(),
            OpInfix::Or => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpOr { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::And => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpAnd { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Eq => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpEq { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Ne => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func().insts.push(Box::new(OpEq {
                    loc: loc.clone(),
                    lhs,
                    rhs,
                    rd,
                }));
                self.get_current_func()
                    .insts
                    .push(Box::new(OpNot { loc, lhs: rd, rd }));
                (rd, true)
            }
            OpInfix::Le => {
                let rd1 = self.registers.declare_intermediate();
                self.get_current_func().insts.push(Box::new(OpLt {
                    loc: loc.clone(),
                    lhs,
                    rhs,
                    rd: rd1,
                }));
                let rd2 = self.registers.declare_intermediate();
                self.get_current_func().insts.push(Box::new(OpEq {
                    loc: loc.clone(),
                    lhs,
                    rhs,
                    rd: rd2,
                }));
                self.registers.free_intermediate(rd1);
                self.registers.free_intermediate(rd2);
                let rd = self.registers.declare_intermediate();
                self.get_current_func().insts.push(Box::new(OpOr {
                    loc,
                    lhs: rd1,
                    rhs: rd2,
                    rd,
                }));
                println!("{lhs} {rhs} {rd1} {rd2} {rd}");
                (rd, true)
            }
            OpInfix::Lt => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpLt { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Ge => {
                let rd1 = self.registers.declare_intermediate();
                self.get_current_func().insts.push(Box::new(OpLt {
                    loc: loc.clone(),
                    lhs,
                    rhs,
                    rd: rd1,
                }));
                self.get_current_func().insts.push(Box::new(OpNot {
                    loc: loc.clone(),
                    lhs: rd1,
                    rd: rd1,
                }));
                let rd2 = self.registers.declare_intermediate();
                self.get_current_func().insts.push(Box::new(OpEq {
                    loc: loc.clone(),
                    lhs,
                    rhs,
                    rd: rd2,
                }));
                self.registers.free_intermediate(rd1);
                self.registers.free_intermediate(rd2);
                let rd = self.registers.declare_intermediate();
                self.get_current_func().insts.push(Box::new(OpOr {
                    loc,
                    lhs: rd1,
                    rhs: rd2,
                    rd,
                }));
                (rd, true)
            }
            OpInfix::Gt => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func().insts.push(Box::new(OpLt {
                    loc: loc.clone(),
                    lhs,
                    rhs,
                    rd,
                }));
                self.get_current_func()
                    .insts
                    .push(Box::new(OpNot { loc, lhs: rd, rd }));
                (rd, true)
            }
            OpInfix::Plus => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpAdd { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Minus => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpSub { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Mul => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpMul { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Div => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpDiv { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::DivFloor => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpIDiv { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Rem => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpRem { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Exp => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpPow { loc, lhs, rhs, rd }));
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
                    .push(Box::new(OpNot { loc, lhs, rd }));
                (rd, true)
            }
            OpPrefix::Neg => {
                let rd = self.registers.declare_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpNeg { loc, lhs, rd }));
                (rd, true)
            }
        }
    }

    fn get_current_func(&mut self) -> &mut Func {
        let id = self.compile_func_stack.last().unwrap();
        &mut self.byte_code[*id]
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
