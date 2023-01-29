use std::ffi::OsStr;

use ahash::{AHashMap, AHashSet};

mod error;

use crate::{
    diagnostic::{Diagnostic, Loc},
    frontend::{
        parser::ast::{Const, Expr, Expr_, OpInfix, Stmt, Stmt_, OpPrefix},
        Ast, Parser,
    },
    vm::{
        op::{OpAdd, OpLoadConstant, OpSub, OpDiv, OpMul, OpNot, OpNeg, OpIDiv, OpRem, OpPow, OpEq, OpLt, OpAnd, OpOr},
        Instruction, Ip, Reg, Vm,
    },
};

use self::error::ErrorCode;

#[derive(Clone)]
struct RegisterTable {
    variables: AHashMap<String, usize>,
    free: Vec<usize>,
    scopes: Vec<AHashSet<String>>,
    assigned: usize,
}

impl RegisterTable {
    fn new() -> Self {
        Self {
            variables: AHashMap::default(),
            free: vec![],
            scopes: vec![AHashSet::default()],
            assigned: 0,
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(AHashSet::new());
    }

    fn leave_scope(&mut self) {
        self.scopes.pop().map(|scope| {
            scope
                .into_iter()
                .map(|name| self.variables.remove(&name).map(|reg| self.free.push(reg)))
        });
    }

    fn declare_variable(&mut self, name: String) -> usize {
        let reg = self.free.pop().unwrap_or_else(|| {
            let reg = self.assigned;
            self.assigned += 1;
            reg
        });
        self.scopes
            .last_mut()
            .map(|scope| scope.insert(name.clone()));
        self.variables.insert(name, reg);
        reg
    }

    fn lookup_variable(&self, name: impl AsRef<str>) -> Result<(usize, bool), ()> {
        self.variables.get(name.as_ref()).ok_or(()).map(|reg| {
            self.scopes
                .last()
                .map(|scope| {
                    scope
                        .get(name.as_ref())
                        .map(|_| (*reg, true))
                        .unwrap_or((*reg, false))
                })
                .unwrap_or((*reg, false))
        })
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
    variables: Vec<RegisterTable>,
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
            variables: vec![RegisterTable::new()],
            compile_func_stack: vec![0],
            byte_code: vec![main],
            vm: Vm::new(),
        }
    }

    pub fn exec_repl(&mut self, code: impl AsRef<str>, color: bool) -> Result<String, String> {
        let mut parser = Parser::new();
        let mut ast = parser.parse_str(OsStr::new("<interactive>"), code.as_ref());
        if ast.diagnoser.error_count() > 0 {
            return Err(ast.diagnoser.render(color));
        }

        let variables_prev = self.variables.clone();
        self.vm.set_ip(Ip {
            func_id: 0,
            inst: self.byte_code[0].insts.len(),
        });

        let return_value = self.compile_ast(&mut ast).map_err(|_| {
            // restore variable table if compile failed
            self.variables = variables_prev;
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
            let Stmt { loc: _, val: stmt } = stmt;
            match stmt {
                Stmt_::Expr(expr) => {
                    let result = self.compile_expr(expr);
                    match result {
                        Ok((reg_id, tmp)) => {
                            return_value = Some(reg_id);
                            if tmp {
                                self.release_local_intermediate(reg_id)
                            }
                        }
                        Err(_) => has_error = true,
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
        }

        if has_error {
            Err(())
        } else {
            Ok(return_value)
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(usize, bool), ErrorCode> {
        let Expr { loc, val: expr } = expr;
        match expr {
            Expr_::Block(_) => todo!(),
            Expr_::If(_) => todo!(),
            Expr_::Prefix(op, lhs) => {
                let (lhs_id, lhs_tmp) = self.compile_expr(lhs)?;
                if lhs_tmp {
                    self.release_local_intermediate(lhs_id)
                };
                Ok(self.compile_prefix(op, lhs_id, loc.clone()))
            },
            Expr_::Call(_, _) => todo!(),
            Expr_::Index(_, _) => todo!(),
            Expr_::Construct(_, _) => todo!(),
            Expr_::Infix(op, lhs, rhs) => {
                let (lhs_id, lhs_tmp) = self.compile_expr(lhs)?;
                let (rhs_id, rhs_tmp) = self.compile_expr(rhs)?;
                let ret = self.compile_infix(op, lhs_id, rhs_id, loc.clone());
                if lhs_tmp {
                    self.release_local_intermediate(lhs_id)
                };
                if rhs_tmp {
                    self.release_local_intermediate(rhs_id)
                };
                Ok(ret)
            }
            Expr_::Fn(_, _) => todo!(),
            Expr_::Id(_) => todo!(),
            Expr_::Parentheses(expr) => self.compile_expr(expr),
            Expr_::Const(c) => Ok(self.compile_constant(c, loc.clone())),
            Expr_::Case(_, _) => todo!(),
            Expr_::Module(_) => todo!(),
            Expr_::Error => unreachable!(),
        }
    }

    fn compile_constant(&mut self, constant: &Const, loc: Loc) -> (usize, bool) {
        let rd = self.declare_local_intermediate();
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
            OpInfix::Assign => todo!(),
            OpInfix::Range => todo!(),
            OpInfix::Or => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpOr { loc, lhs, rhs, rd }));
                (rd, true)
            },
            OpInfix::And => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpAnd { loc, lhs, rhs, rd }));
                (rd, true)
            },
            OpInfix::Eq => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpEq { loc, lhs, rhs, rd }));
                (rd, true)
            },
            OpInfix::Ne => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpEq { loc: loc.clone(), lhs, rhs, rd }));
                self.get_current_func()
                    .insts
                    .push(Box::new(OpNot { loc, lhs: rd, rd }));
                (rd, true)
            },
            OpInfix::Le => {
                let rd1 = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpLt { loc: loc.clone(), lhs, rhs, rd: rd1 }));
                let rd2 = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpEq { loc: loc.clone(), lhs, rhs, rd: rd2 }));
                self.release_local_intermediate(rd1);
                self.release_local_intermediate(rd2);
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpOr { loc, lhs: rd1, rhs: rd2, rd }));
                println!("{lhs} {rhs} {rd1} {rd2} {rd}");
                (rd, true)
            },
            OpInfix::Lt => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpLt { loc, lhs, rhs, rd }));
                (rd, true)
            },
            OpInfix::Ge => {
                let rd1 = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpLt { loc: loc.clone(), lhs, rhs, rd: rd1 }));
                self.get_current_func()
                    .insts
                    .push(Box::new(OpNot { loc: loc.clone(), lhs: rd1, rd: rd1 }));
                let rd2 = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpEq { loc: loc.clone(), lhs, rhs, rd: rd2 }));
                self.release_local_intermediate(rd1);
                self.release_local_intermediate(rd2);
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpOr { loc, lhs: rd1, rhs: rd2, rd }));
                (rd, true)
            },
            OpInfix::Gt => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpLt { loc: loc.clone(), lhs, rhs, rd }));
                self.get_current_func()
                    .insts
                    .push(Box::new(OpNot { loc, lhs: rd, rd }));
                (rd, true)
            },
            OpInfix::Plus => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpAdd { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Minus => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpSub { loc, lhs, rhs, rd }));
                (rd, true)
            }
            OpInfix::Mul => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpMul { loc, lhs, rhs, rd }));
                (rd, true)
            },
            OpInfix::Div => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpDiv { loc, lhs, rhs, rd }));
                (rd, true)
            },
            OpInfix::DivFloor => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpIDiv { loc, lhs, rhs, rd }));
                (rd, true)
            },
            OpInfix::Rem => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpRem { loc, lhs, rhs, rd }));
                (rd, true)
            },
            OpInfix::Exp => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpPow { loc, lhs, rhs, rd }));
                (rd, true)
            },
            OpInfix::Comma => todo!(),
            OpInfix::Member => todo!(),
        }
    }

    fn compile_prefix(&mut self, op: &OpPrefix, lhs: usize, loc: Loc) -> (usize, bool) {
        match op{
            OpPrefix::Not => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpNot{ loc, lhs, rd }));
                (rd, true)
            },
            OpPrefix::Neg => {
                let rd = self.declare_local_intermediate();
                self.get_current_func()
                    .insts
                    .push(Box::new(OpNeg{ loc, lhs, rd }));
                (rd, true)
            },
        }
    }
    fn get_current_func(&mut self) -> &mut Func {
        let id = self.compile_func_stack.last().unwrap();
        &mut self.byte_code[*id]
    }

    fn release_local_intermediate(&mut self, reg_id: usize) {
        if let Some(t) = self.variables.last_mut() {
            t.free_intermediate(reg_id)
        }
    }

    fn declare_local_intermediate(&mut self) -> usize {
        self.variables.last_mut().unwrap().declare_intermediate()
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}
