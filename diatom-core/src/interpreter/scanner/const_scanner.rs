use super::*;

/// Scan and declare all constant before compile closure
pub struct ConstScanner<'a, Buffer: IoWrite> {
    pub register_table: &'a mut RegisterTable,
    pub gc: &'a mut Gc<Buffer>,
    pub insts: &'a mut Vec<VmInst>,
}

impl<'a, Buffer: IoWrite> ConstScanner<'a, Buffer> {
    pub fn scan_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Block { body, .. } => body.iter().for_each(|stmt| self.scan_stmt(stmt)),
            Expr::If {
                conditional,
                default,
                ..
            } => conditional.iter().for_each(|(expr, stmts)| {
                self.scan_expr(expr);
                stmts.iter().for_each(|stmt| self.scan_stmt(stmt));
                if let Some(stmts) = default {
                    stmts.iter().for_each(|stmt| self.scan_stmt(stmt))
                }
            }),
            Expr::Prefix { rhs, .. } => self.scan_expr(rhs),
            Expr::Call {
                lhs, parameters, ..
            } => {
                self.scan_expr(lhs);
                parameters.iter().for_each(|expr| self.scan_expr(expr));
            }
            Expr::Index { lhs, rhs, .. } => {
                self.scan_expr(lhs);
                self.scan_expr(rhs);
            }
            Expr::OpenRange { lhs, .. } => {
                self.scan_expr(lhs);
                self.scan_const(&Const::Int(i64::MAX));
            }
            // Member rhs can not have legal constant values
            Expr::Infix {
                lhs,
                op: OpInfix::Member | OpInfix::DoubleColon,
                ..
            } => {
                self.scan_expr(lhs);
            }
            Expr::Infix { lhs, rhs, .. } => {
                self.scan_expr(lhs);
                self.scan_expr(rhs);
            }
            Expr::Fn { .. } => (),
            Expr::Id { .. } => (),
            Expr::Parentheses { content, .. } => self.scan_expr(content),
            Expr::Const { value, .. } => self.scan_const(value),
            Expr::Error => unreachable!(),
        }
    }

    pub fn scan_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr { expr, .. } => self.scan_expr(expr),
            Stmt::Continue { .. } => (),
            Stmt::Break { .. } => (),
            Stmt::Return { value, .. } => {
                if let Some(expr) = value {
                    self.scan_expr(expr)
                }
            }
            Stmt::Loop {
                condition, body, ..
            } => {
                if let Some(expr) = condition {
                    self.scan_expr(expr)
                }
                body.iter().for_each(|stmt| self.scan_stmt(stmt));
            }
            Stmt::For { iterator, body, .. } => {
                self.scan_expr(iterator);
                body.iter().for_each(|stmt| self.scan_stmt(stmt));
            }
            Stmt::Def { variable, .. } => self.scan_expr(variable),
            Stmt::Import { .. } => (),
            Stmt::Error => unreachable!(),
        }
    }

    fn scan_const(&mut self, constant: &Const) {
        let constant = match constant {
            Const::Unit => self
                .register_table
                .get_or_alloc_constant(ConstantValue::Unit)
                .map_err(|reg| (reg, Reg::Unit)),
            Const::Int(i) => self
                .register_table
                .get_or_alloc_constant(ConstantValue::Int(*i))
                .map_err(|reg| (reg, Reg::Int(*i))),
            Const::Float(f) => self
                .register_table
                .get_or_alloc_constant(ConstantValue::Float((*f).to_bits()))
                .map_err(|reg| (reg, Reg::Float(*f))),
            Const::Str(s) => self
                .register_table
                .get_or_alloc_constant(ConstantValue::Str(s.clone()))
                .map_err(|reg| {
                    let sid = self.gc.alloc_str_pinned(s.clone());
                    (reg, Reg::Str(sid))
                }),
            Const::Bool(b) => self
                .register_table
                .get_or_alloc_constant(ConstantValue::Bool(*b))
                .map_err(|reg| (reg, Reg::Bool(*b))),
            Const::List(list) => {
                list.iter().for_each(|expr| self.scan_expr(expr));
                return;
            }
            Const::Table(entries) => {
                entries.iter().for_each(|(_, expr, _)| self.scan_expr(expr));
                return;
            }
        };
        if let Err((reg_id, reg)) = constant {
            self.insts.push(VmInst::OpLoadConstant(OpLoadConstant {
                constant: reg,
                rd: reg_id,
            }));
        };
    }
}
