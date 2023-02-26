use ahash::AHashMap;

use crate::{frontend::parser::ast::ImportItem, interpreter::Capture};

use super::*;

/// Scan and declare all (nested or not) captured variable before compile closure
pub struct CaptureScanner<'a, Buffer: IoWrite> {
    pub register_table: &'a mut RegisterTable,
    pub gc: &'a mut Gc<Buffer>,
    pub insts: &'a mut Vec<VmInst>,
    pub overridden: AHashMap<String, usize>,
}

impl<'a, Buffer: IoWrite> CaptureScanner<'a, Buffer> {
    pub fn scan_name(&mut self, name: impl AsRef<str>) {
        let name = name.as_ref();
        if self.overridden.get(name).is_some() {
            return;
        }
        if let Some((id, depth, loc)) = self.register_table.lookup_variable(name) {
            // variable is captured
            // make a local copy and register capture info
            if depth > 0 {
                assert_eq!(depth, 1);
                let local_id = self.register_table.declare_captured_variable(name, loc);
                self.register_table.capture.push(Capture {
                    rd: local_id,
                    rs: id,
                });
            }
        }
    }

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
                self.scan_expr(&Expr::Id {
                    loc: Loc {
                        start: 0,
                        end: 0,
                        fid: usize::MAX,
                    },
                    name: "Range".to_string(),
                });
                self.scan_expr(lhs);
            }
            // Member rhs can not have legal constant values
            Expr::Infix {
                lhs,
                op: OpInfix::Member | OpInfix::DoubleColon,
                ..
            } => {
                self.scan_expr(lhs);
            }
            Expr::Infix { lhs, rhs, op, .. } => {
                // x..y implicitly use `Range`
                if matches!(op, OpInfix::Range) {
                    self.scan_expr(&Expr::Id {
                        loc: Loc {
                            start: 0,
                            end: 0,
                            fid: usize::MAX,
                        },
                        name: "Range".to_string(),
                    });
                }
                self.scan_expr(lhs);
                self.scan_expr(rhs);
            }
            Expr::Fn {
                parameters, body, ..
            } => {
                // Parameters will override upper scope variables
                parameters.iter().for_each(|(name, _)| {
                    if let Some(count) = self.overridden.get_mut(name) {
                        *count += 1
                    } else {
                        self.overridden.insert(name.clone(), 1);
                    }
                });

                self.scan_expr(body);

                parameters.iter().for_each(|(name, _)| {
                    let count = self.overridden.get_mut(name).unwrap();
                    *count -= 1;
                    if *count == 0 {
                        self.overridden.remove(name);
                    }
                })
            }
            Expr::Id { name, .. } => {
                self.scan_name(name);
            }
            Expr::ExternId { .. } => (),
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
                // For macro implicitly use `Option`
                self.scan_expr(&Expr::Id {
                    loc: Loc {
                        start: 0,
                        end: 0,
                        fid: usize::MAX,
                    },
                    name: "Option".to_string(),
                });
                self.scan_expr(iterator);
                body.iter().for_each(|stmt| self.scan_stmt(stmt));
            }
            Stmt::Def {
                variable,
                parameters,
                body,
                ..
            } => {
                self.scan_expr(variable);

                // Parameters will override upper scope variables
                parameters.iter().for_each(|(name, _)| {
                    if let Some(count) = self.overridden.get_mut(name) {
                        *count += 1
                    } else {
                        self.overridden.insert(name.clone(), 1);
                    }
                });

                body.iter().for_each(|stmt| self.scan_stmt(stmt));

                parameters.iter().for_each(|(name, _)| {
                    let count = self.overridden.get_mut(name).unwrap();
                    *count -= 1;
                    if *count == 0 {
                        self.overridden.remove(name);
                    }
                })
            }
            Stmt::Import { items, .. } => {
                items.iter().for_each(|ImportItem { alias, path, .. }| {
                    let name = if let Some(alias) = alias {
                        alias
                    } else {
                        path.last().unwrap()
                    };
                    self.scan_name(name);
                })
            }
            Stmt::Error => unreachable!(),
        }
    }

    fn scan_const(&mut self, constant: &Const) {
        match constant {
            Const::List(list) => {
                list.iter().for_each(|expr| self.scan_expr(expr));
            }
            Const::Table(entries) => {
                entries.iter().for_each(|(_, expr, _)| self.scan_expr(expr));
            }
            _ => (),
        }
    }
}
