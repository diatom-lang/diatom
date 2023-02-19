use crate::{
    file_manager::Loc,
    gc::{Gc, GcObject, Reg, Table},
    interpreter::Capture,
    IoWrite, State,
};
use std::{collections::BTreeMap, fmt::Write};

use super::{Instruction, Ip, VmError};

fn get_type<Buffer: IoWrite>(reg: &Reg, gc: &Gc<Buffer>) -> String {
    match reg {
        Reg::Unit => "()".to_string(),
        Reg::Bool(_) => "Bool".to_string(),
        Reg::Int(_) => "Int".to_string(),
        Reg::Float(_) => "Float".to_string(),
        Reg::Str(_) => "String".to_string(),
        Reg::Ref(r) => {
            let obj = unsafe { gc.get_obj_unchecked(*r) };
            match obj {
                GcObject::Closure {
                    func_id: _,
                    parameters: _,
                    captured: _,
                    reg_size: _,
                } => "Closure".to_string(),
                GcObject::NativeFunction(_) => "Extern_Function".to_string(),
                GcObject::Table(_) => "Table".to_string(),
                GcObject::Tuple(_) => "Tuple".to_string(),
                GcObject::List(_) => "List".to_string(),
            }
        }
    }
}

const FORMAT_PAD: usize = 10;

pub struct OpAllocReg {
    pub n_reg: usize,
}

impl Instruction for OpAllocReg {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        gc.alloc_reg_file(self.n_reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    N_REG#{}",
            "alloc", self.n_reg
        )
        .unwrap();
    }
}

pub struct OpCall {
    pub reg_id: usize,
    pub parameters: usize,
    pub start: usize,
    pub write_back: Option<usize>,
    pub loc: Loc,
}

impl Instruction for OpCall {
    #[inline(never)]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        // get closure
        let obj = gc.read_reg(self.reg_id).clone();
        match obj {
            Reg::Ref(r) => {
                let obj = unsafe { gc.get_obj_unchecked(r) };
                match obj {
                    GcObject::Closure {
                        func_id,
                        parameters,
                        ..
                    } => {
                        let func_id = *func_id;
                        let parameters = *parameters;
                        if parameters != self.parameters {
                            return Err(VmError::ParameterLengthNotMatch {
                                loc: self.loc.clone(),
                                expected: parameters,
                                got: self.parameters,
                            });
                        }

                        // allocate call stack
                        gc.alloc_call_stack(
                            Ip {
                                func_id: ip.func_id,
                                inst: ip.inst + 1,
                            },
                            self.write_back,
                            // Reg#0 is always unit type
                            self.start - 1,
                            r,
                        );

                        Ok(Ip { func_id, inst: 0 })
                    }
                    GcObject::NativeFunction(f) => {
                        let f = f.clone();
                        let f = f.borrow();
                        let mut parameters = vec![];
                        (self.start..self.start + self.parameters)
                            .into_iter()
                            .for_each(|i| {
                                let reg = gc.read_reg(i).clone();
                                parameters.push(reg);
                            });
                        let mut state = State { gc };
                        let ret = f(&mut state, &parameters, out).map_err(|s| VmError::Panic {
                            loc: self.loc.clone(),
                            reason: s,
                            notes: vec![],
                        })?;
                        match ret {
                            Reg::Str(id) => {
                                if gc.get_str(id).is_none() {
                                    return Err(VmError::InvalidRef {
                                        loc: self.loc.clone(),
                                        t: "Str",
                                        id,
                                    });
                                }
                            }
                            Reg::Ref(rid) => {
                                if gc.get_obj(rid).is_none() {
                                    return Err(VmError::InvalidRef {
                                        loc: self.loc.clone(),
                                        t: "Reference",
                                        id: rid,
                                    });
                                }
                            }
                            _ => (),
                        }
                        if let Some(write_back) = self.write_back {
                            gc.write_reg(write_back, ret)
                        }
                        Ok(Ip {
                            func_id: ip.func_id,
                            inst: ip.inst + 1,
                        })
                    }
                    _ => Err(()),
                }
            }
            _ => Err(()),
        }
        .map_err(|_| VmError::NotCallable(self.loc.clone(), get_type(&obj, gc)))
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} $ Reg#{}..{} -> {}",
            "call",
            self.reg_id,
            self.start,
            self.start + self.parameters,
            self.write_back
                .map(|x| x.to_string())
                .unwrap_or_else(|| "[[discard]]".to_string())
        )
        .unwrap()
    }
}

pub struct OpRet {
    pub return_reg: usize,
    pub loc: Loc,
}

impl Instruction for OpRet {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        _: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let reg = gc.read_reg(self.return_reg).clone();

        // clean call stack
        let (ip, write_back) = gc.pop_call_stack();

        // write return value back
        if let Some(write_back) = write_back {
            gc.write_reg(write_back, reg)
        }

        Ok(ip)
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{}",
            "ret", self.return_reg
        )
        .unwrap()
    }
}

pub struct OpMakeClosure {
    pub loc: Loc,
    /// closure function id
    pub func_id: usize,
    /// parameter len
    pub parameters: usize,
    /// write closure to target
    pub rd: usize,
    pub capture: Vec<Capture>,
    pub reg_size: usize,
}

impl Instruction for OpMakeClosure {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let mut captured_regs = vec![];

        for Capture { rd, rs, depth } in self.capture.iter() {
            let reg = gc.share_reg(*rs, *depth);
            captured_regs.push((*rd, reg));
        }

        let closure = GcObject::Closure {
            func_id: self.func_id,
            parameters: self.parameters,
            captured: captured_regs,
            reg_size: self.reg_size,
        };
        let reg = Reg::Ref(gc.alloc_obj(closure));
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Func@{} -> Reg#{}",
            "closure", self.func_id, self.rd
        )
        .unwrap();
        for Capture { rd, rs, depth } in self.capture.iter() {
            writeln!(
                decompiled,
                "    {: >FORMAT_PAD$}    Reg#{rd} <- Reg@-{depth}#{rs}",
                ""
            )
            .unwrap()
        }
    }
}

pub struct OpLoadConstant {
    pub constant: Reg,
    pub rd: usize,
}

impl Instruction for OpLoadConstant {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let reg = self.constant.clone();
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, gc: &Gc<Buffer>) {
        let content = match &self.constant {
            Reg::Unit => "()".to_string(),
            Reg::Bool(b) => b.to_string(),
            Reg::Int(i) => i.to_string(),
            Reg::Float(f) => f.to_string(),
            Reg::Str(sid) => format!("'{}'", gc.get_str(*sid).unwrap()),
            Reg::Ref(_) => unreachable!(),
        };
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    {} -> Reg#{}",
            "load", content, self.rd
        )
        .unwrap();
    }
}

pub struct OpNot {
    pub loc: Loc,
    pub lhs: usize,
    pub rd: usize,
}

impl Instruction for OpNot {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let reg = match lhs {
            Reg::Bool(b) => Reg::Bool(!b),
            _ => {
                let t = get_type(lhs, gc);
                return Err(VmError::OpPrefixNotApplicable(self.loc.clone(), "not", t));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} -> Reg#{}",
            "not", self.lhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpNeg {
    pub loc: Loc,
    pub lhs: usize,
    pub rd: usize,
}

impl Instruction for OpNeg {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let reg = match lhs {
            Reg::Int(i) => Reg::Int(-i),
            Reg::Float(f) => Reg::Float(-f),
            _ => {
                let t = get_type(lhs, gc);
                return Err(VmError::OpPrefixNotApplicable(self.loc.clone(), "-", t));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} -> Reg#{}",
            "neg", self.lhs, self.rd
        )
        .unwrap();
    }
}

pub struct OpAdd {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpAdd {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Int(i64::wrapping_add(*i1, *i2)),
            (Reg::Int(i1), Reg::Float(f2)) => Reg::Float(*i1 as f64 + *f2),
            (Reg::Float(f1), Reg::Int(i2)) => Reg::Float(*f1 + *i2 as f64),
            (Reg::Float(f1), Reg::Float(f2)) => Reg::Float(*f1 + *f2),
            (Reg::Str(s1), Reg::Str(s2)) => {
                let s1 = unsafe { gc.get_str_unchecked(*s1) };
                let s2 = unsafe { gc.get_str_unchecked(*s2) };
                let mut s = s1.to_string();
                s.push_str(s2);
                let sid_ret = gc.alloc_str(s);
                Reg::Str(sid_ret)
            }
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "+", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "add", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpSub {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpSub {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Int(i64::wrapping_sub(*i1, *i2)),
            (Reg::Int(i1), Reg::Float(f2)) => Reg::Float(*i1 as f64 - *f2),
            (Reg::Float(f1), Reg::Int(i2)) => Reg::Float(*f1 - *i2 as f64),
            (Reg::Float(f1), Reg::Float(f2)) => Reg::Float(*f1 - *f2),
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "-", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "sub", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpMul {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpMul {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Int(i64::wrapping_mul(*i1, *i2)),
            (Reg::Int(i1), Reg::Float(f2)) => Reg::Float(*i1 as f64 * *f2),
            (Reg::Float(f1), Reg::Int(i2)) => Reg::Float(*f1 * *i2 as f64),
            (Reg::Float(f1), Reg::Float(f2)) => Reg::Float(*f1 * *f2),
            (Reg::Str(s), Reg::Int(i)) => {
                let s = unsafe { gc.get_str_unchecked(*s) };
                let result = if *i > 0 {
                    s.repeat(*i as usize)
                } else {
                    String::new()
                };
                let id = gc.alloc_str(result);
                Reg::Str(id)
            }
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "*", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "mul", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpDiv {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpDiv {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Float(*i1 as f64 / *i2 as f64),
            (Reg::Int(i1), Reg::Float(f2)) => Reg::Float(*i1 as f64 / *f2),
            (Reg::Float(f1), Reg::Int(i2)) => Reg::Float(*f1 / *i2 as f64),
            (Reg::Float(f1), Reg::Float(f2)) => Reg::Float(*f1 / *f2),
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "/", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "div", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpIDiv {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpIDiv {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let result = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => *i1 as f64 / *i2 as f64,
            (Reg::Int(i1), Reg::Float(f2)) => *i1 as f64 / *f2,
            (Reg::Float(f1), Reg::Int(i2)) => *f1 / *i2 as f64,
            (Reg::Float(f1), Reg::Float(f2)) => *f1 / *f2,
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "//", t1, t2));
            }
        };
        let reg = Reg::Int(result.floor() as i64);
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "idiv", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpRem {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpRem {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Int(*i1 % *i2),
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "%", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "rem", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpPow {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpPow {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Float(f64::powf(*i1 as f64, *i2 as f64)),
            (Reg::Int(i1), Reg::Float(f2)) => Reg::Float(f64::powf(*i1 as f64, *f2)),
            (Reg::Float(f1), Reg::Int(i2)) => Reg::Float(f64::powf(*f1, *i2 as f64)),
            (Reg::Float(f1), Reg::Float(f2)) => Reg::Float(f64::powf(*f1, *f2)),
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "**", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "power", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpIndex {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpIndex {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Ref(rid), Reg::Int(idx)) => {
                let idx = *idx;
                if let GcObject::List(l) = unsafe { gc.get_obj_unchecked(*rid) } {
                    if (idx >= 0 && idx as usize >= l.len())
                        || (idx < 0 && idx.unsigned_abs() as usize > l.len())
                    {
                        return Err(VmError::IndexOutOfBound {
                            loc: self.loc.clone(),
                            bound: l.len(),
                            index: idx,
                        });
                    } else {
                        Ok(if idx >= 0 {
                            l[idx.unsigned_abs() as usize].clone()
                        } else {
                            l[l.len() - (idx.unsigned_abs() as usize)].clone()
                        })
                    }
                } else {
                    Err(())
                }
            }
            _ => Err(()),
        }
        .map_err(|_| VmError::CanNotIndex {
            loc: self.loc.clone(),
            t1: get_type(lhs, gc),
            t2: get_type(rhs, gc),
        })?;
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "power", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpIs {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpIs {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Ref(r1), Reg::Ref(r2)) => Reg::Bool(r1 == r2),
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "is", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "eq", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpEq {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpEq {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Unit, Reg::Unit) => Reg::Bool(true),
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Bool(*i1 == *i2),
            (Reg::Bool(b1), Reg::Bool(b2)) => Reg::Bool(*b1 == *b2),
            (Reg::Str(s1), Reg::Str(s2)) => {
                let s1 = gc.get_str(*s1);
                let s2 = gc.get_str(*s2);
                Reg::Bool(s1 == s2)
            }
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "==", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "eq", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpNe {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpNe {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Bool(*i1 != *i2),
            (Reg::Str(s1), Reg::Str(s2)) => {
                let s1 = unsafe { gc.get_str_unchecked(*s1) };
                let s2 = unsafe { gc.get_str_unchecked(*s2) };
                Reg::Bool(s1 != s2)
            }
            (Reg::Bool(b1), Reg::Bool(b2)) => Reg::Bool(*b1 != *b2),
            (Reg::Unit, Reg::Unit) => Reg::Bool(false),
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "<>", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "ne", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpLt {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpLt {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Bool(*i1 < *i2),
            (Reg::Int(i1), Reg::Float(f2)) => Reg::Bool((*i1 as f64) < *f2),
            (Reg::Float(f1), Reg::Int(i2)) => Reg::Bool(*f1 < *i2 as f64),
            (Reg::Float(f1), Reg::Float(f2)) => Reg::Bool(*f1 < *f2),
            (Reg::Str(s1), Reg::Str(s2)) => {
                let s1 = unsafe { gc.get_str_unchecked(*s1) };
                let s2 = unsafe { gc.get_str_unchecked(*s2) };
                Reg::Bool(s1 < s2)
            }
            (Reg::Bool(b1), Reg::Bool(b2)) => Reg::Bool(bool::lt(b1, b2)),
            (Reg::Unit, Reg::Unit) => Reg::Bool(false),
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "<", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "lt", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpLe {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpLe {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Bool(*i1 <= *i2),
            (Reg::Str(s1), Reg::Str(s2)) => {
                let s1 = unsafe { gc.get_str_unchecked(*s1) };
                let s2 = unsafe { gc.get_str_unchecked(*s2) };
                Reg::Bool(s1 <= s2)
            }
            (Reg::Bool(b1), Reg::Bool(b2)) => Reg::Bool(bool::le(b1, b2)),
            (Reg::Unit, Reg::Unit) => Reg::Bool(true),
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "<=", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "le", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpGt {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpGt {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Bool(*i1 > *i2),
            (Reg::Int(i1), Reg::Float(f2)) => Reg::Bool((*i1 as f64) > *f2),
            (Reg::Float(f1), Reg::Int(i2)) => Reg::Bool(*f1 > *i2 as f64),
            (Reg::Float(f1), Reg::Float(f2)) => Reg::Bool(*f1 > *f2),
            (Reg::Str(s1), Reg::Str(s2)) => {
                let s1 = unsafe { gc.get_str_unchecked(*s1) };
                let s2 = unsafe { gc.get_str_unchecked(*s2) };
                Reg::Bool(s1 > s2)
            }
            (Reg::Bool(b1), Reg::Bool(b2)) => Reg::Bool(bool::gt(b1, b2)),
            (Reg::Unit, Reg::Unit) => Reg::Bool(false),
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), ">", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "gt", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpGe {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpGe {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Bool(*i1 >= *i2),
            (Reg::Str(s1), Reg::Str(s2)) => {
                let s1 = unsafe { gc.get_str_unchecked(*s1) };
                let s2 = unsafe { gc.get_str_unchecked(*s2) };
                Reg::Bool(s1 >= s2)
            }
            (Reg::Bool(b1), Reg::Bool(b2)) => Reg::Bool(bool::ge(b1, b2)),
            (Reg::Unit, Reg::Unit) => Reg::Bool(true),
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), ">=", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "ge", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpAnd {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpAnd {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Bool(b1), Reg::Bool(b2)) => Reg::Bool(*b1 && *b2),
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "and", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "and", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpOr {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpOr {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Bool(b1), Reg::Bool(b2)) => Reg::Bool(*b1 || *b2),
            _ => {
                let t1 = get_type(lhs, gc);
                let t2 = get_type(rhs, gc);
                return Err(VmError::OpBinNotApplicable(self.loc.clone(), "or", t1, t2));
            }
        };
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "or", self.lhs, self.rhs, self.rd
        )
        .unwrap()
    }
}

pub struct OpMove {
    pub rs: usize,
    pub rd: usize,
}

impl Instruction for OpMove {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let rs = gc.read_reg(self.rs).clone();
        gc.write_reg(self.rd, rs);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} -> Reg#{}",
            "mov", self.rs, self.rd
        )
        .unwrap()
    }
}

pub struct OpBranchTrue {
    pub loc: Loc,
    pub condition: usize,
    pub offset: i64,
}

impl Instruction for OpBranchTrue {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let condition = gc.read_reg(self.condition);
        if let Reg::Bool(b) = condition {
            Ok(if *b {
                Ip {
                    func_id: ip.func_id,
                    inst: (ip.inst as i64 + self.offset) as usize,
                }
            } else {
                Ip {
                    func_id: ip.func_id,
                    inst: ip.inst + 1,
                }
            })
        } else {
            let t = get_type(condition, gc);
            Err(VmError::InvalidCondition(self.loc.clone(), t))
        }
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} => {:+}",
            "br_true", self.condition, self.offset
        )
        .unwrap()
    }
}

pub struct OpBranchFalse {
    pub loc: Loc,
    pub condition: usize,
    pub offset: i64,
}

impl Instruction for OpBranchFalse {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let condition = gc.read_reg(self.condition);
        if let Reg::Bool(b) = condition {
            Ok(if !*b {
                Ip {
                    func_id: ip.func_id,
                    inst: (ip.inst as i64 + self.offset) as usize,
                }
            } else {
                Ip {
                    func_id: ip.func_id,
                    inst: ip.inst + 1,
                }
            })
        } else {
            let t = get_type(condition, gc);
            Err(VmError::InvalidCondition(self.loc.clone(), t))
        }
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} => {:+}",
            "br_false", self.condition, self.offset
        )
        .unwrap()
    }
}

pub struct OpJump {
    pub loc: Loc,
    pub offset: i64,
}

impl Instruction for OpJump {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        _gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        Ok(Ip {
            func_id: ip.func_id,
            inst: (ip.inst as i64 + self.offset) as usize,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(decompiled, "{: >FORMAT_PAD$}    {}", "jump", self.offset).unwrap()
    }
}

pub struct OpDummy;

impl Instruction for OpDummy {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        _ip: Ip,
        _gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        // This instruction will never be executed
        unreachable!()
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(decompiled, "{: >FORMAT_PAD$}   ", "dummy").unwrap()
    }
}

/// Pause execution and yield control
pub struct OpYield {
    pub show_id: Option<usize>,
}

impl Instruction for OpYield {
    #[cfg_attr(feature = "profile", inline(never))]
    fn exec<Buffer: IoWrite>(
        &self,
        _ip: Ip,
        _gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        Err(VmError::Yield(self.show_id))
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    {}",
            "yield",
            if let Some(id) = self.show_id {
                format!("Reg#{id}")
            } else {
                String::new()
            }
        )
        .unwrap()
    }
}

pub struct OpSetTuple {
    pub loc: Loc,
    pub rs: usize,
    pub rd: usize,
    pub idx: usize,
}

impl Instruction for OpSetTuple {
    #[inline(never)]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let target = gc.read_reg(self.rs).clone();
        let table = gc.read_reg(self.rd).clone();
        match table {
            Reg::Ref(r) => match unsafe { gc.get_obj_unchecked_mut(r) } {
                GcObject::Tuple(t) => {
                    let bound = t.len();
                    let item = t
                        .get_mut(self.idx)
                        .ok_or_else(|| VmError::TupleOutOfBound {
                            loc: self.loc.clone(),
                            bound,
                            access: self.idx,
                        })?;
                    *item = target;
                    Ok(())
                }
                _ => Err(()),
            },
            _ => Err(()),
        }
        .map_err(|_| {
            let reg = gc.read_reg(self.rd);
            let t = get_type(reg, gc);
            VmError::NotATuple {
                loc: self.loc.clone(),
                t,
            }
        })?;
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}   Reg#{} -> Reg#{}.{}",
            "set_tuple", self.rs, self.rd, self.idx
        )
        .unwrap()
    }
}

pub struct OpSetTable {
    pub loc: Loc,
    pub rs: usize,
    pub rd: usize,
    pub attr: usize,
}

impl Instruction for OpSetTable {
    #[inline(never)]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let target = gc.read_reg(self.rs).clone();
        let table = gc.read_reg(self.rd).clone();
        match table {
            Reg::Ref(r) => match unsafe { gc.get_obj_unchecked_mut(r) } {
                GcObject::Table(t) => {
                    t.attributes.insert(self.attr, target);
                    Ok(())
                }
                _ => Err(()),
            },
            _ => Err(()),
        }
        .map_err(|_| {
            let reg = gc.read_reg(self.rd);
            let t = get_type(reg, gc);
            VmError::NotATable {
                loc: self.loc.clone(),
                t,
            }
        })?;
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}   Reg#{} -> Reg#{}.{}",
            "set_table",
            self.rs,
            self.rd,
            gc.look_up_table_key(self.attr).unwrap()
        )
        .unwrap()
    }
}

pub struct OpGetTable {
    pub loc: Loc,
    pub rs: usize,
    pub rd: usize,
    pub attr: usize,
}

impl Instruction for OpGetTable {
    #[inline(never)]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let table = gc.read_reg(self.rs);
        if let Reg::Ref(rid) = table {
            let rid = *rid;
            match unsafe { gc.get_obj_unchecked(rid) } {
                GcObject::Table(t) => {
                    let value = t
                        .attributes
                        .get(&self.attr)
                        .or_else(|| {
                            t.meta_table.and_then(|meta_table| {
                                let meta_table = unsafe { gc.get_obj_unchecked(meta_table) };
                                let meta_table = if let GcObject::Table(t) = meta_table {
                                    t.attributes.get(&self.attr)
                                } else {
                                    unreachable!()
                                };
                                meta_table
                            })
                        })
                        .ok_or(VmError::NoSuchKey {
                            loc: self.loc.clone(),
                            attr: gc.look_up_table_key(self.attr).unwrap().to_string(),
                        })?
                        .clone();
                    gc.write_reg(self.rd, value);
                    return Ok(Ip {
                        func_id: ip.func_id,
                        inst: ip.inst + 1,
                    });
                }
                GcObject::List(_) => {
                    let meta = unsafe { gc.get_obj_unchecked(gc.list_meta()) };
                    if let GcObject::Table(t) = meta {
                        let value = t
                            .attributes
                            .get(&self.attr)
                            .ok_or(VmError::NoSuchKey {
                                loc: self.loc.clone(),
                                attr: gc.look_up_table_key(self.attr).unwrap().to_string(),
                            })?
                            .clone();
                        gc.write_reg(self.rd, value);
                        return Ok(Ip {
                            func_id: ip.func_id,
                            inst: ip.inst + 1,
                        });
                    } else {
                        unreachable!()
                    }
                }
                _ => (),
            }
        }

        match table {
            Reg::Int(_) => {
                let meta = unsafe { gc.get_obj_unchecked(gc.int_meta()) };
                if let GcObject::Table(t) = meta {
                    let value = t
                        .attributes
                        .get(&self.attr)
                        .ok_or(VmError::NoSuchKey {
                            loc: self.loc.clone(),
                            attr: gc.look_up_table_key(self.attr).unwrap().to_string(),
                        })?
                        .clone();
                    gc.write_reg(self.rd, value);
                    return Ok(Ip {
                        func_id: ip.func_id,
                        inst: ip.inst + 1,
                    });
                } else {
                    unreachable!()
                }
            }
            Reg::Float(_) => {
                let meta = unsafe { gc.get_obj_unchecked(gc.float_meta()) };
                if let GcObject::Table(t) = meta {
                    let value = t
                        .attributes
                        .get(&self.attr)
                        .ok_or(VmError::NoSuchKey {
                            loc: self.loc.clone(),
                            attr: gc.look_up_table_key(self.attr).unwrap().to_string(),
                        })?
                        .clone();
                    gc.write_reg(self.rd, value);
                    return Ok(Ip {
                        func_id: ip.func_id,
                        inst: ip.inst + 1,
                    });
                } else {
                    unreachable!()
                }
            }
            _ => (),
        }

        let t = get_type(table, gc);
        Err(VmError::NotATable {
            loc: self.loc.clone(),
            t,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}   Reg#{}.{} -> Reg#{}",
            "get_table",
            self.rs,
            gc.look_up_table_key(self.attr).unwrap(),
            self.rd
        )
        .unwrap()
    }
}

pub struct OpGetTuple {
    pub loc: Loc,
    pub rs: usize,
    pub rd: usize,
    pub idx: usize,
}

impl Instruction for OpGetTuple {
    #[inline(never)]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let table = gc.read_reg(self.rs);
        if let Reg::Ref(rid) = table {
            let rid = *rid;
            if let GcObject::Tuple(t) = unsafe { gc.get_obj_unchecked(rid) } {
                let value = t
                    .get(self.idx)
                    .ok_or(VmError::TupleOutOfBound {
                        loc: self.loc.clone(),
                        bound: t.len(),
                        access: self.idx,
                    })?
                    .clone();
                gc.write_reg(self.rd, value);
                return Ok(Ip {
                    func_id: ip.func_id,
                    inst: ip.inst + 1,
                });
            }
        }

        let t = get_type(table, gc);
        Err(VmError::NotATuple {
            loc: self.loc.clone(),
            t,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}   Reg#{}.{} -> Reg#{}",
            "get_tuple", self.rs, self.idx, self.rd
        )
        .unwrap()
    }
}

pub struct OpMakeTable {
    pub rd: usize,
}

impl Instruction for OpMakeTable {
    #[inline(never)]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let table = gc.alloc_obj(GcObject::Table(Table {
            attributes: BTreeMap::new(),
            meta_table: None,
        }));
        let table = Reg::Ref(table);
        gc.write_reg(self.rd, table);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}   Reg#{}",
            "new_table", self.rd
        )
        .unwrap()
    }
}

pub struct OpMakeTuple {
    pub rd: usize,
    pub size: usize,
}

impl Instruction for OpMakeTuple {
    #[inline(never)]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let tuple = gc.alloc_obj(GcObject::Tuple(vec![Reg::Unit; self.size]));
        let tuple = Reg::Ref(tuple);
        gc.write_reg(self.rd, tuple);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}   size@{} -> Reg#{}",
            "new_tuple", self.size, self.rd
        )
        .unwrap()
    }
}

pub struct OpMakeList {
    pub rd: usize,
    pub items: Vec<usize>,
}

impl Instruction for OpMakeList {
    #[inline(never)]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let mut list = vec![];
        self.items
            .iter()
            .for_each(|reg_id| list.push(gc.read_reg(*reg_id).clone()));

        let list = gc.alloc_obj(GcObject::List(list));
        let list = Reg::Ref(list);
        gc.write_reg(self.rd, list);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}   [] -> Reg#{}",
            "new_tuple", self.rd
        )
        .unwrap()
    }
}

pub struct OpSetMeta {
    pub rs: usize,
    pub rd: usize,
    pub loc: Loc,
}

impl Instruction for OpSetMeta {
    #[inline(never)]
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let rs = gc.read_reg(self.rs);
        let rs = match gc.read_reg(self.rs) {
            Reg::Ref(rid) => {
                if matches!(unsafe { gc.get_obj_unchecked(*rid) }, GcObject::Table(_)) {
                    Ok(*rid)
                } else {
                    Err(())
                }
            }
            _ => Err(()),
        }
        .map_err(|_| VmError::InvalidMetaTable {
            loc: self.loc.clone(),
            t: get_type(rs, gc),
        })?;
        match gc.read_reg(self.rd) {
            Reg::Ref(rid) => match unsafe { gc.get_obj_unchecked_mut(*rid) } {
                GcObject::Table(t) => {
                    t.meta_table = Some(rs);
                }
                _ => unreachable!(),
            },
            _ => {
                unreachable!()
            }
        };
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} <- Reg#{}",
            "set_meta", self.rd, self.rs
        )
        .unwrap()
    }
}
