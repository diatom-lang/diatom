use ahash::AHashMap;

use crate::{
    diagnostic::Loc,
    interpreter::{
        gc::{Gc, GcObject, Reg, Table},
        Capture,
    },
    IoWrite, State,
};
use std::{cell::UnsafeCell, fmt::Write, rc::Rc};

use super::{Instruction, Ip, VmError};

fn get_type<Buffer: IoWrite>(reg: &Reg, gc: &Gc<Buffer>) -> String {
    match reg {
        Reg::Unit => "()".to_string(),
        Reg::Bool(_) => "Bool".to_string(),
        Reg::Int(_) => "Int".to_string(),
        Reg::Float(_) => "Float".to_string(),
        Reg::Str(_) => "String".to_string(),
        Reg::Ref(r) => {
            let obj = &gc[*r];
            match obj {
                GcObject::Closure {
                    func_id: _,
                    parameters: _,
                    captured: _,
                    reg_size: _,
                } => "Closure".to_string(),
                GcObject::NativeFunction(_) => "Extern_Function".to_string(),
                GcObject::Table(_) => "Table".to_string(),
            }
        }
    }
}

const FORMAT_PAD: usize = 10;

pub struct OpAllocReg {
    pub n_reg: usize,
}

impl Instruction for OpAllocReg {
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
    pub parameters: Vec<usize>,
    pub write_back: Option<usize>,
    pub loc: Loc,
}

impl Instruction for OpCall {
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
                let obj = &gc[r];
                match obj {
                    GcObject::Closure {
                        func_id,
                        parameters,
                        captured,
                        reg_size,
                    } => {
                        let func_id = *func_id;
                        let parameters = *parameters;
                        let captured = captured as *const Vec<(usize, Rc<UnsafeCell<Reg>>)>;
                        let reg_size = *reg_size;
                        if parameters != self.parameters.len() {
                            return Err(VmError::ParameterLengthNotMatch {
                                loc: self.loc.clone(),
                                expected: parameters,
                                got: self.parameters.len(),
                            });
                        }

                        // allocate call stack
                        gc.alloc_call_stack(
                            Ip {
                                func_id: ip.func_id,
                                inst: ip.inst + 1,
                            },
                            self.write_back,
                        );

                        // alloc reg file
                        gc.alloc_reg_file(reg_size);

                        // write parameters to call stack
                        self.parameters
                            .iter()
                            .enumerate()
                            .for_each(|(i, reg_prev)| {
                                let reg = gc.read_reg_prev(*reg_prev).clone();
                                gc.write_reg(i, reg);
                            });

                        // write capture values to stack
                        // This operation violets borrow rules but since closure can not be modified or garbaged
                        // collected here. It is safe to dereference the pointer here.
                        unsafe { &*captured }
                            .iter()
                            .for_each(|(rd, reg)| gc.write_shared_reg(*rd, reg.clone()));

                        Ok(Ip { func_id, inst: 0 })
                    }
                    GcObject::NativeFunction(f) => {
                        let f = f.clone();
                        let f = f.borrow();
                        let mut parameters = vec![];
                        for para in self.parameters.iter() {
                            let reg = gc.read_reg(*para).clone();
                            parameters.push(reg);
                        }
                        let mut state = State { gc };
                        let ret = f(&mut state, &parameters, out).map_err(|s| VmError::Panic {
                            loc: self.loc.clone(),
                            reason: s,
                            notes: vec![],
                        })?;
                        match ret {
                            Reg::Str(id) => {
                                if gc.get_string_by_id_checked(id).is_none() {
                                    return Err(VmError::InvalidRef {
                                        loc: self.loc.clone(),
                                        t: "Str",
                                        id,
                                    });
                                }
                            }
                            Reg::Ref(_) => todo!(),
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
        let mut parameters = self.parameters.iter().fold(String::new(), |mut s, x| {
            s.push_str(&format!("{x}, "));
            s
        });
        parameters.pop();
        parameters.pop();
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} $ Reg#{{{}}} -> {}",
            "call",
            self.reg_id,
            parameters,
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
    fn exec<Buffer: IoWrite>(
        &self,
        _: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let reg = gc.read_reg(self.return_reg).clone();

        // clean call stack
        let (ip, write_back) = gc.pop_call_stack().expect("Return on empty call stack!");

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
        let reg = Reg::Ref(gc.alloc(closure));
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
            "clos", self.func_id, self.rd
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
    pub loc: Loc,
}

impl Instruction for OpLoadConstant {
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
            Reg::Str(sid) => format!("'{}'", gc.get_string_by_id(*sid)),
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

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, __gc: &Gc<Buffer>) {
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
                let str1 = gc.get_string_by_id(*s1);
                let str2 = gc.get_string_by_id(*s2);
                let mut s = str1.to_string();
                s.push_str(str2);
                let sid_ret = gc.string_pool().alloc(s);
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
                let s = gc.get_string_by_id(*s);
                let result = if *i > 0 {
                    s.repeat(*i as usize)
                } else {
                    String::new()
                };
                let id = gc.string_pool().alloc(result);
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

pub struct OpEq {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpEq {
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
                let s1 = gc.get_string_by_id(*s1);
                let s2 = gc.get_string_by_id(*s2);
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
                let s1 = gc.get_string_by_id(*s1);
                let s2 = gc.get_string_by_id(*s2);
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
                let s1 = gc.get_string_by_id(*s1);
                let s2 = gc.get_string_by_id(*s2);
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
                let s1 = gc.get_string_by_id(*s1);
                let s2 = gc.get_string_by_id(*s2);
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
                let s1 = gc.get_string_by_id(*s1);
                let s2 = gc.get_string_by_id(*s2);
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
                let s1 = gc.get_string_by_id(*s1);
                let s2 = gc.get_string_by_id(*s2);
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
    pub loc: Loc,
    pub rs: usize,
    pub rd: usize,
}

impl Instruction for OpMove {
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
            "bt", self.condition, self.offset
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
            "bf", self.condition, self.offset
        )
        .unwrap()
    }
}

pub struct OpJump {
    pub loc: Loc,
    pub offset: i64,
}

impl Instruction for OpJump {
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

pub struct OpSetAttr {
    pub loc: Loc,
    pub rs: usize,
    pub rd: usize,
    pub attrs: Vec<String>,
}

impl Instruction for OpSetAttr {
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let target = gc.read_reg(self.rs).clone();
        debug_assert!(!self.attrs.is_empty());
        let mut table = gc.read_reg(self.rd).clone();
        for (i, attr) in self.attrs.iter().enumerate() {
            match table {
                Reg::Ref(r) => match &mut gc[r] {
                    GcObject::Table(t) => {
                        if i == self.attrs.len() - 1 {
                            t.attributes.insert(attr.clone(), target.clone());
                        } else {
                            table = t
                                .attributes
                                .get(attr)
                                .ok_or_else(|| VmError::NoSuchKey {
                                    loc: self.loc.clone(),
                                    attr: attr.clone(),
                                })?
                                .clone();
                        }
                        Ok(())
                    }
                    _ => Err(()),
                },
                _ => Err(()),
            }
            .map_err(|_| {
                let reg = gc.read_reg(self.rd);
                let t = get_type(reg, gc);
                VmError::CanNotSetAttr {
                    loc: self.loc.clone(),
                    t,
                }
            })?;
        }
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        let attr = self.attrs.iter().fold(String::new(), |mut acc, s| {
            write!(acc, ".{s}").unwrap();
            acc
        });
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}   Reg#{} -> Reg#{}{}",
            "set_attr", self.rs, self.rd, attr
        )
        .unwrap()
    }
}

pub struct OpGetAttr {
    pub loc: Loc,
    pub rs: usize,
    pub rd: usize,
    pub attr: String,
}

impl Instruction for OpGetAttr {
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let table = gc.read_reg(self.rs);
        if let Reg::Ref(rid) = table {
            let rid = *rid;
            match &gc[rid] {
                GcObject::Closure { .. } => (),
                GcObject::NativeFunction(_) => (),
                GcObject::Table(t) => {
                    let value = t
                        .attributes
                        .get(&self.attr)
                        .ok_or(VmError::NoSuchKey {
                            loc: self.loc.clone(),
                            attr: self.attr.clone(),
                        })?
                        .clone();
                    gc.write_reg(self.rd, value);
                    return Ok(Ip {
                        func_id: ip.func_id,
                        inst: ip.inst + 1,
                    });
                }
            }
        }

        let t = get_type(table, gc);
        Err(VmError::NotATable {
            loc: self.loc.clone(),
            t,
        })
    }

    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, _gc: &Gc<Buffer>) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}   Reg#{}.{} -> Reg#{}",
            "set_attr", self.rs, self.attr, self.rd
        )
        .unwrap()
    }
}

pub struct OpMakeTable {
    pub rd: usize,
}

impl Instruction for OpMakeTable {
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        _out: &mut Buffer,
    ) -> Result<Ip, VmError> {
        let table = gc.alloc(GcObject::Table(Table {
            attributes: AHashMap::new(),
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
