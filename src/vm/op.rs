use crate::{diagnostic::Loc, interpreter::Capture, State};
use std::fmt::Write;

use super::{
    gc::{Gc, GcObject, Reg},
    FuncId, Instruction, Ip, Vm, VmError,
};

fn get_type(reg: &Reg, gc: &Gc) -> String {
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
                GcObject::_Object(_) => "Table".to_string(),
            }
        }
    }
}

const FORMAT_PAD: usize = 7;

pub struct OpAllocReg {
    pub n_reg: usize,
}

impl Instruction for OpAllocReg {
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        context.gc.alloc_reg_file(self.n_reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    N_REG#{}",
            "alloc", self.n_reg
        )
        .unwrap();
    }
}

pub struct OpCallClosure {
    pub reg_id: usize,
    pub parameters: Vec<usize>,
    pub write_back: Option<usize>,
    pub loc: Loc,
}

impl Instruction for OpCallClosure {
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;

        // get closure
        let obj = gc.read_reg(self.reg_id);
        let (func_id, parameters, capture, reg_size) = match obj {
            Reg::Ref(r) => {
                let obj = &gc[*r];
                match obj {
                    GcObject::Closure {
                        func_id,
                        parameters,
                        captured,
                        reg_size,
                    } => Ok((*func_id, *parameters, captured.to_vec(), *reg_size)),
                    GcObject::NativeFunction(f) => {
                        let f = f.clone();
                        let f = f.borrow();
                        let mut parameters = vec![];
                        for para in self.parameters.iter() {
                            let reg = gc.read_reg(*para).clone();
                            parameters.push(reg);
                        }
                        let mut state = State { vm: context };
                        let ret = f(&mut state, &parameters).map_err(|s| VmError::Panic {
                            loc: self.loc.clone(),
                            reason: s,
                            notes: vec![],
                        })?;
                        match ret {
                            Reg::Str(id) => {
                                if context.gc.get_string_by_id_checked(id).is_none() {
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
                            context.gc.write_reg(write_back, ret)
                        }
                        return Ok(Ip {
                            func_id: ip.func_id,
                            inst: ip.inst + 1,
                        });
                    }
                    _ => Err(()),
                }
            }
            _ => Err(()),
        }
        .map_err(|_| VmError::NotCallable(self.loc.clone(), get_type(obj, gc)))?;

        if parameters != self.parameters.len() {
            return Err(VmError::ParameterLengthNotMatch {
                loc: self.loc.clone(),
                expected: parameters,
                got: self.parameters.len(),
            });
        }

        // clone parameters
        let mut parameters = vec![];
        for para in self.parameters.iter() {
            let reg = gc.read_reg(*para).clone();
            parameters.push(reg);
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
        parameters
            .into_iter()
            .enumerate()
            .for_each(|(i, reg)| gc.write_reg(i, reg));

        // write capture values to stack
        capture
            .into_iter()
            .for_each(|(rd, reg)| gc.write_shared_reg(rd, reg));

        Ok(Ip { func_id, inst: 0 })
    }

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
        let parameters = self.parameters.iter().fold(String::new(), |mut s, x| {
            s.push_str(&format!("{x}, "));
            s
        });
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
    fn exec(&self, _: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let reg = gc.read_reg(self.return_reg).clone();

        // clean call stack
        let (ip, write_back) = gc.pop_call_stack().expect("Return on empty call stack!");

        // write return value back
        if let Some(write_back) = write_back {
            gc.write_reg(write_back, reg)
        }

        Ok(ip)
    }

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
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
    pub func_id: FuncId,
    /// parameter len
    pub parameters: usize,
    /// write closure to target
    pub rd: usize,
    pub capture: Vec<Capture>,
    pub reg_size: usize,
}

impl Instruction for OpMakeClosure {
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let reg = self.constant.clone();
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile(&self, decompiled: &mut String, context: &Vm) {
        let content = match &self.constant {
            Reg::Unit => "()".to_string(),
            Reg::Bool(b) => b.to_string(),
            Reg::Int(i) => i.to_string(),
            Reg::Float(f) => f.to_string(),
            Reg::Str(sid) => context.gc.get_string_by_id(*sid).to_string(),
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, __context: &Vm) {
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "eq", self.lhs, self.rhs, self.rd
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Unit, Reg::Unit) => Reg::Bool(false),
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Bool(*i1 > *i2),
            (Reg::Int(i1), Reg::Float(f2)) => Reg::Bool((*i1 as f64) > *f2),
            (Reg::Float(f1), Reg::Int(i2)) => Reg::Bool(*f1 > *i2 as f64),
            (Reg::Float(f1), Reg::Float(f2)) => Reg::Bool(*f1 > *f2),
            (Reg::Bool(b1), Reg::Bool(b2)) => Reg::Bool(bool::gt(b1, b2)),
            (Reg::Str(s1), Reg::Str(s2)) => {
                let s1 = gc.get_string_by_id(*s1);
                let s2 = gc.get_string_by_id(*s2);
                Reg::Bool(s1 > s2)
            }
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} Reg#{} -> Reg#{}",
            "gt", self.lhs, self.rhs, self.rd
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
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
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let rs = gc.read_reg(self.rs).clone();
        gc.write_reg(self.rd, rs);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} -> Reg#{}",
            "mov", self.rs, self.rd
        )
        .unwrap()
    }
}

pub struct OpBranch {
    pub loc: Loc,
    pub condition: usize,
    pub offset: i64,
}

impl Instruction for OpBranch {
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
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

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
        writeln!(
            decompiled,
            "{: >FORMAT_PAD$}    Reg#{} => {:+}",
            "branch", self.condition, self.offset
        )
        .unwrap()
    }
}

pub struct OpJump {
    pub loc: Loc,
    pub offset: i64,
}

impl Instruction for OpJump {
    fn exec(&self, ip: Ip, _context: &mut Vm) -> Result<Ip, VmError> {
        Ok(Ip {
            func_id: ip.func_id,
            inst: (ip.inst as i64 + self.offset) as usize,
        })
    }

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
        writeln!(decompiled, "{: >FORMAT_PAD$}    {}", "jump", self.offset).unwrap()
    }
}

pub struct OpDummy;

impl Instruction for OpDummy {
    fn exec(&self, ip: Ip, _context: &mut Vm) -> Result<Ip, VmError> {
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
        writeln!(decompiled, "{: >FORMAT_PAD$}   ", "dummy").unwrap()
    }
}

/// Print register to output and stop execution
pub struct OpYield {
    pub show_id: Option<usize>,
}

impl Instruction for OpYield {
    fn exec(&self, _ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        if !context.is_repl {
            return Err(VmError::Yield);
        }
        if let Some(id) = self.show_id {
            let gc = &context.gc;
            let reg = gc.read_reg(id);
            let mut output = match reg {
                // omit unit output in repl
                Reg::Unit => String::new(),
                Reg::Bool(b) => format!("{b}"),
                Reg::Int(i) => format!("{i}"),
                Reg::Float(f) => format!("{f}"),
                Reg::Str(sid) => gc.get_string_by_id(*sid).to_string(),
                Reg::Ref(r) => format!("Object@<{r}>"),
            };
            if !output.is_empty() {
                output.push('\n')
            };
            context.output.push_str(&output)
        }
        Err(VmError::Yield)
    }

    fn decompile(&self, decompiled: &mut String, _context: &Vm) {
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
