use crate::diagnostic::Loc;

use super::{
    gc::{Gc, GcObject, Reg},
    Func, FuncId, Instruction, Ip, Vm, VmError,
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
                GcObject::Closure(_, _) => "Closure".to_string(),
                GcObject::_Object(_) => "Table".to_string(),
            }
        }
    }
}

pub struct OpCallClosure {
    pub reg_id: usize,
    pub parameters: Vec<usize>,
    pub write_back: Option<usize>,
    pub loc: Loc,
}

impl Instruction for OpCallClosure {
    fn exec(&self, ip: Ip, context: &mut Vm, byte_code: &[Func]) -> Result<Ip, VmError> {
        let gc = &mut context.gc;

        // get closure
        let obj = gc.read_reg(self.reg_id);
        let (func_id, capture) = match obj {
            Reg::Ref(r) => {
                let obj = &gc[*r];
                match obj {
                    GcObject::Closure(func_id, capture) => Ok((func_id, capture)),
                    _ => Err(()),
                }
            }
            _ => Err(()),
        }
        .map_err(|_| {
            VmError::NotCallable(self.loc.clone(), "Object is not callable".to_string())
        })?;

        let func_id = *func_id;
        let capture_owned: Vec<_> = capture.to_vec();

        let Func {
            name: _,
            parameters,
            insts: _,
            captures,
        } = byte_code
            .get(func_id)
            .ok_or_else(|| VmError::InvalidFunc(self.loc.clone(), func_id))?;

        if *parameters != self.parameters.len() {
            return Err(VmError::ParameterLengthNotMatch(
                self.loc.clone(),
                *parameters,
                self.parameters.len(),
            ));
        }

        // clone parameters
        let mut parameters = vec![];
        for para in self.parameters.iter() {
            let reg = gc.read_reg(*para);
            let reg = gc.clone_reg(reg);
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

        // write parameters to call stack
        parameters
            .into_iter()
            .enumerate()
            .for_each(|(i, reg)| gc.write_reg(i, reg));

        // write capture values to stack
        capture_owned
            .into_iter()
            .zip(captures.iter())
            .for_each(|(reg, rd)| gc.write_shared_reg(*rd, reg));

        Ok(Ip { func_id, inst: 0 })
    }
}

pub struct OpRet {
    pub return_reg: usize,
    pub loc: Loc,
}

impl Instruction for OpRet {
    fn exec(&self, _: Ip, context: &mut Vm, _byte_code: &[Func]) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let reg = gc.read_reg(self.return_reg);
        let reg = gc.clone_reg(reg);

        // clean call stack
        let (ip, write_back) = gc.pop_call_stack().expect("Return on empty call stack!");

        // write return value back
        if let Some(write_back) = write_back {
            gc.write_reg(write_back, reg)
        }

        Ok(ip)
    }
}

pub struct OpMakeClosure {
    pub loc: Loc,
    /// closure function id
    pub func_id: FuncId,
    /// write closure to target
    pub rd: usize,
    /// capture <reg id>
    pub capture: Vec<usize>,
}

impl Instruction for OpMakeClosure {
    fn exec(&self, ip: Ip, context: &mut Vm, _byte_code: &[Func]) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let mut captured_regs = vec![];

        for rs in self.capture.iter() {
            let reg = gc.share_reg(*rs);
            captured_regs.push(reg);
        }

        let closure = GcObject::Closure(self.func_id, captured_regs);
        let reg = Reg::Ref(gc.alloc(closure));
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }
}

pub struct OpLoadConstant {
    pub constant: Reg,
    pub rd: usize,
    pub loc: Loc,
}

impl Instruction for OpLoadConstant {
    fn exec(&self, ip: Ip, context: &mut Vm, _byte_code: &[Func]) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let reg = gc.clone_reg(&self.constant);
        gc.write_reg(self.rd, reg);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }
}

pub struct OpPanic {
    pub loc: Loc,
    pub reason: String,
}

impl Instruction for OpPanic {
    fn exec(&self, _ip: Ip, _context: &mut Vm, _byte_code: &[Func]) -> Result<Ip, VmError> {
        Err(VmError::Panic(self.loc.clone(), self.reason.clone()))
    }
}

pub struct OpNot {
    pub loc: Loc,
    pub lhs: usize,
    pub rd: usize,
}

impl Instruction for OpNot {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
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
}

pub struct OpNeg {
    pub loc: Loc,
    pub lhs: usize,
    pub rd: usize,
}

impl Instruction for OpNeg {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
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
}

pub struct OpAdd {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpAdd {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Int(i64::wrapping_add(*i1, *i2)),
            (Reg::Int(i1), Reg::Float(f2)) => Reg::Float(*i1 as f64 + *f2),
            (Reg::Float(f1), Reg::Int(i2)) => Reg::Float(*f1 + *i2 as f64),
            (Reg::Float(f1), Reg::Float(f2)) => Reg::Float(*f1 + *f2),
            (Reg::Str(s1), Reg::Str(s2)) => {
                let str2 = gc.get_string_by_id(s2);
                let s1 = gc.string_pool().clone_str(s1);
                let sid_ret = gc.string_pool().modify_str(s1, |s| s.push_str(str2));
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
}

pub struct OpSub {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpSub {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
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
}

pub struct OpMul {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpMul {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Int(i64::wrapping_mul(*i1, *i2)),
            (Reg::Int(i1), Reg::Float(f2)) => Reg::Float(*i1 as f64 * *f2),
            (Reg::Float(f1), Reg::Int(i2)) => Reg::Float(*f1 * *i2 as f64),
            (Reg::Float(f1), Reg::Float(f2)) => Reg::Float(*f1 * *f2),
            (Reg::Str(s), Reg::Int(i)) => {
                let s = gc.string_pool().clone_str(s);
                let sid_ret = gc.string_pool().modify_str(s, |s| {
                    if *i > 0 {
                        let original = s.clone();
                        (2..=*i).into_iter().for_each(|_| s.push_str(&original));
                    } else {
                        s.clear()
                    }
                });
                Reg::Str(sid_ret)
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
}

pub struct OpDiv {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpDiv {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
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
}

pub struct OpIDiv {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpIDiv {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
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
}

pub struct OpRem {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpRem {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
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
}

pub struct OpPow {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpPow {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
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
}

pub struct OpEq {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpEq {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Unit, Reg::Unit) => Reg::Bool(true),
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Bool(*i1 == *i2),
            (Reg::Bool(b1), Reg::Bool(b2)) => Reg::Bool(*b1 == *b2),
            (Reg::Str(s1), Reg::Str(s2)) => {
                let s1 = gc.get_string_by_id(s1);
                let s2 = gc.get_string_by_id(s2);
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
}

pub struct OpLt {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpLt {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let lhs = gc.read_reg(self.lhs);
        let rhs = gc.read_reg(self.rhs);
        let reg = match (lhs, rhs) {
            (Reg::Unit, Reg::Unit) => Reg::Bool(false),
            (Reg::Int(i1), Reg::Int(i2)) => Reg::Bool(*i1 < *i2),
            (Reg::Int(i1), Reg::Float(f2)) => Reg::Bool((*i1 as f64) < *f2),
            (Reg::Float(f1), Reg::Int(i2)) => Reg::Bool(*f1 < *i2 as f64),
            (Reg::Float(f1), Reg::Float(f2)) => Reg::Bool(*f1 < *f2),
            (Reg::Bool(b1), Reg::Bool(b2)) => Reg::Bool(bool::lt(b1, b2)),
            (Reg::Str(s1), Reg::Str(s2)) => {
                let s1 = gc.get_string_by_id(s1);
                let s2 = gc.get_string_by_id(s2);
                Reg::Bool(s1 < s2)
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
}

pub struct OpAnd {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpAnd {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
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
}

pub struct OpOr {
    pub loc: Loc,
    pub lhs: usize,
    pub rhs: usize,
    pub rd: usize,
}

impl Instruction for OpOr {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
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
}

pub struct OpMove {
    pub loc: Loc,
    pub rs: usize,
    pub rd: usize,
}

impl Instruction for OpMove {
    fn exec(&self, ip: Ip, context: &mut Vm, _functions: &[Func]) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let rs = gc.read_reg(self.rs);
        let rs = gc.clone_reg(rs);
        gc.write_reg(self.rd, rs);
        Ok(Ip {
            func_id: ip.func_id,
            inst: ip.inst + 1,
        })
    }
}
