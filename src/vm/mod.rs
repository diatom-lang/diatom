use crate::{
    interpreter::{Func, Gc},
    IoWrite,
};

use self::{error::VmError, op::*};

pub mod error;
pub mod op;
use enum_dispatch::enum_dispatch;

#[derive(Clone, Copy)]
pub struct Ip {
    pub func_id: usize,
    pub inst: usize,
}

#[enum_dispatch(VmInst)]
pub trait Instruction {
    fn exec<Buffer: IoWrite>(
        &self,
        ip: Ip,
        gc: &mut Gc<Buffer>,
        out: &mut Buffer,
    ) -> Result<Ip, VmError>;
    fn decompile<Buffer: IoWrite>(&self, decompiled: &mut String, gc: &Gc<Buffer>);
}

#[allow(clippy::enum_variant_names)]
#[enum_dispatch]
pub enum VmInst {
    OpAdd,
    OpSub,
    OpMul,
    OpDiv,
    OpIDiv,
    OpMove,
    OpCall,
    OpAnd,
    OpOr,
    OpNot,
    OpNeg,
    OpJump,
    OpBranchTrue,
    OpBranchFalse,
    OpEq,
    OpNe,
    OpLt,
    OpLe,
    OpGt,
    OpGe,
    OpPow,
    OpRem,
    OpGetTable,
    OpSetTable,
    OpGetTuple,
    OpSetTuple,
    OpMakeTable,
    OpMakeTuple,
    OpAllocReg,
    OpRet,
    OpMakeClosure,
    OpLoadConstant,
    OpYield,
    OpDummy,
}

pub struct Vm {
    ip: Ip,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            ip: Ip {
                func_id: 0,
                inst: 0,
            },
        }
    }

    pub fn exec<Buffer: IoWrite>(
        &mut self,
        byte_code: &[Func],
        gc: &mut Gc<Buffer>,
        out: &mut Buffer,
    ) -> VmError {
        loop {
            let Ip { func_id, inst } = self.ip;
            debug_assert!(byte_code.len() > func_id);
            let func = unsafe { byte_code.get_unchecked(func_id) };
            debug_assert!(func.insts.len() > inst);
            self.ip = match unsafe { func.insts.get_unchecked(inst) }
                .exec(self.ip, gc, out)
                .map_err(|err| {
                    gc.clean_call_stack();
                    err
                }) {
                Ok(ip) => ip,
                Err(err) => return err,
            };
        }
    }

    pub fn reset_ip(&mut self) {
        self.ip = Ip {
            func_id: 0,
            inst: 0,
        }
    }
}
