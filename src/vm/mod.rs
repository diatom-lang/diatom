use crate::interpreter::Func;

use self::{error::VmError, gc::Gc, op::*};

use ahash::AHashMap;

pub mod error;
mod gc;
pub mod op;
mod state;
mod string_pool;
use enum_dispatch::enum_dispatch;
pub use gc::{GcObject, Reg};
pub use state::State;

type FuncId = usize;

#[derive(Clone, Copy)]
pub struct Ip {
    pub func_id: usize,
    pub inst: usize,
}

#[enum_dispatch(VmInst)]
pub trait Instruction {
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError>;
    fn decompile(&self, decompiled: &mut String, context: &Vm);
}

#[allow(clippy::enum_variant_names)]
#[enum_dispatch]
pub enum VmInst {
    OpAllocReg,
    OpCallClosure,
    OpMakeClosure,
    OpRet,
    OpAdd,
    OpAnd,
    OpBranch,
    OpDiv,
    OpDummy,
    OpEq,
    OpGt,
    OpIDiv,
    OpJump,
    OpLoadConstant,
    OpMove,
    OpMul,
    OpNeg,
    OpNot,
    OpOr,
    OpPow,
    OpRem,
    OpSub,
    OpYield,
}

pub struct Object {
    _attributes: AHashMap<String, Reg>,
}

pub struct Vm {
    gc: Gc,
    ip: Ip,
    output: String,
    is_repl: bool,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            gc: Gc::new(),
            ip: Ip {
                func_id: 0,
                inst: 0,
            },
            output: String::new(),
            is_repl: false,
        }
    }

    pub fn exec(&mut self, byte_code: &[Func], is_repl: bool) -> VmError {
        self.output.clear();
        self.is_repl = is_repl;
        loop {
            let Ip { func_id, inst } = self.ip;
            debug_assert!(byte_code.len() > func_id);
            let func = unsafe { byte_code.get_unchecked(func_id) };
            debug_assert!(func.insts.len() > inst);
            self.ip = match unsafe { func.insts.get_unchecked(inst) }
                .exec(self.ip, self)
                .map_err(|err| {
                    self.gc.clean_call_stack();
                    err
                }) {
                Ok(ip) => ip,
                Err(err) => return err,
            };
        }
    }

    pub fn take_output(&mut self) -> String {
        std::mem::take(&mut self.output)
    }

    pub fn set_ip(&mut self, ip: Ip) {
        self.ip = ip
    }

    pub fn gc_mut(&mut self) -> &mut Gc {
        &mut self.gc
    }
}
