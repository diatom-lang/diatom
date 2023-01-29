use crate::interpreter::Func;

use self::{error::VmError, gc::Gc};

use ahash::AHashMap;

pub mod error;
mod gc;
pub mod op;
mod string_pool;
pub use gc::Reg;

type FuncId = usize;

#[derive(Clone, Copy)]
pub struct Ip {
    pub func_id: usize,
    pub inst: usize,
}

pub trait Instruction {
    fn exec(&self, ip: Ip, context: &mut Vm, functions: &[Func]) -> Result<Ip, VmError>;
}

struct Object {
    attributes: AHashMap<String, Reg>,
}

pub struct Vm {
    gc: Gc,
    ip: Ip,
    output: String,
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
        }
    }

    pub fn exec(&mut self, byte_code: &[Func]) -> Result<String, VmError> {
        self.output.clear();
        loop {
            let Ip { func_id, inst } = self.ip;
            if func_id == 0 && inst >= byte_code[0].insts.len() {
                break;
            }
            self.ip = byte_code[func_id].insts[inst]
                .exec(self.ip, self, byte_code)
                .map_err(|err| {
                    self.gc.clean_call_stack();
                    err
                })?;
        }
        Ok(std::mem::take(&mut self.output))
    }

    pub fn set_ip(&mut self, ip: Ip) {
        self.ip = ip
    }

    pub fn gc(&self) -> &Gc {
        &self.gc
    }

    pub fn gc_mut(&mut self) -> &mut Gc {
        &mut self.gc
    }
}
