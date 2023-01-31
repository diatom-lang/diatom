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
    fn decompile(&self, decompiled: &mut String, context: &Vm);
}

pub struct Object {
    _attributes: AHashMap<String, Reg>,
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

    pub fn exec(&mut self, byte_code: &[Func]) -> VmError {
        self.output.clear();
        loop {
            let Ip { func_id, inst } = self.ip;
            debug_assert!(byte_code.len() > func_id);
            let func = unsafe { byte_code.get_unchecked(func_id) };
            debug_assert!(func.insts.len() > inst);
            self.ip = match unsafe { func.insts.get_unchecked(inst) }
                .exec(self.ip, self, byte_code)
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
