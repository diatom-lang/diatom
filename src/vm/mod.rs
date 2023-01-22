use self::gc::{Gc, GcId, Reg};

use crate::diagnostic::Loc;

use ahash::AHashMap;
use bimap::BiHashMap;

mod gc;
mod op;
mod string_pool;

const STACK_SIZE_LIMIT: usize = u32::MAX as usize;
type TypeId = usize;
type FuncId = usize;

struct Ip {
    pub func_id: usize,
    pub inst: usize,
}

struct Func {
    name: String,
    parameters: usize,
    reg_size: usize,
    insts: Vec<Box<dyn Instruction>>,
}

trait Instruction {
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError>;
}

struct Object {
    obj_type: TypeId,
    attributes: AHashMap<String, Reg>,
}

enum VmError {
    InvalidRegId(Loc, usize),
    InvalidFunc(Loc, usize),
    NotCallable(Loc, String),
    ParameterLengthNotMatch(Loc, usize, usize),
}

pub struct Vm {
    functions: Vec<Func>,
    gc: Gc,
    /// map from type name to type id
    type_table: BiHashMap<String, TypeId>,
    types: Vec<GcId>,
}

impl Vm {
    fn run(&mut self, func_id: usize) -> Result<String, VmError> {
        todo!()
    }

    fn repl_continue(&mut self, func_id: usize) -> Result<String, VmError> {
        todo!()
    }

    fn functions(&self) -> &Vec<Func> {
        &self.functions
    }
}
