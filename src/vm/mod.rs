use bimap::BiHashMap;

use self::{gc::Gc, types::{TypeTable, ParticularType}};

mod gc;
mod op;
mod string_pool;
mod types;
mod prelude;

const STACK_SIZE_LIMIT: usize = u32::MAX as usize;

type TypeId = usize;

#[derive(Debug, PartialEq, Eq)]
enum VmError {
    Success,
    StackOverflow,
    InvalidType(String, String, String),
    ReduceOverflow,
    OverlapInstance,
    CanNotDeduce,
    InvalidConstraint,
    ParameterWrongType,
    ParameterWrongLength,
    AnyTypeAttribute,
    AmbiguousMethod,
    NoSuchMethod,
    NotAnInstance,
    InvalidFunctionId(usize),
    InvalidRegId(usize),
}

trait Context {
    fn gc(&mut self) -> &mut Gc;
    fn get_func(&self, n: usize) -> Option<&VmFunc>;
}

trait Tracing<T>
where
    T: Context,
{
    fn exec(&self, ip: usize, context: &mut T) -> Result<usize, VmError>;
}

struct VmFunc {
    ip: usize,
    regs: usize,
    paras: usize,
}

struct Vm {
    gc: Gc,
    funcs: Vec<VmFunc>,
    type_table: TypeTable,
    type_cache: BiHashMap<ParticularType, usize>
}

impl Context for Vm {
    fn gc(&mut self) -> &mut Gc {
        &mut self.gc
    }
    fn get_func(&self, id: usize) -> Option<&VmFunc> {
        self.funcs.get(id)
    }
}

impl Vm {
    fn new(type_table: TypeTable) -> Self {
        Self {
            gc: Gc::default(),
            funcs: vec![],
            type_table,
            type_cache: BiHashMap::default()
        }
    }

    fn exec(&mut self, ops: Vec<Box<dyn Tracing<Vm>>>) -> (VmError, usize) {
        if ops.len() == 0 {
            return (VmError::Success, 0);
        }
        let mut ip = 0;
        loop {
            ip = match ops[ip].exec(ip, self) {
                Ok(ip) => ip,
                Err(err) => return (err, ip),
            }
        }
    }
}
