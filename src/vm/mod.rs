use crate::runtime::Oceanus;

mod ir;

const STACK_SIZE_LIMIT: usize = u32::MAX as usize;

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
}

#[derive(Default, Clone, Copy)]
enum VmObj {
    #[default]
    Unit,
    Int(i64),
    Float(f64),
}

trait Context {
    fn read_reg(&self, offset: usize) -> VmObj;
    fn write_reg(&mut self, offset: usize, obj: VmObj);
    fn call_by_name(&mut self, method: &str, para: usize) -> Result<VmObj, VmError>;
    fn call(&mut self, obj: VmObj) -> Result<usize, VmError>;
    fn push(&mut self, obj: VmObj) -> Result<(), VmError>;
    fn get_type(&self, obj: VmObj) -> String;
}

trait Tracing<T>
where
    T: Context,
{
    fn exec(&self, ip: usize, context: &mut T) -> Result<usize, VmError>;
}

struct Vm {
    stack: Vec<VmObj>,
    call_frames: Vec<usize>,
    runtime: Oceanus<VmObj>,
}

impl Context for Vm {
    fn read_reg(&self, offset: usize) -> VmObj {
        self.stack[self.call_frames.last().unwrap() + offset]
    }

    fn write_reg(&mut self, offset: usize, obj: VmObj) {
        self.stack[self.call_frames.last().unwrap() + offset] = obj
    }

    fn call_by_name(&mut self, method: &str, para: usize) -> Result<VmObj, VmError> {
        todo!()
    }

    fn push(&mut self, obj: VmObj) -> Result<(), VmError> {
        self.stack.push(obj);
        if self.stack.len() > STACK_SIZE_LIMIT {
            Err(VmError::StackOverflow)
        } else {
            Ok(())
        }
    }

    fn call(&mut self, obj: VmObj) -> Result<usize, VmError> {
        todo!()
    }

    fn get_type(&self, obj: VmObj) -> String {
        match obj {
            VmObj::Unit => "Unit",
            VmObj::Int(_) => "Int",
            VmObj::Float(_) => "Float",
        }
        .to_string()
    }
}

impl Vm {
    fn new() -> Self {
        Self {
            stack: vec![],
            call_frames: vec![0],
            runtime: Oceanus::default(),
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
