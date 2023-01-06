use super::super::{Context, Tracing, VmError};

struct OpTerminate;

impl<T: Context> Tracing<T> for OpTerminate {
    fn exec(&self, ip: usize, context: &mut T) -> Result<usize, VmError> {
        Err(VmError::Success)
    }
}

struct OpMove {
    pub rs: usize,
    pub rd: usize,
}

impl<T: Context> Tracing<T> for OpMove {
    fn exec(&self, ip: usize, context: &mut T) -> Result<usize, VmError> {
        let obj = context.read_reg(self.rs);
        context.write_reg(self.rd, obj);
        Ok(ip + 1)
    }
}

struct OpPush {
    pub rs: Vec<usize>,
}

impl<T: Context> Tracing<T> for OpPush {
    fn exec(&self, ip: usize, context: &mut T) -> Result<usize, VmError> {
        self.rs.iter().map(|x| context.push(context.read_reg(*x)));
        Ok(ip + 1)
    }
}
