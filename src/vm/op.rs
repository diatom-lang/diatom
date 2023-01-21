use super::{Context, Tracing, VmError};

/// Init main call stack
struct OpInit {
    stack_size: usize,
}

impl<T: Context> Tracing<T> for OpInit {
    fn exec(&self, ip: usize, context: &mut T) -> Result<usize, VmError> {
        context.gc().alloc_call_stack(self.stack_size, ip, None);
        Ok(ip + 1)
    }
}

/// Call a function by its id
struct OpCall {
    /// Register that return value is written back to
    pub write_result: Option<usize>,
    /// id of the function
    pub target: usize,
    /// Registers to be passed as parameters
    pub parameters: Vec<usize>,
}

impl<T: Context> Tracing<T> for OpCall {
    fn exec(&self, ip: usize, context: &mut T) -> Result<usize, VmError> {
        let func = context
            .get_func(self.target)
            .ok_or(VmError::InvalidFunctionId(self.target))?;
        if func.paras != self.parameters.len() {
            return Err(VmError::ParameterWrongLength);
        }
        let func_ip = func.ip;
        let func_regs = func.regs;
        let mut paras = vec![];
        for para in self.parameters.iter() {
            paras.push(context.gc().read_reg(*para)?);
        }
        context
            .gc()
            .alloc_call_stack(func_regs, ip + 1, self.write_result);
        assert!(func_regs >= paras.len());
        for (i, para) in paras.into_iter().enumerate() {
            context.gc().write_reg(i, para)?;
        }
        Ok(func_ip)
    }
}

/// Return from a function call
struct OpRet {
    /// Register to be used as return value
    reg: usize,
}

impl<T: Context> Tracing<T> for OpRet {
    fn exec(&self, _ip: usize, context: &mut T) -> Result<usize, VmError> {
        let reg = context.gc().read_reg(self.reg)?;
        let call_stack = context.gc().pop_call_stack();
        match call_stack {
            None => Err(VmError::Success),
            Some((return_address, write_result)) => {
                if let Some(reg_id) = write_result {
                    context.gc().write_reg(reg_id, reg)?;
                }
                Ok(return_address)
            }
        }
    }
}
