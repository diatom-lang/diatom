use crate::diagnostic::Loc;

use super::{
    gc::{GcObject, Reg},
    Func, Instruction, Ip, Object, Vm, VmError,
};

struct OpCallClosure {
    reg_id: usize,
    parameters: Vec<usize>,
    write_back: Option<usize>,
    loc: Loc,
}

impl Instruction for OpCallClosure {
    fn exec(&self, ip: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let obj = gc
            .read_reg(self.reg_id)
            .map_err(|_| VmError::InvalidRegId(self.loc.clone(), self.reg_id))?;
        let (closure, func_id, capture) = match obj {
            Reg::Unit => Err("()".to_string()),
            Reg::Bool(_) => Err("bool".to_string()),
            Reg::Int(_) => Err("Int".to_string()),
            Reg::Float(_) => Err("Float".to_string()),
            Reg::Str(_) => Err("Str".to_string()),
            Reg::Ref(r) => {
                let obj = &gc[*r];
                match obj {
                    GcObject::Closure(func_id, capture) => Ok((*r, func_id, capture)),
                    GcObject::Object(Object {
                        obj_type,
                        attributes: _,
                    }) => Err(context
                        .type_table
                        .get_by_right(obj_type)
                        .unwrap_or_else(|| panic!("Type id {obj_type} does not exists!"))
                        .clone()),
                }
            }
        }
        .map_err(|t| {
            VmError::NotCallable(self.loc.clone(), format!("Type `{t}` is not callable"))
        })?;

        let func_id = *func_id;
        let capture_owned: Vec<_> = capture
            .iter()
            .map(|c| {
                let (pos, reg) = c.take();
                c.set((pos, Reg::default()));
                (pos, reg)
            })
            .collect();

        let Func {
            name: _,
            parameters,
            reg_size,
            insts: _,
        } = *context
            .functions
            .get(func_id)
            .ok_or_else(|| VmError::InvalidFunc(self.loc.clone(), func_id))?;

        if parameters != self.parameters.len() {
            return Err(VmError::ParameterLengthNotMatch(
                self.loc.clone(),
                parameters,
                self.parameters.len(),
            ));
        }

        gc.alloc_call_stack(
            closure,
            reg_size,
            Ip {
                func_id: ip.func_id,
                inst: ip.inst + 1,
            },
            self.write_back,
        );

        // write parameters to call stack
        for (i, parameter) in self.parameters.iter().enumerate() {
            let reg = gc
                .read_reg(*parameter)
                .map_err(|_| VmError::InvalidRegId(self.loc.clone(), *parameter))?;
            let reg = gc.clone_reg(reg);
            gc.write_reg(i, reg)
                .map_err(|_| VmError::InvalidRegId(self.loc.clone(), *parameter))?;
        }

        // write capture values to stack
        for (pos, reg) in capture_owned {
            gc.write_reg(pos, reg).expect("Reg index out of bound");
        }

        Ok(Ip { func_id, inst: 0 })
    }
}

struct OpRet {
    return_reg: usize,
    loc: Loc,
}

impl Instruction for OpRet {
    fn exec(&self, _: Ip, context: &mut Vm) -> Result<Ip, VmError> {
        let gc = &mut context.gc;
        let reg = gc
            .read_reg(self.return_reg)
            .map_err(|_| VmError::InvalidRegId(self.loc.clone(), self.return_reg))?;
        let reg = gc.clone_reg(reg);

        // clean call stack
        let (ip, write_back) = gc.pop_call_stack().expect("Return on empty call stack!");

        // write return value back
        write_back.map(|write_back| gc.write_reg(write_back, reg));

        Ok(ip)
    }
}
