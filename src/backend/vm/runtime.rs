use super::asm::{Inst, OpCode};

#[derive(Clone, Debug)]
pub enum Data {
    Int(i64),
    Float(f64),
    Bool(bool),
    Nil,
}

const fn get_type_name(data: &Data) -> &'static str {
    use Data::*;
    match data {
        Int(_) => "int",
        Float(_) => "float",
        Bool(_) => "bool",
        Nil => "nil",
    }
}

impl Default for Data {
    fn default() -> Self {
        Self::Nil
    }
}

pub struct VM {
    instructions: Vec<Inst>,
    consts: Vec<Data>,
    data_stack: Vec<Data>,
    /// (return address, frame pointer)
    call_stack: Vec<(u32, u32)>,
    next_frame: Option<u32>,
    ip: u32,
}

impl VM {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            consts: vec![],
            data_stack: vec![],
            call_stack: vec![],
            next_frame: None,
            ip: 0,
        }
    }

    pub fn push_instructions<'a, T: Iterator<Item = &'a Inst>>(&mut self, insts: T) {
        insts.for_each(|inst| self.instructions.push(inst.clone()));
    }

    pub fn push_consts<'a, T: Iterator<Item = &'a Data>>(&mut self, consts: T) {
        consts.for_each(|cons| self.consts.push(cons.clone()));
    }

    fn load_const(&mut self, index: u32) -> Result<Data, String> {
        let data = self.consts.get(index as usize);
        match data {
            None => Err(self
                .on_error(format!(
                    "Constants out of boundary.\nRegister index: {index}\nConstant Stack top: {}",
                    self.consts.len()
                ))
                .unwrap_err()),
            Some(data) => Ok(data.clone()),
        }
    }

    fn load_register(&mut self, index: u32) -> Result<Data, String> {
        if self.call_stack.len() == 0 {
            self.on_error(format!("Load register without stack frame."))?
        }
        let fp = self.call_stack[self.call_stack.len() - 1].1;
        let data = self.data_stack.get((fp + index) as usize);
        match data {
            None => {
                Err(self.on_error(format!("Register out of boundary.\nFrame pointer: {fp}\nRegister index: {index}\nStack top: {}", self.data_stack.len())).unwrap_err())
            }
            Some(data) => Ok(data.clone()),
        }
    }

    fn save_register(&mut self, index: u32, data: Data) -> Result<(), String> {
        if self.call_stack.len() == 0 {
            self.on_error(format!("Load register without stack frame."))?
        }
        let fp = self.call_stack[self.call_stack.len() - 1].1;
        let rd = self.data_stack.get_mut((fp + index) as usize);
        match rd {
            None => {
                Err(self.on_error(format!("Register out of boundary.\nFrame pointer: {fp}\nRegister index: {index}\nStack top: {}", self.data_stack.len())).unwrap_err())
            }
            Some(rd) => {
        *rd = data;
        Ok(())
            },
        }
    }

    fn on_error(&mut self, error: String) -> Result<(), String> {
        if self.call_stack.len() > 1 {
            // pop all frames except main (frame 0)
            let remain_len = self.data_stack.len() as u32 - self.call_stack[0].1;
            self.data_stack.truncate(remain_len as usize);
            // reset ip
            self.ip = self.call_stack[0].0;
        }
        Err(error)
    }

    pub fn exec(&mut self) -> Result<(), String> {
        use Data::*;
        use OpCode::*;
        while (self.ip as usize) < self.instructions.len() {
            let Inst { op, rs1, rs2, rd } = self.instructions[self.ip as usize];
            match op {
                push => {
                    let size = rd;
                    if self.next_frame.is_some() {
                        self.on_error(format!("Double push detected."))?
                    } else {
                        self.next_frame = Some(size);
                        (0..size).for_each(|_| self.data_stack.push(Data::Nil));
                    }
                }
                call => match self.next_frame {
                    None => {
                        self.on_error(format!("Call function without creating a stack frame."))?
                    }
                    Some(frame) => {
                        self.call_stack
                            .push((self.ip + 1, self.data_stack.len() as u32 - frame));
                        if rd as usize > self.instructions.len() {
                            self.on_error(format!(
                                "Call address {rd} while maximum instruction offset is {}",
                                self.instructions.len()
                            ))?
                        }
                        self.ip = rd;
                        self.next_frame = None;
                        continue;
                    }
                },
                load => {
                    let data = self.load_const(rs1)?;
                    self.save_register(rd, data)?;
                }
                add => {
                    let rs1_data = self.load_register(rs1)?;
                    let rs2_data = self.load_register(rs2)?;
                    let rd_data = match (rs1_data, rs2_data) {
                        (Int(i1), Int(i2)) => Int(i1.checked_add(i2).ok_or_else(|| {
                            self.on_error(format!("Addition overflow.")).unwrap_err()
                        })?),
                        (Float(i1), Int(i2)) => Float(i1 + i2 as f64),
                        (Int(i1), Float(i2)) => Float(i1 as f64 + i2),
                        (Float(i1), Float(i2)) => Float(i1 + i2),
                        (type1, type2) => {
                            return Err(self
                                .on_error(format!(
                                    "Addition can not be performed between {} and {}",
                                    get_type_name(&type1),
                                    get_type_name(&type2)
                                ))
                                .unwrap_err());
                        }
                    };
                    self.save_register(rd, rd_data)?;
                }
                sub => {
                    let rs1_data = self.load_register(rs1)?;
                    let rs2_data = self.load_register(rs2)?;
                    let rd_data = match (rs1_data, rs2_data) {
                        (Int(i1), Int(i2)) => Int(i1.checked_sub(i2).ok_or_else(|| {
                            self.on_error(format!("Subtraction overflow.")).unwrap_err()
                        })?),
                        (Float(i1), Int(i2)) => Float(i1 - i2 as f64),
                        (Int(i1), Float(i2)) => Float(i1 as f64 - i2),
                        (Float(i1), Float(i2)) => Float(i1 - i2),
                        (type1, type2) => {
                            return Err(self
                                .on_error(format!(
                                    "Subtraction can not be performed between {} and {}",
                                    get_type_name(&type1),
                                    get_type_name(&type2)
                                ))
                                .unwrap_err());
                        }
                    };
                    self.save_register(rd, rd_data)?;
                }
                mul => {
                    let rs1_data = self.load_register(rs1)?;
                    let rs2_data = self.load_register(rs2)?;
                    let rd_data = match (rs1_data, rs2_data) {
                        (Int(i1), Int(i2)) => Int(i1.checked_mul(i2).ok_or_else(|| {
                            self.on_error(format!("Multiplication overflow."))
                                .unwrap_err()
                        })?),
                        (Float(i1), Int(i2)) => Float(i1 * i2 as f64),
                        (Int(i1), Float(i2)) => Float(i1 as f64 * i2),
                        (Float(i1), Float(i2)) => Float(i1 * i2),
                        (type1, type2) => {
                            return Err(self
                                .on_error(format!(
                                    "Multiplication can not be performed between {} and {}",
                                    get_type_name(&type1),
                                    get_type_name(&type2)
                                ))
                                .unwrap_err());
                        }
                    };
                    self.save_register(rd, rd_data)?;
                }
                div => {
                    let rs1_data = self.load_register(rs1)?;
                    let rs2_data = self.load_register(rs2)?;
                    let rd_data = match (rs1_data, rs2_data) {
                        (Int(i1), Int(i2)) => Float(i1 as f64 / i2 as f64),
                        (Float(i1), Int(i2)) => Float(i1 / i2 as f64),
                        (Int(i1), Float(i2)) => Float(i1 as f64 / i2),
                        (Float(i1), Float(i2)) => Float(i1 / i2),
                        (type1, type2) => {
                            return Err(self
                                .on_error(format!(
                                    "Division can not be performed between {} and {}",
                                    get_type_name(&type1),
                                    get_type_name(&type2)
                                ))
                                .unwrap_err());
                        }
                    };
                    self.save_register(rd, rd_data)?;
                }
                idiv => {
                    let rs1_data = self.load_register(rs1)?;
                    let rs2_data = self.load_register(rs2)?;
                    let rd_data = match (rs1_data, rs2_data) {
                        (Int(i1), Int(i2)) => i1 as f64 / i2 as f64,
                        (Float(i1), Int(i2)) => i1 / i2 as f64,
                        (Int(i1), Float(i2)) => i1 as f64 / i2,
                        (Float(i1), Float(i2)) => i1 / i2,
                        (type1, type2) => {
                            return Err(self
                                .on_error(format!(
                                    "Floor division can not be performed between {} and {}",
                                    get_type_name(&type1),
                                    get_type_name(&type2)
                                ))
                                .unwrap_err());
                        }
                    };
                    let rd_data = Data::Int(rd_data as i64);
                    self.save_register(rd, rd_data)?;
                }
                modu => {}
                exp => {}
                or => {}
                and => {}
                not => {}
                ge => {}
                gt => {}
                ne => {}
                eq => {}
                le => {}
                lt => {}
            }
            self.ip += 1;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::OpCode::*;
    use super::*;

    #[test]
    fn test_push_call() {
        let mut vm = VM::new();
        let instructions = vec![
            Inst::new(push, 0, 0, 5),
            Inst::new(call, 0, 0, 2),
            Inst::new(push, 0, 0, 10),
            Inst::new(call, 0, 0, 4),
            Inst::new(push, 0, 0, 1),
        ];
        vm.push_instructions(instructions.iter());
        vm.exec().expect("Execution failed");
        assert_eq!(vm.call_stack, vec![(2, 0), (4, 5)]);
        assert_eq!(vm.next_frame, Some(1));
        assert_eq!(vm.data_stack.len(), 16);
        assert_eq!(vm.ip, 5);
    }

    #[test]
    fn test_add() {
        let mut vm = VM::new();
        let instructions = vec![
            Inst::new(push, 0, 0, 7),
            Inst::new(call, 0, 0, 2),
            Inst::new(load, 0, 0, 0),
            Inst::new(load, 1, 0, 1),
            Inst::new(load, 2, 0, 2),
            Inst::new(add, 0, 1, 3),
            Inst::new(sub, 0, 1, 4),
            Inst::new(mul, 0, 1, 5),
            Inst::new(div, 0, 1, 6),
        ];
        let consts = vec![Data::Int(12), Data::Int(23), Data::Float(1.234)];
        vm.push_instructions(instructions.iter());
        vm.push_consts(consts.iter());
        vm.exec().expect("Execution failed");
        if let Data::Int(data) = vm.load_register(3).unwrap() {
            assert_eq!(data, 12 + 23);
        } else {
            panic!("Wrong type");
        }
        if let Data::Int(data) = vm.load_register(4).unwrap() {
            assert_eq!(data, 12 - 23);
        } else {
            panic!("Wrong type");
        }
        if let Data::Int(data) = vm.load_register(5).unwrap() {
            assert_eq!(data, 12 * 23);
        } else {
            panic!("Wrong type");
        }
        if let Data::Float(data) = vm.load_register(6).unwrap() {
            assert!(f64::abs(data - 12.0 / 23.0) < 1e-10);
        } else {
            panic!("Wrong type");
        }
    }
}
