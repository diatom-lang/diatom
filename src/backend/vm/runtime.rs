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

    /// Contract: this function must returns a Err(...)
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

    fn exec_arithmetic_int_float(&mut self, name: &str, rs1: u32, rs2: u32, rd:u32,
        int_calc: fn(i64, i64) -> Option<i64>, float_calc: fn(f64, f64) -> f64) -> Result<(), String>
    {
        use Data::*;
        let rs1_data = self.load_register(rs1)?;
        let rs2_data = self.load_register(rs2)?;
        let rd_data = match (rs1_data, rs2_data) {
            (Int(i1), Int(i2)) => Int(int_calc(i1, i2).ok_or_else(|| {
                self.on_error(format!("{name} overflow.")).unwrap_err()
            })?),
            (Float(i1), Int(i2)) => Float(float_calc(i1, i2 as f64)),
            (Int(i1), Float(i2)) => Float(float_calc(i1 as f64, i2)),
            (Float(i1), Float(i2)) => Float(float_calc(i1, i2)),
            (type1, type2) => {
                return Err(self
                    .on_error(format!(
                        "{name} can not be performed between {} and {}",
                        get_type_name(&type1),
                        get_type_name(&type2)
                    ))
                    .unwrap_err())
            }
        };
        self.save_register(rd, rd_data)?;
        Ok(())
    }

    fn exec_arithmetic_float(&mut self, name: &str, rs1: u32, rs2: u32, rd: u32, 
        float_calc: fn(f64, f64) -> f64, gen_data: fn(f64) -> Data) -> Result<(), String>
    {
        use Data::*;
        let rs1_data = self.load_register(rs1)?;
        let rs2_data = self.load_register(rs2)?;
        let calc_res = match (rs1_data, rs2_data) {
            (Int(i1), Int(i2)) => float_calc(i1 as f64, i2 as f64),
            (Float(i1), Int(i2)) => float_calc(i1, i2 as f64),
            (Int(i1), Float(i2)) => float_calc(i1 as f64, i2),
            (Float(i1), Float(i2)) => float_calc(i1, i2),
            (type1, type2) => {
                return Err(self
                    .on_error(format!(
                        "{name} can not be performed between {} and {}",
                        get_type_name(&type1),
                        get_type_name(&type2)
                    ))
                    .unwrap_err());
            }
        };
        let rd_data = gen_data(calc_res); 
        self.save_register(rd, rd_data)?;
        Ok(())
    }

    fn exec_arithmetic_two_bool(&mut self, name: &str, rs1: u32, rs2: u32, rd: u32, 
        bool_calc: fn(bool, bool) -> bool) -> Result<(), String>
    {
        use Data::*;
        let rs1_data = self.load_register(rs1)?;
        let rs2_data = self.load_register(rs2)?;
        let calc_res = match (rs1_data, rs2_data) {
            (Bool(b1), Bool(b2)) => bool_calc(b1, b2),
            (type1, type2) => {
                return Err(self
                    .on_error(format!(
                        "{name} can not be performed between {} and {}",
                        get_type_name(&type1),
                        get_type_name(&type2)
                    ))
                    .unwrap_err());
            }
        };
        let rd_data = Bool(calc_res);
        self.save_register(rd, rd_data)?;
        Ok(())
    }

    fn exec_arithmetic_one_bool(&mut self, name: &str, rs1: u32, rd: u32, 
        bool_calc: fn(bool) -> bool) -> Result<(), String>
    {
        use Data::*;
        let rs1_data = self.load_register(rs1)?;
        let calc_res = match rs1_data {
            Bool(b) => bool_calc(b),
            typ => {
                return Err(self
                    .on_error(format!(
                        "{name} can not be performed on {}",
                        get_type_name(&typ)
                    ))
                    .unwrap_err());
            }
        };
        let rd_data = Bool(calc_res);
        self.save_register(rd, rd_data)?;
        Ok(())
    }

    fn exec_arithmetic_compare(&mut self, name: &str, rs1: u32, rs2: u32, rd: u32,
        int_cmp: fn(&i64, &i64) -> bool, float_cmp: fn(&f64, &f64) -> bool) -> Result<(), String>
    {
        use Data::*;
        let rs1_data = self.load_register(rs1)?;
        let rs2_data = self.load_register(rs2)?;
        let cmp_res = match (rs1_data, rs2_data) {
            (Int(i1), Int(i2)) => int_cmp(&i1, &i2),
            (Float(i1), Int(i2)) => float_cmp(&i1, &(i2 as f64)),
            (Int(i1), Float(i2)) => float_cmp(&(i1 as f64), &i2),
            (Float(i1), Float(i2)) => float_cmp(&i1, &i2),
            (type1, type2) => {
                return Err(self
                    .on_error(format!(
                        "{name} can not be performed between {} and {}",
                        get_type_name(&type1),
                        get_type_name(&type2)
                    ))
                    .unwrap_err())
            }
        };
        let rd_data = Bool(cmp_res);
        self.save_register(rd, rd_data)?;
        Ok(())
    }

    pub fn exec(&mut self) -> Result<(), String> {
        use std::ops::{Add, Sub, Mul, Div};
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
                        if rd as usize >= self.instructions.len() {
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
                    self.exec_arithmetic_int_float("Addition", rs1, rs2, rd, i64::checked_add, f64::add)?;
                }
                sub => {
                    self.exec_arithmetic_int_float("Subtraction", rs1, rs2, rd, i64::checked_sub, f64::sub)?;
                }
                mul => {
                    self.exec_arithmetic_int_float("Multiplication", rs1, rs2, rd, i64::checked_mul, f64::mul)?;
                }
                div => {
                    self.exec_arithmetic_float("Division", rs1, rs2, rd, f64::div, Float)?;
                }
                idiv => {
                    let gen_data = |x: f64| Int(x.floor() as i64);
                    self.exec_arithmetic_float("Floor division", rs1, rs2, rd, f64::div, gen_data)?;
                }
                modu => {
                    use std::ops::Rem;
                    self.exec_arithmetic_int_float("Modulus", rs1, rs2, rd, i64::checked_rem, f64::rem)?;
                }
                exp => {
                    self.exec_arithmetic_float("Exponential", rs1, rs2, rd, f64::powf, Float)?;
                }
                or => {
                    use std::ops::BitOr;
                    self.exec_arithmetic_two_bool("Logical or", rs1, rs2, rd, bool::bitor)?;
                }
                and => {
                    use std::ops::BitAnd;
                    self.exec_arithmetic_two_bool("Logical and", rs1, rs2, rd, bool::bitand)?;
                }
                not => {
                    use std::ops::Not;
                    self.exec_arithmetic_one_bool("Logical Not", rs1, rd, bool::not)?;
                }
                ge => {
                    self.exec_arithmetic_compare("GE", rs1, rs2, rd, i64::ge, f64::ge)?;
                }
                gt => {
                    self.exec_arithmetic_compare("GT", rs1, rs2, rd, i64::gt, f64::gt)?;
                }
                ne => {
                    self.exec_arithmetic_compare("NE", rs1, rs2, rd, i64::ne, f64::ne)?;
                }
                eq => {
                    self.exec_arithmetic_compare("EQ", rs1, rs2, rd, i64::eq, f64::eq)?;
                }
                le => {
                    self.exec_arithmetic_compare("LE", rs1, rs2, rd, i64::le, f64::le)?;
                }
                lt => {
                    self.exec_arithmetic_compare("LT", rs1, rs2, rd, i64::lt, f64::lt)?;
                }
            }
            self.ip += 1;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::OpCode::*;
    use super::Data::*;
    use super::*;

    fn tuple_to_inst(t : (OpCode, u32, u32, u32)) -> Inst {
        Inst::new(t.0, t.1, t.2, t.3)
    }

    #[test]
    fn test_push_call() {
        let mut vm = VM::new();
        let instructions = [
            (push, 0, 0, 5),
            (call, 0, 0, 2),
            (push, 0, 0, 10),
            (call, 0, 0, 4),
            (push, 0, 0, 1),
        ].map(tuple_to_inst);
        vm.push_instructions(instructions.iter());
        vm.exec().expect("Execution failed");
        assert_eq!(vm.call_stack, vec![(2, 0), (4, 5)]);
        assert_eq!(vm.next_frame, Some(1));
        assert_eq!(vm.data_stack.len(), 16);
        assert_eq!(vm.ip, 5);
    }

    #[test]
    fn test_arithmetic() {
        let mut vm = VM::new();
        let instructions = [
            (push, 0, 0, 10),
            (call, 0, 0, 2),
            (load, 0, 0, 0),
            (load, 1, 0, 1),
            (load, 2, 0, 2),
            (add, 0, 1, 3),
            (sub, 0, 1, 4),
            (mul, 0, 1, 5),
            (div, 0, 1, 6),
            (idiv, 1, 0, 7),
            (modu, 1, 0, 8),
            (exp, 2, 0, 9)
        ].map(tuple_to_inst);
        let consts = vec![
            Int(12), 
            Int(23), 
            Float(1.234)
        ];
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
            assert!(f64::abs(data - 12.0/23.0) < 1e-10);
        } else {
            panic!("Wrong type");
        }
        if let Data::Int(data) = vm.load_register(7).unwrap() {
            assert_eq!(data, 23 / 12);
        } else {
            panic!("Wrong type");
        }
        if let Data::Int(data) = vm.load_register(8).unwrap() {
            assert_eq!(data, 11);
        } else {
            panic!("Wrong type");
        }
        if let Data::Float(data) = vm.load_register(9).unwrap() {
            // 1.234 ^ 12 = 12.467572902176588
            assert!(f64::abs(data - 12.467572902176588) < 1e-10);
        } else {
            panic!("Wrong type");
        }
    }

    #[test]
    fn test_logical_op() {
         let mut pre_inst = vec![
            (push, 0, 0, 42),
            (call, 0, 0, 2),
            (load, 0, 0, 0),
            (load, 1, 0, 1),
        ];
        let mut rd_k = 2;
        for op in [or, and] {
            pre_inst.push((op, 0, 0, rd_k));
            pre_inst.push((op, 0, 1, rd_k + 1));
            pre_inst.push((op, 1, 0, rd_k + 2));
            pre_inst.push((op, 1, 1, rd_k + 3));
            rd_k += 4;
        }
        pre_inst.push((not, 0, 0, rd_k));
        pre_inst.push((not, 1, 0, rd_k + 1));

        let instructions : Vec<_> = pre_inst.into_iter().map(tuple_to_inst).collect();
        let consts = [Bool(false), Bool(true)];

        let mut vm = VM::new();
        vm.push_instructions(instructions.iter());
        vm.push_consts(consts.iter());
        vm.exec().expect("Execution failed");

        let right_result = [
            false, true, true, true,
            false, false, false, true,
            true, false
        ];

        for i in 0..10 {
            if let Bool(b) = vm.load_register(2 + i).unwrap() {
                assert_eq!(b, right_result[i as usize]);
            } else {
                panic!("Wrong type");
            }
        }
    }

    #[test]
    fn test_compare() {
        let mut pre_inst = vec![
            (push, 0, 0, 42),
            (call, 0, 0, 2),
            (load, 0, 0, 0),
            (load, 1, 0, 1),
            (load, 2, 0, 2),
            (load, 3, 0, 3),
            (load, 4, 0, 4),
            (load, 5, 0, 5),
        ];
        let mut rd_k = 6;
        for op in [lt, le, gt, ge, eq, ne] {
            pre_inst.push((op, 0, 1, rd_k));
            pre_inst.push((op, 1, 1, rd_k + 1));
            pre_inst.push((op, 2, 1, rd_k + 2));
            pre_inst.push((op, 3, 4, rd_k + 3));
            pre_inst.push((op, 4, 4, rd_k + 4));
            pre_inst.push((op, 5, 4, rd_k + 5));
            rd_k += 6;
        }
        let instructions : Vec<_> = pre_inst.into_iter().map(tuple_to_inst).collect();
        let consts = [
            Int(-5), Int(1), Int(3),
            Float(-2.7), Float(3.6), Float(7.9)
        ];

        let mut vm = VM::new();
        vm.push_instructions(instructions.iter());
        vm.push_consts(consts.iter());
        vm.exec().expect("Execution failed");

        let right_result = [
            true, false, false, true, false, false,
            true, true, false, true, true, false,
            false, false, true, false, false, true,
            false, true, true, false, true, true,
            false, true, false, false, true, false,
            true, false, true, true, false, true
        ];
        for i in 0..36 {
            if let Bool(res) = vm.load_register(6 + i).unwrap() {
                assert_eq!(res, right_result[i as usize]);
            } else {
                panic!("Wrong type");
            }
        }
    }
}
