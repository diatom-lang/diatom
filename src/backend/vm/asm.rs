use serde::{Deserialize, Serialize};

pub const VM_VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Serialize, Deserialize, Clone)]
pub struct Inst {
    pub op: OpCode,
    pub rs1: u32,
    pub rs2: u32,
    pub rd: u32,
}

impl Inst {
    pub fn new(op: OpCode, rs1: u32, rs2: u32, rd: u32) -> Self {
        Self { op, rs1, rs2, rd }
    }
}

#[derive(Serialize, Deserialize)]
pub struct AsmFile {
    md5: String,
    vm_version: &'static str,
    instructions: Vec<Inst>,
}

impl AsmFile {
    pub fn new(md5: String, instructions: Vec<Inst>) -> Self {
        Self {
            md5,
            vm_version: VM_VERSION,
            instructions,
        }
    }

    pub fn disassemble(&self) -> String {
        todo!()
    }
}

#[derive(Serialize, Deserialize, Clone, Copy)]
#[allow(non_camel_case_types)]
pub enum OpCode {
    /// Allocate stack for next function.
    ///
    /// `push (rs1 = ?, rs2 = ?, rd)`
    /// 
    /// Allocate `rd` registers for the next function.
    push,
    /// Call a function and switch stack frame
    ///
    /// `call (rs1 = ?, rs2 = ?, rd)`
    /// 
    /// `ip <- rd`
    /// 
    /// A push must be used before call.
    call,
    /// Load a const 
    ///
    /// `load (rs1, rs2 = ?, rd)`
    /// 
    /// `R[rd] <- ConstPool[rs1]`
    load,
    /// Add values.
    /// 
    /// `add (rs1, rs2, rd)`
    /// 
    /// `R[rd] <- R[rs1] + R[rs2]`
    add,
    /// Subtract values.
    /// 
    /// `sub (rs1, rs2, rd)`
    /// 
    /// `R[rd] <- R[rs1] - R[rs2]`
    sub,
    /// Multiply values.
    /// 
    /// `mul (rs1, rs2, rd)`
    /// 
    /// `R[rd] <- R[rs1] * R[rs2]`
    mul,
    /// Divide values.
    /// 
    /// `div (rs1, rs2, rd)`
    /// 
    /// `R[rd] <- R[rs1] / R[rs2]`
    div,
    /// Floor-divide values.
    /// 
    /// `idiv (rs1, rs2, rd)`
    /// 
    /// `R[rd] <- floor(R[rs1] / R[rs2])`
    idiv,
    /// Calculate reminder.
    /// 
    /// `modu (rs1, rs2, rd)`
    /// 
    /// `R[rd] <- R[rs1] % R[rs2]`
    modu,
    /// Exponential.
    /// 
    /// `exp (rs1, rs2, rd)`
    /// 
    /// `R[rd] <- R[rs1] ^ R[rs2]`
    /// 
    /// Note that the result is a float, even if both inputs are integers.
    exp,
    /// Logical or.
    /// 
    /// `or (rs1, rs2, rd)`
    /// 
    /// `R[rd] <- R[rs1] or R[rs2]`.
    /// 
    /// Both inputs should be bool. The output is bool.
    or,
    /// Logical and.
    /// 
    /// `and (rs1, rs2, rd)`
    /// 
    /// `R[rd] <- R[rs1] and R[rs2]`.
    /// 
    /// Both inputs should be bool. The output is bool.
    and,
    /// Logical not.
    /// 
    /// `not (rs1, rs2 = ?, rd)`.
    /// 
    /// `R[rd] <- not R[rs1]`.
    /// 
    /// The input should be bool. The output is bool.
    not,
    /// Greater than or equal.
    /// 
    /// `ge (rs1, rs2, rd)`
    /// 
    /// `R[rd] <- R[rs1] >= R[rs2]`
    /// 
    /// `R[rs1]` and `R[rs2]` should be numbers, and `R[rd]` will be bool.
    ge,
    gt,
    ne,
    eq,
    le,
    lt,
}
