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
    /// e.g. `push 10` (10 at rd)
    push,
    /// Call a function and switch stack frame
    ///
    /// A push must be used before call. e.g. `call 10` (10 at rd)
    call,
    /// Load a const 
    ///
    /// Load a const from const pool(indexed by rs1) to rd. e.g. `load 1, 5`
    load,
    add,
    sub,
    mul,
    div,
    idiv,
    /// mod(%)
    modu,
    exp,
    or,
    and,
    not,
    ge,
    gt,
    ne,
    eq,
    le,
    lt,
}
