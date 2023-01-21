use std::ops::{Index, IndexMut};

use super::{string_pool::StringId, VmError};

type GcId = usize;

enum Mark {
    White,
    Gray,
    Black,
}

pub enum GcObject {
    Unit,
    Int(i64),
    Float(f64),
    Str(StringId),
}

#[derive(Clone)]
pub enum Reg {
    Unit,
    Int(i64),
    Float(f64),
    Ref(GcId),
}

/// Garbage Collector
#[derive(Default)]
pub struct Gc {
    pool: Vec<(GcObject, Mark)>,
    free: Vec<usize>,
    call_stack: Vec<(Vec<Reg>, usize, Option<usize>)>,
}

impl Gc {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc(&mut self, obj: GcObject) -> GcId {
        if self.free.is_empty() {
            self.pool.push((obj, Mark::White));
            self.pool.len() - 1
        } else {
            self.free.pop().unwrap()
        }
    }

    pub fn read_reg(&self, n: usize) -> Result<Reg, VmError> {
        self.call_stack
            .last()
            .and_then(|x| x.0.get(n).cloned())
            .ok_or(VmError::InvalidRegId(n))
    }

    pub fn write_reg(&mut self, n: usize, reg: Reg) -> Result<(), VmError> {
        self.call_stack
            .last_mut()
            .and_then(|x| {
                x.0.get_mut(n).map(|prev| {
                    *prev = reg;
                })
            })
            .ok_or(VmError::InvalidRegId(n))
    }

    pub fn alloc_call_stack(
        &mut self,
        n: usize,
        return_address: usize,
        write_result: Option<usize>,
    ) {
        self.call_stack
            .push((vec![Reg::Unit; n], return_address, write_result))
    }

    /// None if there is no more call stack
    pub fn pop_call_stack(&mut self) -> Option<(usize, Option<usize>)> {
        if self.call_stack.len() <= 1 {
            self.call_stack.pop();
            None
        } else {
            self.call_stack
                .pop()
                .map(|(_, return_address, write_result)| (return_address, write_result))
        }
    }
}

impl Index<GcId> for Gc {
    type Output = GcObject;
    fn index(&self, index: GcId) -> &Self::Output {
        &self.pool[index].0
    }
}

impl IndexMut<GcId> for Gc {
    fn index_mut(&mut self, index: GcId) -> &mut Self::Output {
        &mut self.pool[index].0
    }
}
