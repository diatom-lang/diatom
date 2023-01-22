use std::{
    cell::{Cell, RefCell},
    ops::{Index, IndexMut},
};

use super::{
    string_pool::{StringId, StringPool},
    FuncId, Ip, Object,
};

pub type GcId = usize;

enum Mark {
    White,
    Gray,
    Black,
}

pub enum GcObject {
    Closure(FuncId, Vec<Cell<(usize, Reg)>>),
    Object(Object),
}

#[derive(Default)]
pub enum Reg {
    #[default]
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(StringId),
    Ref(GcId),
}

struct CallStack {
    closure: GcId,
    regs: Vec<Reg>,
    return_addr: Ip,
    write_back: Option<usize>,
}

/// Garbage Collector
#[derive(Default)]
pub struct Gc {
    pool: Vec<(GcObject, Mark)>,
    free: Vec<usize>,
    call_stack: Vec<CallStack>,
    string_pool: RefCell<StringPool>,
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

    pub fn read_reg(&self, n: usize) -> Result<&Reg, ()> {
        self.call_stack.last().and_then(|x| x.regs.get(n)).ok_or(())
    }

    pub fn clone_reg(&self, reg: &Reg) -> Reg {
        match reg {
            Reg::Unit => Reg::Unit,
            Reg::Bool(b) => Reg::Bool(*b),
            Reg::Int(i) => Reg::Int(*i),
            Reg::Float(f) => Reg::Float(*f),
            Reg::Str(sid) => Reg::Str(self.string_pool.borrow_mut().clone_str(sid)),
            Reg::Ref(r) => Reg::Ref(*r),
        }
    }

    pub fn write_reg(&mut self, n: usize, mut reg: Reg) -> Result<(), ()> {
        self.call_stack
            .last_mut()
            .and_then(|x| {
                x.regs.get_mut(n).map(|prev| {
                    std::mem::swap(prev, &mut reg);
                    if let Reg::Str(sid) = reg {
                        self.string_pool.get_mut().delete(sid);
                    }
                })
            })
            .ok_or(())
    }

    pub fn alloc_call_stack(
        &mut self,
        closure: GcId,
        regs: usize,
        return_addr: Ip,
        write_back: Option<usize>,
    ) {
        self.call_stack.push(CallStack {
            closure,
            regs: (1..regs).into_iter().map(|_| Reg::default()).collect(),
            return_addr,
            write_back,
        })
    }

    /// None if there is no more call stack
    pub fn pop_call_stack(&mut self) -> Option<(Ip, Option<usize>)> {
        if self.call_stack.len() <= 1 {
            self.call_stack.pop();
            None
        } else {
            self.call_stack.pop().map(
                |CallStack {
                     closure,
                     mut regs,
                     return_addr,
                     write_back,
                 }| {
                    // write captured values back to closure
                    if let GcObject::Closure(_, capture) = &self[closure] {
                        capture.iter().for_each(|c| {
                            let (pos, reg) = c.take();
                            debug_assert!(matches!(reg, Reg::Unit));
                            c.set((pos, std::mem::replace(&mut regs[pos], reg)));
                        })
                    } else {
                        panic!("Internal Error: Call stack does not contain a closure.")
                    }
                    // destruct regs
                    regs.into_iter().for_each(|reg| {
                        if let Reg::Str(sid) = reg {
                            self.string_pool.get_mut().delete(sid)
                        }
                    });
                    (return_addr, write_back)
                },
            )
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
