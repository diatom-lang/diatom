use std::{
    cell::UnsafeCell,
    ops::{Index, IndexMut},
    rc::Rc,
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
    Closure(FuncId, Vec<Rc<UnsafeCell<Reg>>>),
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

enum StackReg {
    Reg(Reg),
    Shared(Rc<UnsafeCell<Reg>>),
}

struct CallStack {
    regs: Vec<StackReg>,
    return_addr: Ip,
    write_back: Option<usize>,
}

/// Garbage Collector
pub struct Gc {
    pool: Vec<(GcObject, Mark)>,
    free: Vec<usize>,
    call_stack: Vec<CallStack>,
    string_pool: UnsafeCell<StringPool>,
}

impl Gc {
    pub fn new() -> Self {
        Self {
            pool: vec![],
            free: vec![],
            call_stack: vec![CallStack {
                regs: vec![],
                return_addr: Ip {
                    func_id: usize::MAX,
                    inst: usize::MAX,
                },
                write_back: None,
            }],
            string_pool: UnsafeCell::new(StringPool::new()),
        }
    }

    pub fn alloc(&mut self, obj: GcObject) -> GcId {
        if self.free.is_empty() {
            self.pool.push((obj, Mark::White));
            self.pool.len() - 1
        } else {
            self.free.pop().unwrap()
        }
    }

    pub fn read_reg(&self, n: usize) -> &Reg {
        self.call_stack
            .last()
            .and_then(|x| {
                x.regs.get(n).map(|reg| match reg {
                    StackReg::Reg(reg) => reg,
                    StackReg::Shared(rc) => unsafe { &*rc.as_ref().get() },
                })
            })
            .unwrap()
    }

    pub fn clone_reg(&self, reg: &Reg) -> Reg {
        match reg {
            Reg::Unit => Reg::Unit,
            Reg::Bool(b) => Reg::Bool(*b),
            Reg::Int(i) => Reg::Int(*i),
            Reg::Float(f) => Reg::Float(*f),
            Reg::Str(sid) => Reg::Str(unsafe { (*(self.string_pool.get())).clone_str(sid) }),
            Reg::Ref(r) => Reg::Ref(*r),
        }
    }

    pub fn share_reg(&mut self, id: usize) -> Rc<UnsafeCell<Reg>> {
        let last = self.call_stack.len() - 1;
        let stack = &mut self.call_stack[last];
        let reg = std::mem::replace(&mut stack.regs[id], StackReg::Reg(Reg::Unit));
        match reg {
            StackReg::Reg(reg) => {
                let rc = Rc::new(UnsafeCell::new(reg));
                stack.regs[id] = StackReg::Shared(rc.clone());
                rc
            }
            StackReg::Shared(rc) => rc,
        }
    }

    /// Write reg to id
    ///
    /// Automatically extend reg file size if n > regs.len()
    pub fn write_reg(&mut self, n: usize, mut reg: Reg) {
        let last = self.call_stack.len() - 1;
        let stack = &mut self.call_stack[last];
        if n >= stack.regs.len() {
            (0..=n - stack.regs.len())
                .into_iter()
                .for_each(|_| stack.regs.push(StackReg::Reg(Reg::Unit)))
        }

        let prev = &mut stack.regs[n];
        let prev = match prev {
            StackReg::Reg(prev) => prev,
            StackReg::Shared(rc) => unsafe { &mut *rc.as_ref().get() },
        };
        std::mem::swap(prev, &mut reg);
        if let Reg::Str(sid) = reg {
            self.string_pool.get_mut().delete(sid);
        }
    }

    pub fn write_shared_reg(&mut self, n: usize, reg: Rc<UnsafeCell<Reg>>) {
        let last = self.call_stack.len() - 1;
        let stack = &mut self.call_stack[last];
        if n > stack.regs.len() {
            (0..=n - stack.regs.len())
                .into_iter()
                .for_each(|_| stack.regs.push(StackReg::Reg(Reg::Unit)))
        }

        let prev = std::mem::replace(&mut stack.regs[n], StackReg::Shared(reg));
        let prev = match prev {
            StackReg::Reg(prev) => prev,
            // This operation is only used when function is initializing
            // This can not be a shared reg
            StackReg::Shared(_) => unreachable!(),
        };
        if let Reg::Str(sid) = prev {
            self.string_pool.get_mut().delete(sid);
        }
    }

    pub fn alloc_call_stack(&mut self, return_addr: Ip, write_back: Option<usize>) {
        self.call_stack.push(CallStack {
            regs: vec![],
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
                     regs,
                     return_addr,
                     write_back,
                 }| {
                    regs.into_iter().for_each(|reg| {
                        // Delete string ref count
                        // Shared Reg must be referenced by at least one closure
                        // do not need to delete here
                        if let StackReg::Reg(Reg::Str(sid)) = reg {
                            self.string_pool.get_mut().delete(sid)
                        }
                    });
                    (return_addr, write_back)
                },
            )
        }
    }

    pub fn clean_call_stack(&mut self) {
        while self.call_stack.len() > 1 {
            self.pop_call_stack();
        }
    }

    pub fn get_string_by_id(&self, sid: &StringId) -> &str {
        unsafe { &(*self.string_pool.get())[sid] }
    }

    #[allow(clippy::mut_from_ref)]
    pub fn string_pool(&self) -> &mut StringPool {
        unsafe { &mut *self.string_pool.get() }
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
