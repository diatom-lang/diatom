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
}

pub enum GcObject {
    Closure(FuncId, Vec<Rc<UnsafeCell<Reg>>>),
    _Object(Object),
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
    prev: Option<Box<CallStack>>,
    regs: Vec<StackReg>,
    return_addr: Ip,
    write_back: Option<usize>,
}

/// Garbage Collector
pub struct Gc {
    pool: Vec<(GcObject, Mark)>,
    free: Vec<usize>,
    call_stack: CallStack,
    string_pool: UnsafeCell<StringPool>,
}

impl Gc {
    pub fn new() -> Self {
        Self {
            pool: vec![],
            free: vec![],
            call_stack: CallStack {
                prev: None,
                regs: vec![],
                return_addr: Ip {
                    func_id: usize::MAX,
                    inst: usize::MAX,
                },
                write_back: None,
            },
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
        debug_assert!(self.call_stack.regs.len() > n);
        let reg = unsafe { self.call_stack.regs.get_unchecked(n) };
        match reg {
            StackReg::Reg(reg) => reg,
            StackReg::Shared(rc) => unsafe { &*rc.as_ref().get() },
        }
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
        debug_assert!(self.call_stack.regs.len() > id);
        let shared_reg = unsafe { self.call_stack.regs.get_unchecked_mut(id) };
        let reg = std::mem::replace(shared_reg, StackReg::Reg(Reg::Unit));
        match reg {
            StackReg::Reg(reg) => {
                let rc = Rc::new(UnsafeCell::new(reg));
                *shared_reg = StackReg::Shared(rc.clone());
                rc
            }
            StackReg::Shared(rc) => rc,
        }
    }

    /// Write reg to id
    ///
    /// Automatically extend reg file size if n > regs.len()
    pub fn write_reg(&mut self, n: usize, mut reg: Reg) {
        let stack = &mut self.call_stack;
        if n >= stack.regs.len() {
            (0..=n - stack.regs.len())
                .into_iter()
                .for_each(|_| stack.regs.push(StackReg::Reg(Reg::Unit)))
        }

        debug_assert!(stack.regs.len() > n);
        let prev = unsafe { stack.regs.get_unchecked_mut(n) };
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
        let stack = &mut self.call_stack;
        if n > stack.regs.len() {
            (0..=n - stack.regs.len())
                .into_iter()
                .for_each(|_| stack.regs.push(StackReg::Reg(Reg::Unit)))
        }

        debug_assert!(stack.regs.len() > n);
        let prev = std::mem::replace(
            unsafe { stack.regs.get_unchecked_mut(n) },
            StackReg::Shared(reg),
        );
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
        let call_stack_prev = std::mem::replace(
            &mut self.call_stack,
            CallStack {
                prev: None,
                regs: vec![],
                return_addr,
                write_back,
            },
        );
        self.call_stack.prev = Some(Box::new(call_stack_prev));
    }

    /// None if there is no more call stack
    pub fn pop_call_stack(&mut self) -> Option<(Ip, Option<usize>)> {
        let call_stack_prev = std::mem::replace(&mut self.call_stack.prev, None);
        let call_stack_prev = match call_stack_prev {
            Some(call_stack) => *call_stack,
            None => return None,
        };

        let CallStack {
            regs,
            return_addr,
            write_back,
            prev: _,
        } = std::mem::replace(&mut self.call_stack, call_stack_prev);
        regs.into_iter().for_each(|reg| {
            // Delete string ref count
            // Shared Reg must be referenced by at least one closure
            // do not need to delete here
            if let StackReg::Reg(Reg::Str(sid)) = reg {
                self.string_pool.get_mut().delete(sid)
            }
        });
        Some((return_addr, write_back))
    }

    pub fn clean_call_stack(&mut self) {
        while self.call_stack.prev.is_some() {
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
