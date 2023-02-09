use std::{
    cell::{RefCell, UnsafeCell},
    collections::BTreeSet,
    io::Write,
    ops::{Index, IndexMut},
    rc::Rc,
};

use ahash::AHashMap;

use crate::State;

use super::{string_pool::StringPool, Ip};

/// Reference id of heap allocated object
pub type RefId = usize;

enum Mark {
    White,
}

pub struct Table {
    pub attributes: AHashMap<String, Reg>,
}

pub enum GcObject<Buffer: Write> {
    Closure {
        func_id: usize,
        parameters: usize,
        reg_size: usize,
        /// local id, shared reg
        captured: Vec<(usize, Rc<UnsafeCell<Reg>>)>,
    },
    #[allow(clippy::type_complexity)]
    NativeFunction(
        Rc<RefCell<dyn Fn(&mut State<Buffer>, &[Reg], &mut Buffer) -> Result<Reg, String>>>,
    ),
    Table(Table),
}

/// Diatom's unboxed value type
#[derive(Default, Clone)]
pub enum Reg {
    #[default]
    Unit,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(usize),
    Ref(RefId),
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
pub struct Gc<Buffer: Write> {
    pool: Vec<(GcObject<Buffer>, Mark)>,
    free: BTreeSet<usize>,
    call_stack: CallStack,
    string_pool: StringPool,
}

impl<Buffer: Write> Gc<Buffer> {
    pub fn new() -> Self {
        Self {
            pool: vec![],
            free: BTreeSet::new(),
            call_stack: CallStack {
                prev: None,
                regs: vec![],
                return_addr: Ip {
                    func_id: usize::MAX,
                    inst: usize::MAX,
                },
                write_back: None,
            },
            string_pool: StringPool::new(),
        }
    }

    pub fn alloc(&mut self, obj: GcObject<Buffer>) -> RefId {
        if self.free.is_empty() {
            self.pool.push((obj, Mark::White));
            self.pool.len() - 1
        } else {
            self.free.pop_last().unwrap()
        }
    }

    pub fn alloc_reg_file(&mut self, n: usize) {
        let stack = &mut self.call_stack;
        if n >= stack.regs.len() {
            (0..=n - stack.regs.len())
                .into_iter()
                .for_each(|_| stack.regs.push(StackReg::Reg(Reg::Unit)))
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

    pub fn share_reg(&mut self, id: usize, depth: usize) -> Rc<UnsafeCell<Reg>> {
        debug_assert!(self.call_stack.regs.len() > id);
        let mut call_stack = &mut self.call_stack;
        debug_assert!(depth > 0);
        // relative depth to capture and make_closure is 1
        let depth = depth - 1;
        for _ in 0..depth {
            debug_assert!(call_stack.prev.is_some());
            call_stack = unsafe { call_stack.prev.as_mut().unwrap_unchecked().as_mut() }
        }
        let shared_reg = unsafe { call_stack.regs.get_unchecked_mut(id) };
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
    pub fn write_reg(&mut self, n: usize, reg: Reg) {
        let stack = &mut self.call_stack;
        debug_assert!(stack.regs.len() > n);
        let prev = unsafe { stack.regs.get_unchecked_mut(n) };
        match prev {
            StackReg::Reg(r) => *r = reg,
            StackReg::Shared(rc) => *unsafe { &mut *rc.as_ref().get() } = reg,
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
        *unsafe { stack.regs.get_unchecked_mut(n) } = StackReg::Shared(reg)
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
            regs: _,
            return_addr,
            write_back,
            prev: _,
        } = std::mem::replace(&mut self.call_stack, call_stack_prev);
        Some((return_addr, write_back))
    }

    pub fn clean_call_stack(&mut self) {
        while self.call_stack.prev.is_some() {
            self.pop_call_stack();
        }
    }

    pub fn string_pool(&mut self) -> &mut StringPool {
        &mut self.string_pool
    }

    pub fn get_string_by_id_checked(&self, id: usize) -> Option<&str> {
        self.string_pool.get(id)
    }

    pub fn get_string_by_id(&self, id: usize) -> &str {
        &self.string_pool[id]
    }

    pub fn get_obj_by_ref(&mut self, ref_id: usize) -> Option<&mut GcObject<Buffer>> {
        if self.free.get(&ref_id).is_some() {
            return None;
        }
        self.pool.get_mut(ref_id).map(|obj| &mut obj.0)
    }
}

impl<Buffer: Write> Index<RefId> for Gc<Buffer> {
    type Output = GcObject<Buffer>;
    fn index(&self, index: RefId) -> &Self::Output {
        &self.pool[index].0
    }
}

impl<Buffer: Write> IndexMut<RefId> for Gc<Buffer> {
    fn index_mut(&mut self, index: RefId) -> &mut Self::Output {
        &mut self.pool[index].0
    }
}
