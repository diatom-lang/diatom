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

struct Frame {
    ptr: usize,
    shared: usize,
    return_addr: Ip,
    write_back: Option<usize>,
}

struct CallStack {
    shared: Vec<usize>,
    frames: Vec<Frame>,
    regs: Vec<StackReg>,
    fp: Frame,
}

/// Garbage Collector
pub struct Gc<Buffer: Write> {
    pool: Vec<(GcObject<Buffer>, Mark)>,
    free: BTreeSet<usize>,
    call_stack: CallStack,
    string_pool: StringPool,
}

static UNIT_REG: Reg = Reg::Unit;

impl<Buffer: Write> Gc<Buffer> {
    pub fn new() -> Self {
        Self {
            pool: vec![],
            free: BTreeSet::new(),
            call_stack: CallStack {
                shared: vec![],
                frames: vec![],
                regs: vec![],
                fp: Frame {
                    ptr: 0,
                    shared: 0,
                    return_addr: Ip {
                        func_id: usize::MAX,
                        inst: usize::MAX,
                    },
                    write_back: None,
                },
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
        let frame = stack.fp.ptr;
        (stack.regs.len()..frame + n)
            .into_iter()
            .for_each(|_| stack.regs.push(StackReg::Reg(Reg::Unit)))
    }

    pub fn read_reg(&self, n: usize) -> &Reg {
        if n == 0 {
            return &UNIT_REG;
        }
        let stack = &self.call_stack;
        let n = stack.fp.ptr + n;
        debug_assert!(stack.regs.len() > n);
        let reg = unsafe { self.call_stack.regs.get_unchecked(n) };
        match reg {
            StackReg::Reg(reg) => reg,
            StackReg::Shared(rc) => unsafe { &*rc.as_ref().get() },
        }
    }

    pub fn share_reg(&mut self, id: usize, depth: usize) -> Rc<UnsafeCell<Reg>> {
        debug_assert!(id != 0);
        let stack = &mut self.call_stack;
        debug_assert!(depth > 0);
        let frame = if depth == 1 {
            stack.fp.ptr
        } else {
            stack.frames[stack.frames.len() + 1 - depth].ptr
        };
        let shared_reg = &mut self.call_stack.regs[frame + id];
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
        debug_assert!(n != 0);
        let stack = &mut self.call_stack;
        let n = stack.fp.ptr + n;
        debug_assert!(stack.regs.len() > n);
        let prev = unsafe { stack.regs.get_unchecked_mut(n) };
        match prev {
            StackReg::Reg(r) => *r = reg,
            StackReg::Shared(rc) => *unsafe { &mut *rc.as_ref().get() } = reg,
        }
    }

    pub fn write_shared_reg(&mut self, n: usize, reg: Rc<UnsafeCell<Reg>>) {
        debug_assert!(n != 0);
        let stack = &mut self.call_stack;
        stack.fp.shared += 1;
        stack.shared.push(n);
        let n = stack.fp.ptr + n;
        debug_assert!(stack.regs.len() > n);
        *unsafe { stack.regs.get_unchecked_mut(n) } = StackReg::Shared(reg)
    }

    pub fn alloc_call_stack(&mut self, return_addr: Ip, write_back: Option<usize>, start: usize) {
        let stack = &mut self.call_stack;
        let ptr = stack.fp.ptr;
        let fp_old = std::mem::replace(
            &mut stack.fp,
            Frame {
                ptr: ptr + start,
                shared: 0,
                return_addr,
                write_back,
            },
        );
        self.call_stack.frames.push(fp_old);
    }

    /// None if there is no more call stack
    pub fn pop_call_stack(&mut self) -> Option<(Ip, Option<usize>)> {
        let stack = &mut self.call_stack;
        let fp = stack.frames.pop().unwrap();

        let Frame {
            return_addr,
            write_back,
            shared,
            ptr,
        } = std::mem::replace(&mut stack.fp, fp);

        (0..shared).into_iter().for_each(|_| {
            let shared_reg = stack.shared.pop().unwrap();
            stack.regs[shared_reg + ptr] = StackReg::Reg(Reg::Unit);
        });
        Some((return_addr, write_back))
    }

    pub fn clean_call_stack(&mut self) {
        self.call_stack.frames.truncate(1);
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
