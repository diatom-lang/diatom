use std::{cell::RefCell, io::Write, rc::Rc};

use ahash::AHashMap;

use crate::State;

mod pool;
use pool::Pool;

use super::Ip;

pub struct Table {
    pub attributes: AHashMap<String, Reg>,
}

pub enum GcObject<Buffer: Write> {
    Closure {
        func_id: usize,
        parameters: usize,
        reg_size: usize,
        /// local id, shared reg
        captured: Vec<(usize, usize)>,
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
    Ref(usize),
}

enum StackReg {
    Reg(Reg),
    Shared(usize),
}

struct Frame {
    ptr: usize,
    return_addr: Ip,
    write_back: Option<usize>,
    rid: usize,
}

struct CallStack {
    frames: Vec<Frame>,
    regs: Vec<StackReg>,
    fp: Frame,
}

/// Garbage Collector
pub struct Gc<Buffer: Write> {
    obj_pool: Pool<GcObject<Buffer>>,
    escaped_pool: Pool<Reg>,
    string_pool: Pool<String>,
    call_stack: CallStack,
}

static UNIT_REG: Reg = Reg::Unit;

impl<Buffer: Write> Gc<Buffer> {
    pub fn new() -> Self {
        Self {
            call_stack: CallStack {
                frames: vec![],
                regs: vec![],
                fp: Frame {
                    ptr: 0,
                    // Main function can never return
                    rid: usize::MAX,
                    return_addr: Ip {
                        func_id: usize::MAX,
                        inst: usize::MAX,
                    },
                    write_back: None,
                },
            },
            string_pool: Default::default(),
            obj_pool: Default::default(),
            escaped_pool: Default::default(),
        }
    }

    pub fn alloc_obj(&mut self, obj: GcObject<Buffer>) -> usize {
        self.obj_pool.alloc(obj)
    }

    pub fn alloc_str(&mut self, s: String) -> usize {
        self.string_pool.alloc(s)
    }

    pub fn get_obj(&self, id: usize) -> Option<&GcObject<Buffer>> {
        self.obj_pool.get(id)
    }

    pub unsafe fn get_obj_unchecked(&self, id: usize) -> &GcObject<Buffer> {
        self.obj_pool.get_unchecked(id)
    }

    pub fn get_obj_mut(&mut self, id: usize) -> Option<&mut GcObject<Buffer>> {
        self.obj_pool.get_mut(id)
    }

    pub unsafe fn get_obj_unchecked_mut(&mut self, id: usize) -> &mut GcObject<Buffer> {
        self.obj_pool.get_unchecked_mut(id)
    }

    pub fn get_str(&self, id: usize) -> Option<&str> {
        self.string_pool.get(id).map(|s| s.as_str())
    }

    pub unsafe fn get_str_unchecked(&self, id: usize) -> &str {
        self.string_pool.get_unchecked(id)
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
            StackReg::Shared(id) => unsafe { self.escaped_pool.get_unchecked(*id) },
        }
    }

    pub fn share_reg(&mut self, id: usize, depth: usize) -> usize {
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
                let id = self.escaped_pool.alloc(reg);
                *shared_reg = StackReg::Shared(id);
                id
            }
            StackReg::Shared(id) => id,
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
            StackReg::Shared(id) => *unsafe { self.escaped_pool.get_unchecked_mut(*id) } = reg,
        }
    }

    pub fn alloc_call_stack(
        &mut self,
        return_addr: Ip,
        write_back: Option<usize>,
        start: usize,
        rid: usize,
    ) {
        let stack = &mut self.call_stack;
        let ptr = stack.fp.ptr + start;
        let fp_old = std::mem::replace(
            &mut stack.fp,
            Frame {
                ptr,
                return_addr,
                write_back,
                rid,
            },
        );
        stack.frames.push(fp_old);

        if let GcObject::Closure {
            captured, reg_size, ..
        } = unsafe { self.obj_pool.get_unchecked(rid) }
        {
            // Alloc registers
            (stack.regs.len()..ptr + reg_size)
                .into_iter()
                .for_each(|_| stack.regs.push(StackReg::Reg(Reg::Unit)));
            // Write captured regs
            captured.iter().for_each(|(rd, reg)| {
                debug_assert!(rd + ptr < stack.regs.len());
                *unsafe { stack.regs.get_unchecked_mut(rd + ptr) } = StackReg::Shared(*reg);
            })
        } else {
            debug_assert!(false)
        }
    }

    /// None if there is no more call stack
    pub fn pop_call_stack(&mut self) -> (Ip, Option<usize>) {
        let stack = &mut self.call_stack;
        debug_assert!(!stack.frames.is_empty());
        let fp = unsafe { stack.frames.pop().unwrap_unchecked() };

        let Frame {
            return_addr,
            write_back,
            ptr,
            rid,
        } = std::mem::replace(&mut stack.fp, fp);

        if let GcObject::Closure { captured, .. } = unsafe { self.obj_pool.get_unchecked(rid) } {
            captured.iter().for_each(|(shared_reg, _)| {
                debug_assert!(shared_reg + ptr < stack.regs.len());
                *unsafe { stack.regs.get_unchecked_mut(shared_reg + ptr) } =
                    StackReg::Reg(Reg::Unit);
            })
        } else {
            debug_assert!(false)
        }
        (return_addr, write_back)
    }

    pub fn clean_call_stack(&mut self) {
        self.call_stack.frames.truncate(1);
    }
}
