use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};

use crate::{vm::Ip, IoWrite, State};

mod key_pool;
mod pool;
mod prelude;
use key_pool::KeyPool;
use more_asserts::debug_assert_gt;
use pool::Pool;

use self::prelude::{init_float_meta, init_gc_meta, init_int_meta, init_list_meta};

pub struct Table {
    pub attributes: BTreeMap<usize, Reg>,
    pub meta_table: Option<usize>,
}

pub enum GcObject<Buffer: IoWrite> {
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
    List(Vec<Reg>),
    Table(Table),
    Tuple(Vec<Reg>),
}

impl<Buffer: IoWrite> Default for GcObject<Buffer> {
    fn default() -> Self {
        Self::Tuple(vec![])
    }
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
    reg_size: usize,
}

struct CallStack {
    frames: Vec<Frame>,
    regs: Vec<StackReg>,
    fp: Frame,
}

#[derive(Default)]
struct GrayPool {
    pinned_string: BTreeSet<usize>,
    escaped: BTreeSet<usize>,
    objects: BTreeSet<usize>,
}

/// Garbage Collector
pub struct Gc<Buffer: IoWrite> {
    obj_pool: Pool<GcObject<Buffer>>,
    escaped_pool: Pool<Reg>,
    string_pool: Pool<String>,
    call_stack: CallStack,
    up_values: Vec<BTreeSet<usize>>,
    gray_pool: GrayPool,
    key_pool: KeyPool,
    int_meta: usize,
    float_meta: usize,
    list_meta: usize,
    gc_meta: usize,
    threshold: usize,
    paused: bool,
}

static UNIT_REG: Reg = Reg::Unit;

impl<Buffer: IoWrite> Gc<Buffer> {
    pub fn new() -> Self {
        let mut key_pool = KeyPool::default();
        let mut obj_pool = Pool::<GcObject<Buffer>>::default();
        let int_meta = init_int_meta(&mut obj_pool, &mut key_pool);
        let float_meta = init_float_meta(&mut obj_pool, &mut key_pool);
        let list_meta = init_list_meta(&mut obj_pool, &mut key_pool);
        let gc_meta = init_gc_meta(&mut obj_pool, &mut key_pool);

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
                    reg_size: 0,
                },
            },
            up_values: vec![BTreeSet::new()],
            string_pool: Default::default(),
            obj_pool,
            escaped_pool: Default::default(),
            key_pool,
            int_meta,
            float_meta,
            list_meta,
            gc_meta,
            gray_pool: Default::default(),
            threshold: 0,
            paused: false,
        }
    }

    pub fn look_up_table_key(&self, id: usize) -> Option<&str> {
        self.key_pool.look_up_key(id)
    }

    pub fn get_or_insert_table_key(&mut self, key: impl Into<String> + AsRef<str>) -> usize {
        self.key_pool.get_or_insert(key)
    }

    pub fn alloc_obj(&mut self, obj: GcObject<Buffer>) -> usize {
        self.try_collect();
        self.obj_pool.alloc(obj)
    }

    pub fn alloc_str(&mut self, s: String) -> usize {
        self.try_collect();
        self.string_pool.alloc(s)
    }

    pub fn alloc_str_pinned(&mut self, s: String) -> usize {
        let id = self.string_pool.alloc(s);
        self.gray_pool.pinned_string.insert(id);
        id
    }

    pub fn get_obj(&self, id: usize) -> Option<&GcObject<Buffer>> {
        self.obj_pool.get(id)
    }

    pub unsafe fn get_obj_unchecked(&self, id: usize) -> &GcObject<Buffer> {
        self.obj_pool.get_unchecked(id)
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
        debug_assert_gt!(stack.regs.len(), n);
        debug_assert_gt!(stack.fp.ptr + stack.fp.reg_size, n);
        let reg = unsafe { self.call_stack.regs.get_unchecked(n) };
        match reg {
            StackReg::Reg(reg) => reg,
            StackReg::Shared(id) => {
                let reg = unsafe { self.escaped_pool.get_unchecked(*id) };
                reg
            }
        }
    }

    pub fn share_reg(&mut self, id: usize) -> usize {
        debug_assert!(id != 0);
        self.try_collect();
        let stack = &mut self.call_stack;
        let frame = stack.fp.ptr;
        debug_assert!(self.call_stack.regs.len() > frame + id);
        let shared_reg = unsafe { self.call_stack.regs.get_unchecked_mut(frame + id) };
        match shared_reg {
            StackReg::Reg(reg) => {
                let sid = self.escaped_pool.alloc(reg.clone());
                *shared_reg = StackReg::Shared(sid);
                let frame_idx = self.call_stack.frames.len();
                debug_assert!(self.up_values.len() > frame_idx);
                unsafe { self.up_values.get_unchecked_mut(frame_idx) }.insert(frame + id);
                sid
            }
            StackReg::Shared(sid) => *sid,
        }
    }

    /// Write reg to id
    ///
    /// Automatically extend reg file size if n > regs.len()
    pub fn write_reg(&mut self, n: usize, reg: Reg) {
        debug_assert!(n != 0);
        let stack = &mut self.call_stack;
        let n = stack.fp.ptr + n;
        debug_assert_gt!(stack.regs.len(), n);
        debug_assert_gt!(stack.fp.ptr + stack.fp.reg_size, n);
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
                reg_size: usize::MAX,
            },
        );
        stack.frames.push(fp_old);
        if stack.frames.len() + 1 > self.up_values.len() {
            self.up_values.push(BTreeSet::new());
        } else {
            debug_assert!(self.up_values.len() > stack.frames.len());
            unsafe { self.up_values.get_unchecked_mut(stack.frames.len()).clear() }
        }

        if let GcObject::Closure {
            captured, reg_size, ..
        } = unsafe { self.obj_pool.get_unchecked(rid) }
        {
            stack.fp.reg_size = *reg_size;
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
        let frame_idx = stack.frames.len();
        debug_assert!(self.up_values.len() > frame_idx);
        unsafe { self.up_values.get_unchecked_mut(frame_idx) }
            .iter()
            .for_each(|reg| {
                debug_assert!(stack.regs.len() > *reg);
                *unsafe { stack.regs.get_unchecked_mut(*reg) } = StackReg::Reg(Reg::Unit);
            });

        debug_assert!(!stack.frames.is_empty());
        let fp = unsafe { stack.frames.pop().unwrap_unchecked() };

        let Frame {
            return_addr,
            write_back,
            ptr,
            rid,
            ..
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

    pub fn int_meta(&self) -> usize {
        self.int_meta
    }

    pub fn float_meta(&self) -> usize {
        self.float_meta
    }

    pub fn list_meta(&self) -> usize {
        self.list_meta
    }

    pub fn gc_meta(&self) -> usize {
        self.gc_meta
    }

    pub fn clean_call_stack(&mut self) -> Vec<Ip> {
        let mut trace = vec![];
        while !self.call_stack.frames.is_empty() {
            trace.push(self.call_stack.fp.return_addr);
            self.pop_call_stack();
        }
        trace
    }

    pub fn print(&self, reg: &Reg) -> String {
        let mut buffer = String::new();
        let mut visited = BTreeSet::new();
        self.print_reg(reg, &mut visited, &mut buffer);
        buffer
    }

    fn print_reg(&self, reg: &Reg, visited: &mut BTreeSet<usize>, buffer: &mut String) {
        use std::fmt::Write;
        match reg {
            Reg::Unit => write!(buffer, "()"),
            Reg::Bool(b) => write!(buffer, "{b}"),
            Reg::Int(i) => write!(buffer, "{i}"),
            Reg::Float(f) => write!(buffer, "{f}"),
            Reg::Str(sid) => write!(buffer, "{}", self.get_str(*sid).unwrap()),
            Reg::Ref(r) => {
                if visited.get(r).is_some() {
                    write!(buffer, "<Recursive ref@{}>", *r).unwrap();
                    return;
                }
                visited.insert(*r);
                match self.get_obj(*r).unwrap() {
                    GcObject::Closure {
                        func_id,
                        parameters: _,
                        reg_size: _,
                        captured: _,
                    } => {
                        write!(buffer, "Closure[{func_id}]")
                    }
                    GcObject::NativeFunction(f) => {
                        write!(buffer, "External function@{:p}", f.as_ptr())
                    }
                    GcObject::List(l) => {
                        write!(buffer, "[").unwrap();
                        for (i, value) in l.iter().enumerate() {
                            if i == l.len() - 1 {
                                self.print_reg(value, visited, buffer);
                            } else {
                                self.print_reg(value, visited, buffer);
                                write!(buffer, ", ").unwrap();
                            }
                        }
                        write!(buffer, "]")
                    }
                    GcObject::Table(t) => {
                        write!(buffer, "{{").unwrap();
                        for (i, (key, value)) in t.attributes.iter().enumerate() {
                            let key = self.key_pool.look_up_key(*key).unwrap();
                            write!(buffer, "{key} = ").unwrap();
                            if i == t.attributes.len() - 1 {
                                self.print_reg(value, visited, buffer);
                            } else {
                                self.print_reg(value, visited, buffer);
                                write!(buffer, ", ").unwrap();
                            }
                        }
                        write!(buffer, "}}")
                    }
                    GcObject::Tuple(t) => {
                        write!(buffer, "(").unwrap();
                        for (i, value) in t.iter().enumerate() {
                            if i == t.len() - 1 {
                                self.print_reg(value, visited, buffer);
                            } else {
                                self.print_reg(value, visited, buffer);
                                write!(buffer, ", ").unwrap();
                            }
                        }
                        write!(buffer, ")")
                    }
                }
            }
        }
        .unwrap();
    }

    pub fn set_main_reg_size(&mut self, n: usize) {
        assert!(self.call_stack.frames.is_empty());
        self.call_stack.fp.reg_size = n;
    }

    /// Collect garbage if threshold requirement is met
    #[cfg(not(test))]
    fn try_collect(&mut self) {
        if self.paused {
            return;
        }
        let total_allocated =
            self.string_pool.len() + self.obj_pool.len() + self.escaped_pool.len();
        if total_allocated > self.threshold {
            self.collect()
        }

        let total_allocated =
            self.string_pool.len() + self.obj_pool.len() + self.escaped_pool.len();
        self.threshold = total_allocated * 2;
    }

    #[cfg(test)]
    fn try_collect(&mut self) {
        if self.paused {
            return;
        }
        let _ = self.threshold;
        let _ = self.obj_pool.len();
        self.collect()
    }

    fn clear_marks(&mut self) {
        self.obj_pool.clear_marks();
        self.escaped_pool.clear_marks();
        self.string_pool.clear_marks();
    }

    fn mark_roots(&mut self) {
        self.clear_marks();

        let stack = &mut self.call_stack;
        let stack_limit = stack
            .frames
            .iter()
            .map(|frame| frame.ptr + frame.reg_size)
            .fold(stack.fp.ptr + stack.fp.reg_size, usize::max);

        // Detect incorrect stack actually length in debug
        #[cfg(debug_assertions)]
        stack.regs.truncate(stack_limit);

        self.call_stack.regs.iter().for_each(|reg| {
            match reg {
                StackReg::Reg(reg) => match reg {
                    Reg::Str(sid) => {
                        self.string_pool.mark(*sid);
                    }
                    Reg::Ref(rid) => {
                        self.gray_pool.objects.insert(*rid);
                    }
                    _ => (),
                },
                StackReg::Shared(sid) => {
                    self.gray_pool.escaped.insert(*sid);
                }
            };
        });
        self.gray_pool
            .pinned_string
            .iter()
            .for_each(|sid| self.string_pool.mark(*sid));
    }

    pub fn pause(&mut self) {
        self.paused = true
    }

    pub fn resume(&mut self) {
        self.paused = false
    }

    pub fn collect(&mut self) {
        self.mark_roots();
        let gray_pool = &mut self.gray_pool;

        fn mark_reg(
            reg: &Reg,
            obj_gray_pool: &mut BTreeSet<usize>,
            string_pool: &mut Pool<String>,
        ) {
            match reg {
                Reg::Str(sid) => string_pool.mark(*sid),
                Reg::Ref(rid) => {
                    obj_gray_pool.insert(*rid);
                }
                _ => (),
            }
        }

        while !gray_pool.objects.is_empty() || !gray_pool.escaped.is_empty() {
            if !gray_pool.escaped.is_empty() {
                // Mark all escaped variable
                let escaped_id = gray_pool.escaped.pop_last().unwrap();
                let (reg, marked) = unsafe { self.escaped_pool.get_unchecked_raw(escaped_id) };
                if !marked {
                    mark_reg(reg, &mut gray_pool.objects, &mut self.string_pool);
                    self.escaped_pool.mark(escaped_id);
                }
            } else {
                // Mark all objects
                let obj_id = gray_pool.objects.pop_last().unwrap();
                match unsafe { self.obj_pool.get_unchecked_raw(obj_id) } {
                    (_, true) | (GcObject::NativeFunction { .. }, _) => (),
                    (GcObject::List(l) | GcObject::Tuple(l), false) => {
                        for item in l.iter() {
                            mark_reg(item, &mut gray_pool.objects, &mut self.string_pool);
                        }
                    }
                    (
                        GcObject::Table(Table {
                            attributes,
                            meta_table,
                        }),
                        false,
                    ) => attributes.values().for_each(|reg| {
                        mark_reg(reg, &mut gray_pool.objects, &mut self.string_pool);
                        if let Some(reg_id) = meta_table {
                            gray_pool.objects.insert(*reg_id);
                        }
                    }),
                    (GcObject::Closure { captured, .. }, false) => {
                        captured.iter().for_each(|(_, reg_id)| {
                            gray_pool.escaped.insert(*reg_id);
                        });
                    }
                }
                self.obj_pool.mark(obj_id);
            }
        }
        self.escaped_pool.collect();
        self.string_pool.collect();
        self.obj_pool.collect();
    }
}

impl<Buffer: IoWrite> Default for Gc<Buffer> {
    fn default() -> Self {
        Self::new()
    }
}
