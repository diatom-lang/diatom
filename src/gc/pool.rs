use std::collections::BTreeSet;

use more_asserts::debug_assert_gt;

pub struct Pool<T: Default> {
    pool: Vec<(T, bool)>,
    free: BTreeSet<usize>,
}

impl<T: Default> Pool<T> {
    pub fn new() -> Self {
        Self {
            pool: vec![],
            free: BTreeSet::new(),
        }
    }

    pub fn len(&self) -> usize {
        self.pool.len() - self.free.len()
    }

    pub fn alloc(&mut self, value: T) -> usize {
        if self.free.is_empty() {
            self.pool.push((value, false));
            self.pool.len() - 1
        } else {
            let ref_id = self.free.pop_last().unwrap();
            debug_assert_gt!(self.pool.len(), ref_id);
            *unsafe { self.pool.get_unchecked_mut(ref_id) } = (value, false);
            ref_id
        }
    }

    pub fn get(&self, idx: usize) -> Option<&T> {
        if idx < self.pool.len() && self.free.get(&idx).is_none() {
            Some(&self.pool[idx].0)
        } else {
            None
        }
    }

    pub unsafe fn get_unchecked_raw(&self, idx: usize) -> &(T, bool) {
        debug_assert!(self.pool.len() > idx && self.free.get(&idx).is_none());
        self.pool.get_unchecked(idx)
    }

    pub unsafe fn get_unchecked(&self, idx: usize) -> &T {
        debug_assert!(self.pool.len() > idx && self.free.get(&idx).is_none());
        &self.pool.get_unchecked(idx).0
    }

    pub unsafe fn get_unchecked_mut(&mut self, idx: usize) -> &mut T {
        debug_assert!(self.pool.len() > idx && self.free.get(&idx).is_none());
        &mut self.pool.get_unchecked_mut(idx).0
    }

    pub fn mark(&mut self, idx: usize) {
        debug_assert!(self.pool.len() > idx && self.free.get(&idx).is_none());
        unsafe { self.pool.get_unchecked_mut(idx).1 = true }
    }

    pub fn clear_marks(&mut self) {
        self.pool.iter_mut().for_each(|pair| pair.1 = false);
    }

    pub fn collect(&mut self) {
        self.pool
            .iter_mut()
            .enumerate()
            .for_each(|(id, (obj, mark))| {
                if !*mark {
                    self.free.insert(id).then(|| *obj = Default::default());
                }
            });
    }
}

impl<T: Default> Default for Pool<T> {
    fn default() -> Self {
        Self::new()
    }
}
