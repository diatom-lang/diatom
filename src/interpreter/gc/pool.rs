use std::collections::BTreeSet;

pub struct Pool<T> {
    pool: Vec<T>,
    free: BTreeSet<usize>,
}

impl<T> Pool<T> {
    pub fn new() -> Self {
        Self {
            pool: vec![],
            free: BTreeSet::new(),
        }
    }

    pub fn alloc(&mut self, value: T) -> usize {
        if self.free.is_empty() {
            self.pool.push(value);
            self.pool.len() - 1
        } else {
            self.free.pop_last().unwrap()
        }
    }

    pub fn get(&self, idx: usize) -> Option<&T> {
        if idx < self.pool.len() && self.free.get(&idx).is_none() {
            Some(&self.pool[idx])
        } else {
            None
        }
    }

    pub unsafe fn get_unchecked(&self, idx: usize) -> &T {
        debug_assert!(self.pool.len() > idx && self.free.get(&idx).is_none());
        self.pool.get_unchecked(idx)
    }

    pub unsafe fn get_unchecked_mut(&mut self, idx: usize) -> &mut T {
        debug_assert!(self.pool.len() > idx && self.free.get(&idx).is_none());
        self.pool.get_unchecked_mut(idx)
    }

    pub fn get_mut(&mut self, idx: usize) -> Option<&mut T> {
        if idx < self.pool.len() && self.free.get(&idx).is_none() {
            Some(&mut self.pool[idx])
        } else {
            None
        }
    }
}

impl<T> Default for Pool<T> {
    fn default() -> Self {
        Self::new()
    }
}
