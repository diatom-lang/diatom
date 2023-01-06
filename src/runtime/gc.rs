use std::ops::{Index, IndexMut};

type GcId = usize;

enum Mark {
    White,
    Gray,
    Black,
}
/// Garbage Collector
#[derive(Default)]
pub struct Gc<T> {
    /// Object, IsRoot, Mark
    pool: Vec<(T, bool, Mark)>,
    free: Vec<usize>,
}

impl<T> Gc<T> {
    pub fn new() -> Self {
        Self {
            pool: vec![],
            free: vec![],
        }
    }

    pub fn alloc(&mut self, obj: T) -> GcId {
        if self.free.is_empty() {
            self.pool.push((obj, false, Mark::White));
            self.pool.len() - 1
        } else {
            let id = self.free.pop().unwrap();
            id
        }
    }

    pub fn mark_root(&mut self, id: GcId) {
        self.pool[id].1 = true;
        self.pool[id].2 = Mark::Black;
    }

    pub fn unmark_root(&mut self, id: GcId) {
        self.pool[id].1 = false;
        self.pool[id].2 = Mark::White;
    }
}

impl<T> Index<GcId> for Gc<T> {
    type Output = T;
    fn index(&self, index: GcId) -> &Self::Output {
        &self.pool[index].0
    }
}

impl<T> IndexMut<GcId> for Gc<T> {
    fn index_mut(&mut self, index: GcId) -> &mut Self::Output {
        &mut self.pool[index].0
    }
}
