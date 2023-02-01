use std::ops::Index;

/// Immutable String Pool without Interning
#[derive(Default)]
pub struct StringPool {
    pool: Vec<String>,
    free: Vec<usize>,
}

impl StringPool {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc(&mut self, s: String) -> usize {
        if self.free.is_empty() {
            self.pool.push(s);
            self.pool.len() - 1
        } else {
            let id = self.free.pop().unwrap();
            self.pool[id] = s;
            id
        }
    }

    pub fn _free(&mut self, id: usize) {
        debug_assert!(id < self.pool.len());
        self.free.push(id)
    }
}

impl Index<usize> for StringPool {
    type Output = String;
    fn index(&self, id: usize) -> &Self::Output {
        &self.pool[id]
    }
}
