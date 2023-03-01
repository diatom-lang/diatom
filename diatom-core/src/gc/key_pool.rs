use bimap::BiHashMap;

#[derive(Default)]
pub struct KeyPool {
    pool: BiHashMap<String, usize>,
}

impl KeyPool {
    pub fn get_or_insert(&mut self, key: impl Into<String> + AsRef<str>) -> usize {
        self.pool
            .get_by_left(key.as_ref())
            .copied()
            .unwrap_or_else(|| {
                self.pool.insert(key.into(), self.pool.len());
                self.pool.len() - 1
            })
    }

    pub fn look_up_key(&self, id: usize) -> Option<&str> {
        self.pool.get_by_right(&id).map(|s| s.as_str())
    }

    pub fn get_key(&self, key: impl AsRef<str>) -> Option<usize> {
        self.pool.get_by_left(key.as_ref()).cloned()
    }
}
