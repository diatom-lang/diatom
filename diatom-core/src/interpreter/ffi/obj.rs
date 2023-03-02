use super::*;

/// Immutable reference to a diatom table
pub struct DiatomTable<'a, Buffer: IoWrite> {
    pub(super) gc: &'a Gc<Buffer>,
    pub(super) ref_id: usize,
}

impl<'a, Buffer: IoWrite> DiatomTable<'a, Buffer> {
    pub fn ref_id(&self) -> usize {
        self.ref_id
    }

    pub fn get_field(&self, name: impl AsRef<str>) -> Option<DiatomValue> {
        let key_id = match self.gc.get_table_key(name.as_ref()) {
            Some(id) => id,
            None => return None,
        };
        let table = self.gc.get_obj(self.ref_id).unwrap();
        match table {
            GcObject::Table(t) => t.attributes.get(&key_id).cloned(),
            _ => unreachable!(),
        }
    }

    pub fn fields(&self) -> Keys<usize, DiatomValue> {
        let table = self.gc.get_obj(self.ref_id).unwrap();
        match table {
            GcObject::Table(t) => t.attributes.keys(),
            _ => unreachable!(),
        }
    }
}

/// Immutable reference to a diatom list
pub struct DiatomList<'a, Buffer: IoWrite> {
    pub(super) gc: &'a Gc<Buffer>,
    pub(super) ref_id: usize,
}

impl<'a, Buffer: IoWrite> DiatomList<'a, Buffer> {
    pub fn ref_id(&self) -> usize {
        self.ref_id
    }

    pub fn get(&self, idx: usize) -> Option<DiatomValue> {
        let list = self.gc.get_obj(self.ref_id).unwrap();
        match list {
            GcObject::List(l) => l.get(idx).cloned(),
            _ => unreachable!(),
        }
    }

    pub fn len(&self) -> usize {
        let list = self.gc.get_obj(self.ref_id).unwrap();
        match list {
            GcObject::List(l) => l.len(),
            _ => unreachable!(),
        }
    }

    pub fn is_empty(&self) -> bool {
        let list = self.gc.get_obj(self.ref_id).unwrap();
        match list {
            GcObject::List(l) => l.is_empty(),
            _ => unreachable!(),
        }
    }
}

/// Immutable reference to diatom tuple
pub struct DiatomTuple<'a, Buffer: IoWrite> {
    pub(super) gc: &'a Gc<Buffer>,
    pub(super) ref_id: usize,
}

impl<'a, Buffer: IoWrite> DiatomTuple<'a, Buffer> {
    pub fn ref_id(&self) -> usize {
        self.ref_id
    }

    pub fn get(&self, idx: usize) -> Option<DiatomValue> {
        let list = self.gc.get_obj(self.ref_id).unwrap();
        match list {
            GcObject::List(l) => l.get(idx).cloned(),
            _ => unreachable!(),
        }
    }

    pub fn len(&self) -> usize {
        let list = self.gc.get_obj(self.ref_id).unwrap();
        match list {
            GcObject::List(l) => l.len(),
            _ => unreachable!(),
        }
    }

    pub fn is_empty(&self) -> bool {
        let list = self.gc.get_obj(self.ref_id).unwrap();
        match list {
            GcObject::List(l) => l.is_empty(),
            _ => unreachable!(),
        }
    }
}

/// Immutable reference to a diatom heap allocated object
pub enum DiatomObject<'a, Buffer: IoWrite> {
    /// Native Diatom Closure (Contains its id)
    Closure(usize),
    /// Foreign rust closure
    ForeignFunction(&'a Arc<ForeignFunction<Buffer>>),
    /// Table
    Table(DiatomTable<'a, Buffer>),
    /// Tuple
    Tuple(DiatomTuple<'a, Buffer>),
    /// List
    List(DiatomList<'a, Buffer>),
}
