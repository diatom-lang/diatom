use std::{any::Any, collections::BTreeMap};

use super::*;

/// Immutable reference to a diatom table
pub struct DiatomTable<'a, Buffer: IoWrite> {
    pub(super) gc: &'a Gc<Buffer>,
    pub(super) table: &'a BTreeMap<usize, DiatomValue>,
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
        self.table.get(&key_id).cloned()
    }

    pub fn fields(&self) -> Vec<&str> {
        let mut fields = vec![];
        self.table.keys().for_each(|k|{
            let field = self.gc.look_up_table_key(*k).unwrap();
            fields.push(field);
        });
        fields
    }
}

/// Immutable reference to a diatom list
pub struct DiatomList<'a> {
    pub(super) list: &'a [DiatomValue],
    pub(super) ref_id: usize,
}

impl<'a> DiatomList<'a> {
    pub fn ref_id(&self) -> usize {
        self.ref_id
    }

    pub fn get(&self, idx: usize) -> Option<DiatomValue> {
        self.list.get(idx).cloned()
    }

    pub fn len(&self) -> usize {
        self.list.len()
    }

    pub fn is_empty(&self) -> bool {
        self.list.is_empty()
    }
}

/// Immutable reference to diatom tuple
pub struct DiatomTuple<'a> {
    pub(super) tuple: &'a [DiatomValue],
    pub(super) ref_id: usize,
}

impl<'a> DiatomTuple<'a> {
    pub fn ref_id(&self) -> usize {
        self.ref_id
    }

    pub fn get(&self, idx: usize) -> Option<DiatomValue> {
        self.tuple.get(idx).cloned()
    }

    pub fn len(&self) -> usize {
        self.tuple.len()
    }

    pub fn is_empty(&self) -> bool {
        self.tuple.is_empty()
    }
}

/// Immutable reference to a diatom heap allocated object
pub enum DiatomObject<'a, Buffer: IoWrite> {
    /// Native Diatom Closure (Contains its id)
    Closure(usize),
    /// Foreign rust closure
    ForeignFunction,
    /// Table
    Table(DiatomTable<'a, Buffer>),
    /// Tuple
    Tuple(DiatomTuple<'a>),
    /// List
    List(DiatomList<'a>),
    /// UserData
    UserData(&'a Box<dyn Any + Send>),
}
