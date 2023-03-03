use std::any::Any;

use super::*;

fn check_value<Buffer: IoWrite>(gc: &Gc<Buffer>, value: &DiatomValue) -> bool {
    match value {
        DiatomValue::Ref(rid) => gc.get_obj(*rid).is_some(),
        DiatomValue::Str(sid) => gc.get_str(*sid).is_some(),
        _ => true,
    }
}

/// Mutable reference to a diatom table
pub struct DiatomTableMut<'a, Buffer: IoWrite> {
    pub(super) gc: &'a mut Gc<Buffer>,
    pub(super) ref_id: usize,
}

impl<'a, Buffer: IoWrite> DiatomTableMut<'a, Buffer> {
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

    pub fn fields(&self) -> Vec<&str> {
        let table = self.gc.get_obj(self.ref_id).unwrap();
        match table {
            GcObject::Table(t) => {
                let mut fields = vec![];
                t.attributes.keys().for_each(|k| {
                    let field = self.gc.look_up_table_key(*k).unwrap();
                    fields.push(field);
                });
                fields
            }
            _ => unreachable!(),
        }
    }
}

/// Mutable reference to a diatom table
pub struct DiatomListMut<'a, Buffer: IoWrite> {
    pub(super) gc: &'a mut Gc<Buffer>,
    pub(super) ref_id: usize,
}

impl<'a, Buffer: IoWrite> DiatomListMut<'a, Buffer> {
    fn to_list_mut(&mut self) -> &mut Vec<DiatomValue> {
        let list = self.gc.get_obj_mut(self.ref_id).unwrap();
        match list {
            GcObject::List(l) => l,
            _ => unreachable!(),
        }
    }

    fn to_list(&self) -> &Vec<DiatomValue> {
        let list = self.gc.get_obj(self.ref_id).unwrap();
        match list {
            GcObject::List(l) => l,
            _ => unreachable!(),
        }
    }

    pub fn ref_id(&self) -> usize {
        self.ref_id
    }

    pub fn get(&self, idx: usize) -> Option<DiatomValue> {
        self.to_list().get(idx).cloned()
    }

    pub fn len(&self) -> usize {
        self.to_list().len()
    }

    pub fn is_empty(&self) -> bool {
        self.to_list().is_empty()
    }

    /// Return true if value is valid
    pub fn push(&mut self, value: DiatomValue) -> bool {
        if !check_value(self.gc, &value) {
            return false;
        }
        self.to_list_mut().push(value);
        true
    }

    /// Clear the whole list
    pub fn clear(&mut self) {
        self.to_list_mut().clear()
    }

    pub fn reverse(&mut self) {
        self.to_list_mut().reverse()
    }

    pub fn pop(&mut self) -> Option<DiatomValue> {
        self.to_list_mut().pop()
    }

    /// Set value at index, return false if idx out of bound or value is not valid
    pub fn set_idx(&mut self, idx: usize, value: DiatomValue) -> bool {
        if !check_value(self.gc, &value) {
            return false;
        }
        let l = self.to_list_mut();
        if l.len() <= idx {
            return false;
        }
        l[idx] = value;
        true
    }

    /// Insert value at index, return false if idx out of bound or value is not valid
    pub fn insert(&mut self, idx: usize, value: DiatomValue) -> bool {
        if !check_value(self.gc, &value) {
            return false;
        }
        let l = self.to_list_mut();
        if l.len() < idx {
            return false;
        }
        l.insert(idx, value);
        true
    }

    /// Remove value at index, return false if idx out of bound or value is not valid
    pub fn remove(&mut self, idx: usize) -> bool {
        let l = self.to_list_mut();
        if l.len() <= idx {
            return false;
        }
        l.remove(idx);
        true
    }
}

/// Mutable reference to a diatom table
pub struct DiatomTupleMut<'a, Buffer: IoWrite> {
    pub(super) gc: &'a mut Gc<Buffer>,
    pub(super) ref_id: usize,
}

impl<'a, Buffer: IoWrite> DiatomTupleMut<'a, Buffer> {
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

pub struct UserDataMut<'a, Buffer: IoWrite> {
    pub(super) gc: &'a mut Gc<Buffer>,
    pub(super) ref_id: usize,
}

impl<'a, Buffer: IoWrite> UserDataMut<'a, Buffer> {
    pub fn ref_id(&self) -> usize {
        self.ref_id
    }

    pub fn get(&mut self) -> &mut Box<dyn Any + Send> {
        let table = self.gc.get_obj_mut(self.ref_id).unwrap();
        match table {
            GcObject::UserData(data) => data,
            _ => unreachable!(),
        }
    }
}

/// Mutable reference to a diatom heap allocated object
pub enum DiatomObjectMut<'a, Buffer: IoWrite> {
    /// Native Diatom Closure
    Closure(usize),
    /// Foreign rust closure
    ForeignFunction,
    /// Table
    Table(DiatomTableMut<'a, Buffer>),
    /// Tuple
    Tuple(DiatomTupleMut<'a, Buffer>),
    /// List
    List(DiatomListMut<'a, Buffer>),
    /// UserData
    UserData(UserDataMut<'a, Buffer>),
}
