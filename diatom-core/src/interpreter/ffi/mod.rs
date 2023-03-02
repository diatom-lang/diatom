mod obj;
mod obj_mut;

pub use obj::{DiatomList, DiatomObject, DiatomTable, DiatomTuple};
pub use obj_mut::{DiatomListMut, DiatomObjectMut, DiatomTableMut, DiatomTupleMut};

use std::collections::btree_map::Keys;
use std::sync::Arc;

use crate::{
    ffi::{DiatomValue, ForeignFunction},
    gc::{Gc, GcObject},
    IoWrite,
};

/// State of the virtual machine
pub struct State<'a, Buffer: IoWrite> {
    pub(crate) gc: &'a mut Gc<Buffer>,
}

impl<'a, Buffer: IoWrite> State<'a, Buffer> {
    /// Get string by string id
    ///
    /// Return None if id is invalid. If id is provided by parameters, it can never be invalid and
    /// thus is safe to unwrap.
    pub fn get_string_by_id(&self, id: usize) -> Option<&str> {
        self.gc.get_str(id)
    }

    /// Create a new string
    ///
    /// Return reference id to the string which can be put into `DiatomValue::Str()`.
    pub fn create_str(&mut self, s: String) -> usize {
        self.gc.alloc_str(s)
    }

    /// Get a mutable reference by reference id
    ///
    /// Return None if id is invalid. If id is provided by parameters, it can never be invalid and
    /// thus is safe to unwrap.
    pub fn get_obj_mut(&mut self, ref_id: usize) -> Option<DiatomObjectMut<Buffer>> {
        let obj = match self.gc.get_obj(ref_id) {
            None => return None,
            Some(obj) => obj,
        };

        Some(match obj {
            GcObject::Closure { func_id, .. } => DiatomObjectMut::Closure(*func_id),
            GcObject::NativeFunction(f) => DiatomObjectMut::ForeignFunction(f.clone()),
            GcObject::List(_) => DiatomObjectMut::List(DiatomListMut {
                gc: self.gc,
                ref_id,
            }),
            GcObject::Table(_) => DiatomObjectMut::Table(DiatomTableMut {
                gc: self.gc,
                ref_id,
            }),
            GcObject::Tuple(_) => DiatomObjectMut::Tuple(DiatomTupleMut {
                gc: self.gc,
                ref_id,
            }),
        })
    }

    /// Same as `get_obj_mut` but this is immutable
    pub fn get_obj(&self, ref_id: usize) -> Option<DiatomObject<Buffer>> {
        let obj = match self.gc.get_obj(ref_id) {
            None => return None,
            Some(obj) => obj,
        };
        Some(match obj {
            GcObject::Closure { func_id, .. } => DiatomObject::Closure(*func_id),
            GcObject::NativeFunction(f) => DiatomObject::ForeignFunction(f),
            GcObject::List(_) => DiatomObject::List(DiatomList {
                gc: self.gc,
                ref_id,
            }),
            GcObject::Table(_) => DiatomObject::Table(DiatomTable {
                gc: self.gc,
                ref_id,
            }),
            GcObject::Tuple(_) => DiatomObject::Tuple(DiatomTuple {
                gc: self.gc,
                ref_id,
            }),
        })
    }

    pub fn print(&self, value: &DiatomValue) -> String {
        self.gc.print(value)
    }

    /// Immediately collect garbage
    pub fn collect_garbage(&mut self) {
        self.gc.collect()
    }

    /// Pause garbage collection
    ///
    /// Do nothing if gc is already paused.
    pub fn pause_gc(&mut self) {
        self.gc.pause()
    }

    /// Resume garbage collection
    ///
    /// Do nothing if gc is not paused.
    pub fn resume_gc(&mut self) {
        self.gc.resume()
    }
}
