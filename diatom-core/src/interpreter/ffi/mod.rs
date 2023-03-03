mod obj;
mod obj_mut;

pub use obj::{DiatomList, DiatomObject, DiatomTable, DiatomTuple};
pub use obj_mut::{DiatomListMut, DiatomObjectMut, DiatomTableMut, DiatomTupleMut};

use std::any::Any;

use crate::{
    ffi::DiatomValue,
    gc::{Gc, GcObject},
    IoWrite,
};

use self::obj_mut::UserDataMut;

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

    pub fn create_user_data(&mut self, data: Box<dyn Any + Send>) -> usize {
        let obj = GcObject::UserData(data);
        self.gc.alloc_obj(obj)
    }

    /// Get a mutable reference by reference id
    ///
    /// Return None if id is invalid. If id is provided by parameters, it can never be invalid and
    /// thus is safe to unwrap.
    pub fn get_obj_mut(&mut self, ref_id: usize) -> Option<DiatomObjectMut<Buffer>> {
        let obj = match self.gc.get_obj_mut(ref_id) {
            None => return None,
            Some(obj) => obj,
        };

        Some(match obj {
            GcObject::Closure { func_id, .. } => DiatomObjectMut::Closure(*func_id),
            GcObject::NativeFunction(_) => DiatomObjectMut::ForeignFunction,
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
            GcObject::UserData(_) => DiatomObjectMut::UserData(UserDataMut {
                gc: self.gc,
                ref_id,
            }),
        })
    }

    /// Same as `get_obj_mut` but this is immutable
    ///
    /// This is much cheaper than a mutable borrow.
    pub fn get_obj(&self, ref_id: usize) -> Option<DiatomObject<Buffer>> {
        let obj = match self.gc.get_obj(ref_id) {
            None => return None,
            Some(obj) => obj,
        };
        Some(match obj {
            GcObject::Closure { func_id, .. } => DiatomObject::Closure(*func_id),
            GcObject::NativeFunction(_) => DiatomObject::ForeignFunction,
            GcObject::List(list) => DiatomObject::List(DiatomList { list, ref_id }),
            GcObject::Table(table) => DiatomObject::Table(DiatomTable {
                gc: self.gc,
                table: &table.attributes,
                ref_id,
            }),
            GcObject::Tuple(tuple) => DiatomObject::Tuple(DiatomTuple { tuple, ref_id }),
            GcObject::UserData(data) => DiatomObject::UserData(data),
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
