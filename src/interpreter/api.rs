use crate::{interpreter::Gc, DiatomObject, IoWrite, DiatomValue};

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
    pub fn get_obj_mut(&mut self, ref_id: usize) -> Option<&mut DiatomObject<Buffer>> {
        self.gc.get_obj_mut(ref_id)
    }

    /// Same as `get_obj_mut` but this is immutable
    pub fn get_obj(&mut self, ref_id: usize) -> Option<&DiatomObject<Buffer>> {
        self.gc.get_obj(ref_id)
    }

    pub fn print(&self, value: &DiatomValue) -> String {
        self.gc.print(value)
    }
}
