use crate::{interpreter::Gc, DiatomObject, IoWrite};

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
        self.gc.get_string_by_id_checked(id)
    }

    /// Create a new string
    ///
    /// Return reference id to the string which can be put into `DiatomValue::Str()`.
    pub fn create_str(&mut self, s: String) -> usize {
        self.gc.string_pool().alloc(s)
    }

    /// Get a mutable reference by reference id
    ///
    /// Return None if id is invalid. If id is provided by parameters, it can never be invalid and
    /// thus is safe to unwrap.
    pub fn get_obj_by_ref(&mut self, ref_id: usize) -> Option<&mut DiatomObject<Buffer>> {
        self.gc.get_obj_by_ref(ref_id)
    }
}
