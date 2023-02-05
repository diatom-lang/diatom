use crate::vm::Vm;

/// State of the virtual machine
pub struct State<'a> {
    pub(crate) vm: &'a mut Vm,
}

impl<'a> State<'a> {
    /// Print output to virtual machine's output buffer
    ///
    /// Note: this function does not print any thing to stdout/stderr
    pub fn print(&mut self, s: &str) {
        self.vm.output.push_str(s);
    }

    /// Get string by string id
    ///
    /// Return None if id is invalid. If id is provided by parameters, it can never be invalid and
    /// thus is safe to unwrap.
    pub fn get_string_by_id(&self, id: usize) -> Option<&str> {
        self.vm.gc.get_string_by_id_checked(id)
    }

    /// Create a new string
    ///
    /// Return reference id to the string which can be put into `DiatomValue::Str()`.
    pub fn create_str(&mut self, s: String) -> usize {
        self.vm.gc.string_pool().alloc(s)
    }
}
