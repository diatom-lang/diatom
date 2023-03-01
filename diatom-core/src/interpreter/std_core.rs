use std::sync::Arc;

use ahash::AHashMap;

use crate::ffi::ForeignFunction;
use crate::IoWrite;

/// Interface with standard library core
pub trait StdCore: Send {
    /// Names should be defined in prelude
    fn prelude_names() -> &'static [&'static str];
    /// Prelude files to execute right after interpreter initialized
    ///
    /// Return [(`file_name`, `file_content`)]
    fn prelude_files() -> &'static [(&'static str, &'static str)];
    /// Prelude modules
    fn prelude_extension<Buffer: IoWrite>() -> Extension<Buffer>;
}

/// Kind of extension
pub enum ExtensionKind<Buffer: IoWrite> {
    /// A Set of Foreign functions and their names.
    ForeignFunctions(AHashMap<String, Arc<ForeignFunction<Buffer>>>),
    /// Use diatom source code as extension.
    File(String),
    /// Sub extensions.
    SubExtensions(Vec<Extension<Buffer>>),
}

/// # Diatom rust extension
///
/// Each extension has a name, which is used to identify extension when being imported. For
/// example, `import my_ext.sub_ext` will automatically look up base extension `my_ext` and its sub
/// extension `sub_ext`.
pub struct Extension<Buffer: IoWrite> {
    /// # Name of this extension
    ///
    /// # Note:
    /// - Sub extensions with the same name will **OVERRIDE** each other.
    /// - If name is not a valid diatom identifier this extension can **NOT** be imported.  
    /// - Name must not contain `/`, otherwise will be ignore this extension.
    /// - If the name is `mod` then the extension will be load when parent module is imported. (The
    /// same way `<module>/mod.dm` works)
    ///
    /// # For base extension
    /// Base extensions are the extension that directly registered to the interpreter. Their names
    /// must be unique (will otherwise rejected by the interpreter). Also, base extension names
    /// will hijack **ANY**  import starts with its name. For example, an extension with name
    /// `my_ext` will cause all `import my_ext.<anything here>` to be resolved to the extension.
    /// This will make local file like `my_ext.dm` inaccessible.
    ///
    pub name: String,
    /// The kind of this extension
    pub kind: ExtensionKind<Buffer>,
}
