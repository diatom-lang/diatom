use std::ffi::OsStr;

use crate::{Interpreter, IoWrite};

macro_rules! load_lib {
    ($interpreter: ident, $lib: literal) => {
        if let Err(err) = $interpreter.exec(
            include_str!($lib),
            OsStr::new(concat!("prelude/", $lib)),
            true,
        ) {
            print!("{err}");
            panic!("Standard library failed to load: `prelude/{}`", $lib);
        }
    };
}

pub fn load_prelude<Buffer: IoWrite>(interpreter: &mut Interpreter<Buffer>) {
    // NOTE LOAD ORDER MATTERS HERE!
    load_lib!(interpreter, "option.dm");
    load_lib!(interpreter, "iter.dm");
    load_lib!(interpreter, "list.dm");
    load_lib!(interpreter, "range.dm");
}
