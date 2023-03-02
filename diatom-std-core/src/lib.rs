mod built_in;
mod files;
mod float;
mod int;
mod list;
mod string;

use std::sync::Arc;

use ahash::AHashMap;
use diatom_core::{
    extension::{Extension, ExtensionKind},
    ffi::{DiatomValue, ForeignFunction},
    IoWrite, StdCore,
};

static PRELUDE_NAMES: [&str; 17] = [
    "print",
    "println",
    "todo",
    "assert",
    "unreachable",
    "panic",
    "List",
    "Int",
    "Float",
    "String",
    "Table",
    "Iter",
    "Range",
    "Option",
    "Some",
    "None",
    "Gc",
];

pub struct StdLibCore;

impl StdCore for StdLibCore {
    fn prelude_names() -> &'static [&'static str] {
        &PRELUDE_NAMES
    }

    fn prelude_files() -> &'static [(&'static str, &'static str)] {
        &files::PRELUDE_FILES
    }

    fn prelude_extension<Buffer: IoWrite>() -> Extension<Buffer> {
        Extension {
            name: "prelude".to_string(),
            kind: ExtensionKind::SubExtensions(vec![
                built_in::built_in_extension(),
                int::int_extension(),
                float::float_extension(),
                list::list_extension(),
            ]),
        }
    }
}

macro_rules! assure_para_len {
    ($parameters: ident, $len: literal) => {
        if $parameters.len() != $len {
            return Err(format!(
                "Expected {} parameter while {} is provided",
                $len,
                $parameters.len()
            ));
        }
    };
}

pub(crate) use assure_para_len;
