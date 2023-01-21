use std::vec;

use ahash::AHashMap;

use super::types::TypeTable;

/// Create a type
///
/// Note that type parameter must be "a","b",... "z"
macro_rules! dtype {
    ($table:ident, $name:literal[$($para:expr),*]) => {{
        let data_id = $table.data_types().get_by_right($name).unwrap();
        let mut paras = vec![];
        $(
            let t = dtype!($table, $para);
            paras.push(t);
         )*
        crate::vm::types::Type::Poly(*data_id, paras)
    }};
    ($table: ident, $name: literal) => {{
        let c = $name.chars().nth(0).unwrap(); 
        match c {
            '*' => crate::vm::types::Type::Any,
            'a'..='z' =>{
                assert_eq!($name.len(), 1);
                crate::vm::types::Type::PolyFree($name.chars().nth(0).unwrap() as u32 - 'a' as u32)
            }
            _ => {
                let data_id = $table.data_types().get_by_right($name).unwrap();
                crate::vm::types::Type::Mono(*data_id)
            }
        }
    }}
}

macro_rules! init {
    ($table:expr,) => { $table };
    (table : $($tail:tt)*) => {{
        let mut table = crate::vm::types::TypeTable::new();
        init!(table, $($tail)*)
    }};
    ($table:expr, data { $name:expr } $($tail:tt)*) => {{
        $table.new_data_type($name.to_string(), vec![], vec![]).unwrap();
        init!($table, $($tail)*)

    }};
    ($table:expr, class { $name: expr, $para: expr,  $methods: expr, $constraint: expr } $($tail:tt)*) => {{
        $table.new_class();
        init!($table, $($tail)*)
    }}
}

impl Default for TypeTable {
    fn default() -> Self {
        let tt = init! {
             table:
                 data { "Prelude.!" }
                 data { "Prelude.()" }
                 data { "Prelude.Int" }
                 data { "Prelude.Float" }
                 data { "Prelude.Bool" }
                 data { "Prelude.Str" }
        };
        let t = dtype!(tt, "*");
        let t = dtype!(tt, "a");
        let t = dtype!(tt, "Prelude.!"["*", "a"]);
        todo!()
    }
}
