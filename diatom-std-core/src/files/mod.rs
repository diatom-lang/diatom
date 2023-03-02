macro_rules! prelude {
    ($name: literal) => {
        (concat!("prelude/", $name), include_str!($name))
    };
}

pub static PRELUDE_FILES: [(&str, &str); 4] = [
    prelude!("prelude.dm"),
    prelude!("option.dm"),
    prelude!("iter.dm"),
    prelude!("range.dm"),
];
