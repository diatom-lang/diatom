use lazy_static::lazy_static;

lazy_static! {
    static ref classes: Vec<&'static str> = vec!["Add", "Sub", "Mul", "Div", "IDiv", "Mod", "Pow",];
    static ref data_types: Vec<(String,)> = vec![];
}
