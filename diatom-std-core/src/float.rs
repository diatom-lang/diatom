use super::*;

macro_rules! load_func_static {
    ($funcs: ident, $name: ident, $ret: expr) => {
        $funcs.insert(stringify!($name).to_string(), Arc::new(|_, _, _| Ok($ret)));
    };
}

macro_rules! load_func_1 {
    ($funcs: ident, $name: ident, $ret: expr) => {
        $funcs.insert(
            stringify!($name).to_string(),
            Arc::new(|_, parameters, _| {
                assure_para_len!(parameters, 1);
                match parameters[0] {
                    DiatomValue::Float(f) => Ok($ret(f)),
                    _ => Err("Expected type `Float`".to_string()),
                }
            }),
        );
    };
}

pub fn float_extension<Buffer: IoWrite>() -> Extension<Buffer> {
    let mut funcs: AHashMap<String, Arc<ForeignFunction<Buffer>>> = AHashMap::default();
    load_func_static!(funcs, MAX, DiatomValue::Float(f64::MAX));
    load_func_static!(funcs, MIN, DiatomValue::Float(f64::MIN));
    load_func_static!(funcs, INF, DiatomValue::Float(f64::INFINITY));
    load_func_static!(funcs, NEG_INF, DiatomValue::Float(f64::NEG_INFINITY));
    load_func_static!(funcs, NAN, DiatomValue::Float(f64::NAN));

    load_func_1!(funcs, abs, |f: f64| DiatomValue::Float(f.abs()));
    load_func_1!(funcs, floor, |f: f64| DiatomValue::Float(f.floor()));
    load_func_1!(funcs, ceil, |f: f64| DiatomValue::Float(f.ceil()));
    load_func_1!(funcs, int, |f: f64| DiatomValue::Int(f as i64));
    load_func_1!(funcs, round, |f: f64| DiatomValue::Float(f.round()));
    load_func_1!(funcs, is_nan, |f: f64| DiatomValue::Bool(f.is_nan()));
    load_func_1!(funcs, is_inf, |f: f64| DiatomValue::Bool(f.is_infinite()));

    Extension {
        name: "float".to_string(),
        kind: ExtensionKind::ForeignFunctions(funcs),
    }
}
