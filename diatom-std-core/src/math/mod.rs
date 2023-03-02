use super::*;

macro_rules! math_op_float {
    ($funcs: ident, $name: ident) => {
        $funcs.insert(
            stringify!($name).to_string(),
            Arc::new(|_, parameters, _| {
                assure_para_len!(parameters, 1);
                let f = match parameters[0] {
                    DiatomValue::Int(i) => i as f64,
                    DiatomValue::Float(f) => f,
                    _ => return Err("Expected `Int` or `Float to operate!`".to_string()),
                };
                Ok(DiatomValue::Float(f.$name()))
            }),
        );
    };
}

pub fn math_extension<Buffer: IoWrite>() -> Extension<Buffer> {
    let mut funcs: AHashMap<String, Arc<ForeignFunction<Buffer>>> = AHashMap::default();
    math_op_float!(funcs, sqrt);
    math_op_float!(funcs, cbrt);
    math_op_float!(funcs, sin);
    math_op_float!(funcs, cos);
    math_op_float!(funcs, tan);
    math_op_float!(funcs, sinh);
    math_op_float!(funcs, cosh);
    math_op_float!(funcs, tanh);
    math_op_float!(funcs, ln);
    math_op_float!(funcs, log2);
    math_op_float!(funcs, log10);
    math_op_float!(funcs, asin);
    math_op_float!(funcs, acos);
    math_op_float!(funcs, atan);
    math_op_float!(funcs, asinh);
    math_op_float!(funcs, acosh);
    math_op_float!(funcs, atanh);
    Extension {
        name: "math".to_string(),
        kind: ExtensionKind::ForeignFunctions(funcs),
    }
}
