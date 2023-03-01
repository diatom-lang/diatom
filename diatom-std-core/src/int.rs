use super::*;

pub fn int_extension<Buffer: IoWrite>() -> Extension<Buffer> {
    let mut funcs: AHashMap<String, Arc<ForeignFunction<Buffer>>> = AHashMap::default();
    funcs.insert(
        "MAX".to_string(),
        Arc::new(|_, _, _| {Ok(DiatomValue::Int(i64::MAX))}),
    );

    funcs.insert(
        "MIN".to_string(),
        Arc::new(|_, _, _| {Ok(DiatomValue::Int(i64::MIN))}),
    );

    funcs.insert(
        "abs".to_string(),
        Arc::new(|_, parameters, _| {
            assure_para_len!(parameters, 1);
            match parameters[0] {
                DiatomValue::Int(i) => Ok(DiatomValue::Int(i64::wrapping_abs(i))),
                _ => Err("Expected type `Int`".to_string()),
            }
        }),
    );
    funcs.insert(
        "abs".to_string(),
        Arc::new(|_, parameters, _| {
            assure_para_len!(parameters, 1);
            match parameters[0] {
                DiatomValue::Int(i) => Ok(DiatomValue::Int(i64::wrapping_abs(i))),
                _ => Err("Expected type `Int`".to_string()),
            }
        }),
    );

    funcs.insert(
        "float".to_string(),
        Arc::new(|_, parameters, _| {
            assure_para_len!(parameters, 1);
            match parameters[0] {
                DiatomValue::Int(i) => Ok(DiatomValue::Float(i as f64)),
                _ => Err("Expected type `Int`".to_string()),
            }
        }),
    );

    Extension {
        name: "int".to_string(),
        kind: ExtensionKind::ForeignFunctions(funcs),
    }
}
