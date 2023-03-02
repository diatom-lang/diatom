use super::*;

pub fn built_in_extension<Buffer: IoWrite>() -> Extension<Buffer> {
    let mut funcs: AHashMap<String, Arc<ForeignFunction<Buffer>>> = AHashMap::default();
    funcs.insert(
        "print".to_string(),
        Arc::new(|state, parameters, out| {
            let mut flag = true;
            for parameter in parameters {
                if flag {
                    flag = false
                } else {
                    write!(out, " ").map_err(|err| format!("IoError: {err}"))?;
                }

                let text = state.print(parameter);
                write!(out, "{text}").map_err(|err| format!("IoError: {err}"))?;
            }
            Ok(DiatomValue::Unit)
        }),
    );
    funcs.insert(
        "println".to_string(),
        Arc::new(|state, parameters, out| {
            let mut flag = true;
            for parameter in parameters {
                if flag {
                    flag = false
                } else {
                    write!(out, " ").map_err(|err| format!("IoError: {err}"))?;
                }

                let text = state.print(parameter);
                write!(out, "{text}").map_err(|err| format!("IoError: {err}"))?;
            }
            writeln!(out).map_err(|err| format!("IoError: {err}"))?;
            Ok(DiatomValue::Unit)
        }),
    );
    funcs.insert(
        "assert".to_string(),
        Arc::new(|_, parameters, _| {
            assure_para_len!(parameters, 1);
            match parameters[0] {
                DiatomValue::Bool(b) => {
                    if b {
                        Ok(DiatomValue::Unit)
                    } else {
                        Err("Assert failed".to_string())
                    }
                }
                _ => Err("Assert on an invalid type that is not bool".to_string()),
            }
        }),
    );
    funcs.insert(
        "panic".to_string(),
        Arc::new(|state, parameters, _| {
            if parameters.len() > 1 {
                return Err(format!(
                    "Expected 1 or 0 parameter while {} is provided",
                    parameters.len()
                ));
            }
            if parameters.is_empty() {
                return Err("Panic triggered here".to_string());
            }
            match parameters[0] {
                DiatomValue::Str(sid) => {
                    let reason = state.get_string_by_id(sid).unwrap().to_string();
                    Err(format!("Panic: `{reason}`"))
                }
                _ => Err(
                    "Panic triggered with invalid type(Can not show non-string parameter)"
                        .to_string(),
                ),
            }
        }),
    );
    funcs.insert(
        "collect".to_string(),
        Arc::new(|state, parameters, _| {
            assure_para_len!(parameters, 0);
            state.collect_garbage();
            Ok(DiatomValue::Unit)
        }),
    );
    funcs.insert(
        "pause".to_string(),
        Arc::new(|state, parameters, _| {
            assure_para_len!(parameters, 0);
            state.pause_gc();
            Ok(DiatomValue::Unit)
        }),
    );
    funcs.insert(
        "resume".to_string(),
        Arc::new(|state, parameters, _| {
            assure_para_len!(parameters, 0);
            state.resume_gc();
            Ok(DiatomValue::Unit)
        }),
    );
    let kind = ExtensionKind::ForeignFunctions(funcs);
    Extension {
        name: "built_in".to_string(),
        kind,
    }
}
