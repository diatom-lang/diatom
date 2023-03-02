use std::sync::Arc;

use ahash::AHashMap;
use chrono::{DateTime, Duration, NaiveDateTime, Utc};
use diatom_core::{
    extension::{Extension, ExtensionKind},
    ffi::{DiatomValue, ForeignFunction},
    IoWrite,
};
use humantime::format_duration;

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

fn time_extension<Buffer: IoWrite>() -> Extension<Buffer> {
    let mut funcs: AHashMap<String, Arc<ForeignFunction<Buffer>>> = AHashMap::default();
    funcs.insert(
        "now".to_string(),
        Arc::new(|_, parameters, _| {
            assure_para_len!(parameters, 0);
            Ok(DiatomValue::Int(Utc::now().timestamp_millis()))
        }),
    );

    funcs.insert(
        "show_as_date".to_string(),
        Arc::new(|state, parameters, _| {
            assure_para_len!(parameters, 1);
            match parameters[0] {
                DiatomValue::Int(i) => {
                    let date = NaiveDateTime::from_timestamp_millis(i)
                        .map(|dt| format!("{}", DateTime::<Utc>::from_utc(dt, Utc)))
                        .unwrap_or("Date out of range!".to_string());
                    let sid = state.create_str(date);
                    Ok(DiatomValue::Str(sid))
                }
                _ => Err("Expected `Int` to operate".to_string()),
            }
        }),
    );

    funcs.insert(
        "show_as_duration".to_string(),
        Arc::new(|state, parameters, _| {
            assure_para_len!(parameters, 1);
            match parameters[0] {
                DiatomValue::Int(i) => {
                    let duration = Duration::milliseconds(i)
                        .to_std()
                        .map(|duration| format_duration(duration).to_string())
                        .unwrap_or_else(|_| "Duration out of range!".to_string());
                    let sid = state.create_str(duration);
                    Ok(DiatomValue::Str(sid))
                }
                _ => Err("Expected `Int` to operate".to_string()),
            }
        }),
    );

    Extension {
        name: "time".to_string(),
        kind: ExtensionKind::ForeignFunctions(funcs),
    }
}

pub fn os_extension<Buffer: IoWrite>() -> Extension<Buffer> {
    Extension {
        name: "os".to_string(),
        kind: ExtensionKind::SubExtensions(vec![time_extension()]),
    }
}
