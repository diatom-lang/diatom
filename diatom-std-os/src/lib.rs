use std::sync::Arc;

use ahash::AHashMap;
use diatom_core::{
    extension::{Extension, ExtensionKind},
    ffi::{DiatomObject, DiatomValue, ForeignFunction, State},
    IoWrite,
};
use humantime::format_duration;
use time::{Duration, OffsetDateTime};

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

fn cast_to_offset_date<'a, Buffer: IoWrite>(
    state: &'a State<Buffer>,
    value: &DiatomValue,
) -> Option<&'a OffsetDateTime> {
    if let DiatomValue::Ref(rid) = value {
        if let Some(DiatomObject::UserData(b)) = state.get_obj(*rid) {
            let date_time: Option<&OffsetDateTime> = b.downcast_ref();
            return date_time;
        }
    }
    None
}

fn cast_to_duration<'a, Buffer: IoWrite>(
    state: &'a State<Buffer>,
    value: &DiatomValue,
) -> Option<&'a Duration> {
    if let DiatomValue::Ref(rid) = value {
        if let Some(DiatomObject::UserData(b)) = state.get_obj(*rid) {
            let duration: Option<&Duration> = b.downcast_ref();
            return duration;
        }
    }
    None
}

fn time_util_extension<Buffer: IoWrite>() -> Extension<Buffer> {
    let mut funcs: AHashMap<String, Arc<ForeignFunction<Buffer>>> = AHashMap::default();
    funcs.insert(
        "now".to_string(),
        Arc::new(|state, parameters, _| {
            assure_para_len!(parameters, 0);
            let date_time = OffsetDateTime::now_utc();
            let rid = state.create_user_data(Box::new(date_time));
            Ok(DiatomValue::Ref(rid))
        }),
    );

    funcs.insert(
        "show_date_time".to_string(),
        Arc::new(|state, parameters, _| {
            assure_para_len!(parameters, 1);
            if let Some(date_time) = cast_to_offset_date(state, &parameters[0]) {
                let s = format!("{date_time}");
                let sid = state.create_str(s);
                return Ok(DiatomValue::Str(sid));
            }
            Err("Expected a `DateTime` to operate".to_string())
        }),
    );

    funcs.insert(
        "duration".to_string(),
        Arc::new(|state, parameter, _| {
            assure_para_len!(parameter, 2);
            if let Some(d1) = cast_to_offset_date(state, &parameter[0]) {
                if let Some(d2) = cast_to_offset_date(state, &parameter[1]) {
                    let duration = *d2 - *d1;
                    let duration = state.create_user_data(Box::new(duration));
                    return Ok(DiatomValue::Ref(duration));
                }
            };
            Err("Expected two DateTime objects to operate".to_string())
        }),
    );

    funcs.insert(
        "show_duration".to_string(),
        Arc::new(|state, parameters, _| {
            assure_para_len!(parameters, 1);
            if let Some(duration) = cast_to_duration(state, &parameters[0]) {
                let duration = std::time::Duration::try_from(*duration)
                    .map_or_else(|err| format!("{err}"), |d| format_duration(d).to_string());
                let s = duration;
                let sid = state.create_str(s);
                return Ok(DiatomValue::Str(sid));
            }
            Err("Expected a `Duration` to operate".to_string())
        }),
    );

    Extension {
        name: "util".to_string(),
        kind: ExtensionKind::ForeignFunctions(funcs),
    }
}

fn time_extension<Buffer: IoWrite>() -> Extension<Buffer> {
    Extension {
        name: "time".to_string(),
        kind: ExtensionKind::SubExtensions(vec![
            time_util_extension(),
            Extension {
                name: "mod".to_string(),
                kind: ExtensionKind::File(include_str!("time.dm").to_string()),
            },
        ]),
    }
}

pub fn os_extension<Buffer: IoWrite>() -> Extension<Buffer> {
    Extension {
        name: "os".to_string(),
        kind: ExtensionKind::SubExtensions(vec![time_extension()]),
    }
}
