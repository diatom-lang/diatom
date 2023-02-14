use std::io::Write;

use crate::{DiatomValue, Interpreter};

pub fn impl_prelude<Buffer: Write>(interpreter: &mut Interpreter<Buffer>) {
    interpreter.add_extern_function("print".to_string(), |state, parameters, out| {
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
    });

    interpreter.add_extern_function("assert".to_string(), |_state, parameters, _| {
        if parameters.len() != 1 {
            return Err(format!(
                "Expected 1 parameter while {} is provided",
                parameters.len()
            ));
        }
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
    });

    interpreter.add_extern_function("panic".to_string(), |state, parameters, _| {
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
                "Panic triggered with invalid type(Can not show non-string parameter)".to_string(),
            ),
        }
    });

    interpreter.add_extern_function("unreachable".to_string(), |_, parameters, _| {
        if !parameters.is_empty() {
            Err(format!(
                "expect 0 parameter while {} is provided",
                parameters.len()
            ))
        } else {
            Err("Unreachable code reached".to_string())
        }
    });
}
