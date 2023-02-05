use std::fmt::Write;

use crate::{DiatomValue, Interpreter, State};

pub fn impl_prelude(interpreter: &mut Interpreter) {
    let print = |state: &mut State, parameters: &[DiatomValue]| {
        let mut output = String::new();
        for parameter in parameters {
            match parameter {
                DiatomValue::Unit => write!(output, "()").unwrap(),
                DiatomValue::Bool(b) => write!(output, "{b}").unwrap(),
                DiatomValue::Int(i) => write!(output, "{i}").unwrap(),
                DiatomValue::Float(f) => write!(output, "{f}").unwrap(),
                DiatomValue::Str(sid) => {
                    write!(output, "{}", state.get_string_by_id(*sid).unwrap()).unwrap()
                }
                DiatomValue::Ref(rid) => write!(output, "Object@<{rid}>").unwrap(),
            }
            write!(output, ", ").unwrap();
        }
        if output.len() >= 2 {
            output.pop();
            output.pop();
        }
        output.push('\n');
        state.print(&output);
        Ok(DiatomValue::Unit)
    };
    interpreter.add_extern_function("print".to_string(), print);

    let assert = |_state: &mut State, parameters: &[DiatomValue]| {
        if parameters.len() != 1 {
            return Err(format!(
                "Assert expected 1 parameter while {} is provided",
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
    };
    interpreter.add_extern_function("assert".to_string(), assert);

    let panic = |state: &mut State, parameters: &[DiatomValue]| {
        if parameters.len() > 1 {
            return Err(format!(
                "Assert expected 1 or 0 parameter while {} is provided",
                parameters.len()
            ));
        }
        if parameters.is_empty() {
            return Err("Panic triggered here".to_string());
        }
        match parameters[0] {
            DiatomValue::Str(sid) => {
                let reason = state.get_string_by_id(sid).unwrap().to_string();
                Err(format!("Panic triggered: `{reason}`"))
            }
            _ => Err(
                "Panic triggered with invalid type(Can not show non-string parameter)".to_string(),
            ),
        }
    };
    interpreter.add_extern_function("panic".to_string(), panic);
}
