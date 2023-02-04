use std::fmt::Write;

use crate::{vm::Vm, DiatomValue, Interpreter};

pub fn impl_prelude(interpreter: &mut Interpreter) {
    let print = |vm: &mut Vm, parameters: &[DiatomValue]| {
        let mut output = String::new();
        for parameter in parameters {
            match parameter {
                DiatomValue::Unit => write!(output, "()").unwrap(),
                DiatomValue::Bool(b) => write!(output, "{b}").unwrap(),
                DiatomValue::Int(i) => write!(output, "{i}").unwrap(),
                DiatomValue::Float(f) => write!(output, "{f}").unwrap(),
                DiatomValue::Str(sid) => {
                    write!(output, "{}", vm.get_string_by_id(*sid).unwrap()).unwrap()
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
        vm.print(&output);
        Ok(DiatomValue::Unit)
    };
    interpreter.add_extern_function("print".to_string(), print);

    let assert = |_vm: &mut Vm, parameters: &[DiatomValue]| {
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
}
