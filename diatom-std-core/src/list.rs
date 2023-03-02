use diatom_core::ffi::{DiatomList, DiatomListMut, DiatomObject, DiatomObjectMut};

use super::*;

macro_rules! load_func {
    ($funcs: ident, $name: ident, $ret: expr) => {
        $funcs.insert(
            stringify!($name).to_string(),
            Arc::new(|state, parameters, _| {
                assure_para_len!(parameters, 1);
                match parameters[0] {
                    DiatomValue::Ref(id) => match state.get_obj(id) {
                        Some(DiatomObject::List(l)) => Ok($ret(l)),
                        _ => Err(()),
                    },
                    _ => Err(()),
                }
                .map_err(|_| "Expected type `List` to operate".to_string())
            }),
        );
    };
}

macro_rules! load_func_mut {
    ($funcs: ident, $name: ident, $ret: expr) => {
        $funcs.insert(
            stringify!($name).to_string(),
            Arc::new(|state, parameters, _| {
                assure_para_len!(parameters, 1);
                match parameters[0] {
                    DiatomValue::Ref(id) => match state.get_obj_mut(id) {
                        Some(DiatomObjectMut::List(l)) => Ok($ret(l, id)),
                        _ => Err(()),
                    },
                    _ => Err(()),
                }
                .map_err(|_| "Expected type `List` to operate".to_string())
            }),
        );
    };
}

pub fn list_extension<Buffer: IoWrite>() -> Extension<Buffer> {
    let mut funcs: AHashMap<String, Arc<ForeignFunction<Buffer>>> = AHashMap::default();
    load_func!(funcs, len, |l: DiatomList<Buffer>| DiatomValue::Int(
        l.len() as i64
    ));
    load_func!(funcs, is_empty, |l: DiatomList<Buffer>| DiatomValue::Bool(
        l.is_empty()
    ));
    load_func_mut!(funcs, reverse, |mut l: DiatomListMut<Buffer>, id| {
        l.reverse();
        DiatomValue::Ref(id)
    });
    load_func_mut!(funcs, clear, |mut l: DiatomListMut<Buffer>, id| {
        l.clear();
        DiatomValue::Ref(id)
    });

    funcs.insert(
        "append".to_string(),
        Arc::new(|state, parameters, _| {
            assure_para_len!(parameters, 2);
            match parameters[0] {
                DiatomValue::Ref(id) => match state.get_obj_mut(id) {
                    Some(DiatomObjectMut::List(mut l)) => {
                        l.push(parameters[1].clone());
                        Ok(DiatomValue::Ref(id))
                    }
                    _ => Err(()),
                },
                _ => Err(()),
            }
            .map_err(|_| "Expected type `List` to operate".to_string())
        }),
    );

    funcs.insert(
        "insert".to_string(),
        Arc::new(|state, parameters, _| {
            assure_para_len!(parameters, 3);
            match (&parameters[0], &parameters[1]) {
                (DiatomValue::Ref(id), DiatomValue::Int(idx)) => match state.get_obj_mut(*id) {
                    Some(DiatomObjectMut::List(mut l)) => {
                        if (*idx >= 0 && *idx as usize >= l.len())
                            || (*idx < 0 && idx.unsigned_abs() as usize > l.len())
                        {
                            return Err(format!(
                                "Index list$[{idx}] while having {} items",
                                l.len()
                            ));
                        }
                        let index = if *idx >= 0 {
                            *idx as usize
                        } else {
                            l.len() - (idx.unsigned_abs() as usize)
                        };
                        l.insert(index, parameters[2].clone());
                        Ok(DiatomValue::Ref(*id))
                    }
                    _ => Err(()),
                },
                _ => Err(()),
            }
            .map_err(|_| "Expected type `List` and `Int` to operate".to_string())
        }),
    );

    funcs.insert(
        "remove".to_string(),
        Arc::new(|state, parameters, _| {
            assure_para_len!(parameters, 2);
            match (&parameters[0], &parameters[1]) {
                (DiatomValue::Ref(id), DiatomValue::Int(idx)) => match state.get_obj_mut(*id) {
                    Some(DiatomObjectMut::List(mut l)) => {
                        if (*idx >= 0 && *idx as usize >= l.len())
                            || (*idx < 0 && idx.unsigned_abs() as usize > l.len())
                        {
                            return Err(format!(
                                "Index list$[{idx}] while having {} items",
                                l.len()
                            ));
                        }
                        let index = if *idx >= 0 {
                            *idx as usize
                        } else {
                            l.len() - (idx.unsigned_abs() as usize)
                        };
                        l.remove(index);
                        Ok(DiatomValue::Ref(*id))
                    }
                    _ => Err(()),
                },
                _ => Err(()),
            }
            .map_err(|_| "Expected type `List` and `Int` to operate".to_string())
        }),
    );

    Extension {
        name: "list".to_string(),
        kind: ExtensionKind::ForeignFunctions(funcs),
    }
}
