use std::{cell::RefCell, rc::Rc};

use super::{key_pool::KeyPool, pool::Pool, GcObject, Reg, Table};

use crate::{IoWrite, State};

fn new_f<F, Buffer: IoWrite>(f: F) -> GcObject<Buffer>
where
    F: Fn(&mut State<Buffer>, &[Reg], &mut Buffer) -> Result<Reg, String> + 'static,
{
    GcObject::NativeFunction(Rc::new(RefCell::new(f)))
}

pub fn init_int_meta<Buffer: IoWrite>(
    pool: &mut Pool<GcObject<Buffer>>,
    key_pool: &mut KeyPool,
) -> usize {
    let mut meta = Table {
        attributes: Default::default(),
        meta_table: None,
    };
    meta.attributes
        .insert(key_pool.get_or_insert("MIN"), Reg::Int(i64::MIN));
    meta.attributes
        .insert(key_pool.get_or_insert("MAX"), Reg::Int(i64::MAX));

    let abs = pool.alloc(new_f(|_, parameters: &[Reg], _| {
        if parameters.len() != 1 {
            return Err(format!(
                "Expected 1 parameter while {} is provided",
                parameters.len()
            ));
        }
        match parameters[0] {
            Reg::Int(i) => Ok(Reg::Int(i64::wrapping_abs(i))),
            _ => Err("Expected type `Int`".to_string()),
        }
    }));

    meta.attributes
        .insert(key_pool.get_or_insert("abs"), Reg::Ref(abs));

    let float = pool.alloc(new_f(|_, parameters: &[Reg], _| {
        if parameters.len() != 1 {
            return Err(format!(
                "Expected 1 parameter while {} is provided",
                parameters.len()
            ));
        }
        match parameters[0] {
            Reg::Int(i) => Ok(Reg::Float(i as f64)),
            _ => Err("Expected type `Int`".to_string()),
        }
    }));

    meta.attributes
        .insert(key_pool.get_or_insert("float"), Reg::Ref(float));

    pool.alloc(GcObject::Table(meta))
}

pub fn init_float_meta<Buffer: IoWrite>(
    pool: &mut Pool<GcObject<Buffer>>,
    key_pool: &mut KeyPool,
) -> usize {
    let mut meta = Table {
        attributes: Default::default(),
        meta_table: None,
    };
    meta.attributes
        .insert(key_pool.get_or_insert("MIN"), Reg::Float(f64::MIN));
    meta.attributes
        .insert(key_pool.get_or_insert("MAX"), Reg::Float(f64::MAX));
    meta.attributes
        .insert(key_pool.get_or_insert("INF"), Reg::Float(f64::INFINITY));
    meta.attributes.insert(
        key_pool.get_or_insert("NEG_INF"),
        Reg::Float(f64::NEG_INFINITY),
    );
    meta.attributes
        .insert(key_pool.get_or_insert("NAN"), Reg::Float(f64::NAN));

    let abs = pool.alloc(new_f(|_, parameters: &[Reg], _| {
        if parameters.len() != 1 {
            return Err(format!(
                "Expected 1 parameter while {} is provided",
                parameters.len()
            ));
        }
        match parameters[0] {
            Reg::Float(f) => Ok(Reg::Float(f64::abs(f))),
            _ => Err("Expected type `Float`".to_string()),
        }
    }));
    meta.attributes
        .insert(key_pool.get_or_insert("abs"), Reg::Ref(abs));

    let int = pool.alloc(new_f(|_, parameters: &[Reg], _| {
        if parameters.len() != 1 {
            return Err(format!(
                "Expected 1 parameter while {} is provided",
                parameters.len()
            ));
        }
        match parameters[0] {
            Reg::Float(f) => Ok(Reg::Int(f as i64)),
            _ => Err("Expected type `Float`".to_string()),
        }
    }));
    meta.attributes
        .insert(key_pool.get_or_insert("int"), Reg::Ref(int));

    let int = pool.alloc(new_f(|_, parameters: &[Reg], _| {
        if parameters.len() != 1 {
            return Err(format!(
                "Expected 1 parameter while {} is provided",
                parameters.len()
            ));
        }
        match parameters[0] {
            Reg::Float(f) => Ok(Reg::Float(f.round())),
            _ => Err("Expected type `Float`".to_string()),
        }
    }));
    meta.attributes
        .insert(key_pool.get_or_insert("round"), Reg::Ref(int));

    let floor = pool.alloc(new_f(|_, parameters: &[Reg], _| {
        if parameters.len() != 1 {
            return Err(format!(
                "Expected 1 parameter while {} is provided",
                parameters.len()
            ));
        }
        match parameters[0] {
            Reg::Float(f) => Ok(Reg::Float(f64::floor(f))),
            _ => Err("Expected type `Float`".to_string()),
        }
    }));
    meta.attributes
        .insert(key_pool.get_or_insert("floor"), Reg::Ref(floor));

    let ceil = pool.alloc(new_f(|_, parameters: &[Reg], _| {
        if parameters.len() != 1 {
            return Err(format!(
                "Expected 1 parameter while {} is provided",
                parameters.len()
            ));
        }
        match parameters[0] {
            Reg::Float(f) => Ok(Reg::Float(f64::ceil(f))),
            _ => Err("Expected type `Float`".to_string()),
        }
    }));
    meta.attributes
        .insert(key_pool.get_or_insert("ceil"), Reg::Ref(ceil));

    let is_nan = pool.alloc(new_f(|_, parameters: &[Reg], _| {
        if parameters.len() != 1 {
            return Err(format!(
                "Expected 1 parameter while {} is provided",
                parameters.len()
            ));
        }
        match parameters[0] {
            Reg::Float(f) => Ok(Reg::Bool(f64::is_nan(f))),
            _ => Err("Expected type `Float`".to_string()),
        }
    }));
    meta.attributes
        .insert(key_pool.get_or_insert("is_nan"), Reg::Ref(is_nan));

    let is_inf = pool.alloc(new_f(|_, parameters: &[Reg], _| {
        if parameters.len() != 1 {
            return Err(format!(
                "Expected 1 parameter while {} is provided",
                parameters.len()
            ));
        }
        match parameters[0] {
            Reg::Float(f) => Ok(Reg::Bool(f64::is_infinite(f))),
            _ => Err("Expected type `Float`".to_string()),
        }
    }));
    meta.attributes
        .insert(key_pool.get_or_insert("is_inf"), Reg::Ref(is_inf));

    pool.alloc(GcObject::Table(meta))
}

pub fn init_list_meta<Buffer: IoWrite>(
    pool: &mut Pool<GcObject<Buffer>>,
    key_pool: &mut KeyPool,
) -> usize {
    let mut meta = Table {
        attributes: Default::default(),
        meta_table: None,
    };
    let len = pool.alloc(new_f(|state, parameters: &[Reg], _| {
        if parameters.len() != 1 {
            return Err(format!(
                "Expected 1 parameter while {} is provided",
                parameters.len()
            ));
        }
        match parameters[0] {
            Reg::Ref(id) => match unsafe { state.gc.get_obj_unchecked_mut(id) } {
                GcObject::List(l) => Ok(Reg::Int(l.len() as i64)),
                _ => Err(()),
            },
            _ => Err(()),
        }
        .map_err(|_| "Expected type `List` to operate".to_string())
    }));

    meta.attributes
        .insert(key_pool.get_or_insert("len"), Reg::Ref(len));

    let clear = pool.alloc(new_f(|state, parameters: &[Reg], _| {
        if parameters.len() != 1 {
            return Err(format!(
                "Expected 1 parameter while {} is provided",
                parameters.len()
            ));
        }
        match parameters[0] {
            Reg::Ref(id) => match unsafe { state.gc.get_obj_unchecked_mut(id) } {
                GcObject::List(l) => {
                    l.clear();
                    Ok(Reg::Ref(id))
                }
                _ => Err(()),
            },
            _ => Err(()),
        }
        .map_err(|_| "Expected type `List` to operate".to_string())
    }));

    meta.attributes
        .insert(key_pool.get_or_insert("clear"), Reg::Ref(clear));

    let reverse = pool.alloc(new_f(|state, parameters: &[Reg], _| {
        if parameters.len() != 1 {
            return Err(format!(
                "Expected 1 parameter while {} is provided",
                parameters.len()
            ));
        }
        match parameters[0] {
            Reg::Ref(id) => match unsafe { state.gc.get_obj_unchecked_mut(id) } {
                GcObject::List(l) => {
                    l.reverse();
                    Ok(Reg::Ref(id))
                }
                _ => Err(()),
            },
            _ => Err(()),
        }
        .map_err(|_| "Expected type `List` to operate".to_string())
    }));

    meta.attributes
        .insert(key_pool.get_or_insert("reverse"), Reg::Ref(reverse));

    let append = pool.alloc(new_f(|state, parameters: &[Reg], _| {
        if parameters.len() != 2 {
            return Err(format!(
                "Expected 2 parameter while {} is provided",
                parameters.len()
            ));
        }
        match parameters[0] {
            Reg::Ref(id) => match unsafe { state.gc.get_obj_unchecked_mut(id) } {
                GcObject::List(l) => {
                    l.push(parameters[1].clone());
                    Ok(Reg::Ref(id))
                }
                _ => Err(()),
            },
            _ => Err(()),
        }
        .map_err(|_| "Expected type `List` to operate".to_string())
    }));

    meta.attributes
        .insert(key_pool.get_or_insert("append"), Reg::Ref(append));

    let insert = pool.alloc(new_f(|state, parameters: &[Reg], _| {
        if parameters.len() != 3 {
            return Err(format!(
                "Expected 3 parameter while {} is provided",
                parameters.len()
            ));
        }
        match (&parameters[0], &parameters[1]) {
            (Reg::Ref(id), Reg::Int(idx)) => match unsafe { state.gc.get_obj_unchecked_mut(*id) } {
                GcObject::List(l) => {
                    if (*idx >= 0 && *idx as usize >= l.len())
                        || (*idx < 0 && idx.unsigned_abs() as usize > l.len())
                    {
                        return Err(format!("Index list$[{idx}] while having {} items", l.len()));
                    }
                    let index = if *idx >= 0 {
                        *idx as usize
                    } else {
                        l.len() - (idx.unsigned_abs() as usize)
                    };
                    l.insert(index, parameters[2].clone());
                    Ok(Reg::Ref(*id))
                }
                _ => Err(()),
            },
            _ => Err(()),
        }
        .map_err(|_| "Expected type `List` and `Int` to operate".to_string())
    }));

    meta.attributes
        .insert(key_pool.get_or_insert("insert"), Reg::Ref(insert));

    let remove = pool.alloc(new_f(|state, parameters: &[Reg], _| {
        if parameters.len() != 2 {
            return Err(format!(
                "Expected 2 parameter while {} is provided",
                parameters.len()
            ));
        }
        match (&parameters[0], &parameters[1]) {
            (Reg::Ref(id), Reg::Int(idx)) => match unsafe { state.gc.get_obj_unchecked_mut(*id) } {
                GcObject::List(l) => {
                    if (*idx >= 0 && *idx as usize >= l.len())
                        || (*idx < 0 && idx.unsigned_abs() as usize > l.len())
                    {
                        return Err(format!("Index list$[{idx}] while having {} items", l.len()));
                    }
                    let index = if *idx >= 0 {
                        *idx as usize
                    } else {
                        l.len() - (idx.unsigned_abs() as usize)
                    };
                    l.remove(index);
                    Ok(Reg::Ref(*id))
                }
                _ => Err(()),
            },
            _ => Err(()),
        }
        .map_err(|_| "Expected type `List` and `Int` to operate".to_string())
    }));

    meta.attributes
        .insert(key_pool.get_or_insert("remove"), Reg::Ref(remove));

    pool.alloc(GcObject::Table(meta))
}
