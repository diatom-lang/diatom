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
            Reg::Int(i) => Ok(Reg::Int(i64::abs(i))),
            _ => Err("Expected type `Int`".to_string()),
        }
    }));

    meta.attributes
        .insert(key_pool.get_or_insert("abs"), Reg::Ref(abs));

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
