use ahash::{AHashMap, AHashSet};

use crate::vm::VmError;

type TypeIdType = u32;

const MAX_CHECK_RECURSION_DEPTH: u16 = 256;

/// id(index) for a PolyType's variable
type VarId = TypeIdType;
/// id for instance
type InstanceId = TypeIdType;
/// id for type class
type ClassId = TypeIdType;
/// id for data type
type DataId = TypeIdType;
/// id for function
type FuncId = TypeIdType;

#[derive(PartialEq, Eq, Clone)]
pub enum Type {
    Any,
    Mono(DataId),
    Poly(DataId, Vec<Type>),
    PolyFree(VarId),
}

#[derive(PartialEq, Eq, Clone)]
pub enum ParticularType {
    Any,
    Mono(DataId),
    Poly(DataId, Vec<ParticularType>),
}
struct FuncType {
    pub type_paras: Vec<String>,
    pub paras: Vec<Type>,
    pub constraints: Vec<(ClassId, Vec<Type>)>,
}

struct DataType {
    pub name: String,
    pub methods: AHashMap<String, FuncType>,
    pub classes: AHashSet<ClassId>,
}

/// An instance
///
/// Such as
/// ```haskell
/// instance Eq a => Eq (Maybe a) where
///     (==) x y = ...
/// ```
struct Instance {
    /// [ a ]
    pub para_type: Vec<String>,
    /// [ (Maybe a) ]
    pub instance_type: Vec<Type>,
    /// { (==): xxx }
    pub methods: AHashMap<String, FuncId>,
    /// [ (Eq, [a]) ]
    pub constraints: Vec<(ClassId, Vec<Type>)>,
}

/// type class
struct TypeClass {
    pub name: String,
    pub instances: Vec<Instance>,
    pub type_parameters: Vec<String>,
    /// Method names and types
    pub methods: AHashMap<String, Vec<Type>>,
    pub constraints: Vec<(ClassId, Vec<Type>)>,
}

/// Type table
#[derive(Default)]
pub struct TypeTable {
    data_types: Vec<DataType>,
    classes: Vec<TypeClass>,
    funcs: Vec<Instance>,
}

impl TypeTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_data_type(mut self, data_type: DataType) -> Self {
        self.data_types.push(data_type);
        self
    }

    pub fn with_class(mut self, class: TypeClass) -> Self {
        self.classes.push(class);
        self
    }

    fn show_data_type(&self, id: DataId) -> String {
        self.data_types[id as usize].name.clone()
    }

    pub fn show_particular_type(&self, t: &ParticularType) -> String {
        match t {
            ParticularType::Any => "*".to_string(),
            ParticularType::Mono(id) => self.show_data_type(*id),
            ParticularType::Poly(id, vec) => format!(
                "( {} {} )",
                self.show_data_type(*id),
                vec.iter()
                    .map(|pt| self.show_particular_type(pt))
                    .reduce(|a, b| a + " " + &b)
                    .unwrap_or_else(|| " ".to_string())
            ),
        }
    }

    pub fn show_type(&self, t: &Type) -> String {
        match t {
            Type::Any => "*".to_string(),
            Type::Mono(id) => self.show_data_type(*id),
            Type::Poly(id, vec) => {
                format!(
                    "( {} {} )",
                    self.show_data_type(*id),
                    vec.iter()
                        .map(|t| self.show_type(t))
                        .reduce(|a, b| a + " " + &b)
                        .unwrap_or_else(|| " ".to_string())
                )
            }
            Type::PolyFree(i) => i.to_string(),
        }
    }

    /// Apply type parameters to a type
    fn specialize(&self, general: &Type, parameters: &[ParticularType]) -> ParticularType {
        match general {
            Type::Mono(did) => ParticularType::Mono(*did),
            Type::Poly(did, tvec) => ParticularType::Poly(
                *did,
                tvec.iter()
                    .map(|t| self.specialize(t, parameters))
                    .collect(),
            ),
            Type::PolyFree(vid) => parameters[*vid as usize].clone(),
            Type::Any => ParticularType::Any,
        }
    }

    /// Lookup a member function
    ///
    /// The first parameters is treat as the object.
    /// For example `1.add(2)` will be `lookup_member_function([Int, Int], "add")`
    pub fn lookup_member_function(
        &self,
        parameters: &[ParticularType],
        name: &str,
    ) -> Result<FuncId, VmError> {
        let data_id = match parameters[0] {
            ParticularType::Mono(id) => id,
            ParticularType::Poly(id, _) => id,
            ParticularType::Any => return Err(VmError::AnyTypeAttribute),
        };

        // search for Self instance
        let datatype = &self.data_types[data_id as usize];
        if let Some(f) = datatype.methods.get(name) {
            let type_parameters =
                self.infer_parameters(f.type_paras.len(), &f.paras, parameters)?;
            let type_parameters = if let Some(t) = type_parameters {
                t
            } else {
                return Err(VmError::ParameterWrongType);
            };
            for constraint in f.constraints.iter() {
                match self.check_constraint(&type_parameters, constraint, 0) {
                    Ok(true) => (),
                    Ok(false) => return Err(VmError::InvalidConstraint),
                    Err(err) => return Err(err),
                }
            }
        }

        // search for instances
        let methods = datatype
            .classes
            .iter()
            .filter_map(|class_id| {
                let class = &self.classes[*class_id as usize];
                class
                    .methods
                    .get(name)
                    .map(|method| (class_id, class, method))
            })
            .collect::<Vec<_>>();

        match methods.len() {
            0 => Err(VmError::NoSuchMethod),
            1 => {
                let (class_id, class, method) = *methods.first().unwrap();
                let type_parameters =
                    self.infer_parameters(class.type_parameters.len(), method, parameters)?;
                let type_parameters = if let Some(tp) = type_parameters {
                    tp
                } else {
                    return Err(VmError::CanNotDeduce);
                };
                for constraint in &class.constraints {
                    if !self.check_constraint(&type_parameters, constraint, 0)? {
                        return Err(VmError::InvalidConstraint);
                    }
                }
                let instances = self.lookup_instances(&type_parameters, *class_id, 0)?;
                match instances.len() {
                    0 => Err(VmError::NotAnInstance),
                    1 => Ok(*instances[0].methods.get(name).unwrap()),
                    _ => Err(VmError::OverlapInstance),
                }
            }
            _ => Err(VmError::AmbiguousMethod),
        }
    }

    /// Reslove a type's parameters to a particular type according to a give particular type
    fn _infer(
        result: &mut [Option<ParticularType>],
        expect: &Type,
        actual: &ParticularType,
    ) -> Result<bool, VmError> {
        match (expect, actual) {
            (Type::Mono(t1), ParticularType::Mono(t2)) => Ok(t1 == t2),
            (Type::Mono(_), ParticularType::Any) => Ok(true),
            (Type::Any, _) => Ok(true),
            (Type::PolyFree(tp), pt) => match &result[*tp as usize] {
                None | Some(ParticularType::Any) => {
                    result[*tp as usize] = Some(pt.clone());
                    Ok(true)
                }
                Some(pt2) => Ok(pt == pt2),
            },
            (Type::Poly(t1, v1), ParticularType::Poly(t2, v2)) => {
                if t1 == t2 && v1.len() == v2.len() {
                    for (t, pt) in v1.iter().zip(v2.iter()) {
                        if !Self::_infer(result, t, pt)? {
                            return Ok(false);
                        }
                    }
                    Ok(true)
                } else {
                    Ok(false)
                }
            }
            (Type::Poly(_, v), ParticularType::Any) => {
                for expect in v {
                    if !Self::_infer(result, expect, &ParticularType::Any)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            (Type::Mono(_), ParticularType::Poly(_, _))
            | (Type::Poly(_, _), ParticularType::Mono(_)) => Ok(false),
        }
    }

    /// Try to infer type parameters from given particular types
    ///
    /// Return None if has conflict or can not infer all
    /// Return VmError if error happens
    fn infer_parameters(
        &self,
        len: usize,
        general: &[Type],
        actual: &[ParticularType],
    ) -> Result<Option<Vec<ParticularType>>, VmError> {
        if general.len() != actual.len() {
            return Err(VmError::ParameterWrongLength);
        }
        let mut result: Vec<Option<ParticularType>> = vec![None; len];
        for (expect, actual) in general.iter().zip(actual.iter()) {
            if !Self::_infer(&mut result, expect, actual)? {
                return Ok(None);
            }
        }
        let mut inferred = vec![];
        for para in result {
            if let Some(t) = para {
                inferred.push(t);
            } else {
                return Ok(None);
            }
        }
        Ok(Some(inferred))
    }

    /// Lookup instances of a particular type
    ///
    /// For example, search for `Eq Float` would be `lookup_instances([Float], Eq, ...)`
    fn lookup_instances(
        &self,
        instance_type: &[ParticularType],
        class: ClassId,
        recursion_depth: u16,
    ) -> Result<Vec<&Instance>, VmError> {
        let instances = &self.classes[class as usize].instances;
        let mut instance_possible = vec![];
        'outer: for instance in instances {
            // try to match parameters
            let type_parameters = self.infer_parameters(
                instance.para_type.len(),
                &instance.instance_type,
                instance_type,
            )?;
            let type_parameters = if let Some(p) = type_parameters {
                p
            } else {
                continue;
            };
            for constraint in &instance.constraints {
                if !self.check_constraint(&type_parameters, constraint, recursion_depth)? {
                    continue 'outer;
                }
            }
            instance_possible.push(instance);
        }
        Ok(instance_possible)
    }

    /// Check if constraint is satisfied give what parameters are applied
    ///
    /// Return Ok(bool) if constraint can be verified else Err(())
    fn check_constraint(
        &self,
        type_parameters: &[ParticularType],
        constraint: &(ClassId, Vec<Type>),
        recursion_depth: u16,
    ) -> Result<bool, VmError> {
        if recursion_depth >= MAX_CHECK_RECURSION_DEPTH {
            return Err(VmError::ReduceOverflow);
        }
        let instance_type = constraint
            .1
            .iter()
            .map(|t| self.specialize(t, type_parameters))
            .collect::<Vec<_>>();
        let instances = self.lookup_instances(&instance_type, constraint.0, recursion_depth + 1)?;
        Ok(!instances.is_empty())
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    macro_rules! as_type {
        (Int) => {
            Type::Mono(0)
        };
        (Float) => {
            Type::Mono(1)
        };
        (Maybe) => {
            Type::Mono(2)
        };
    }

    macro_rules! as_p_type {
        (Int) => {
            ParticularType::Mono(0)
        };
        (Float) => {
            ParticularType::Mono(1)
        };
        (Maybe) => {
            ParticularType::Mono(2)
        };
    }

    macro_rules! as_class {
        (Add) => {
            0
        };
        (Eq) => {
            1
        };
        (Ord) => {
            2
        };
    }

    macro_rules! as_func {
        (add_int_int) => {
            0
        };
        (add_int_float) => {
            1
        };
        (eq_int_int) => {
            2
        };
        (lt_a_a) => {
            3
        };
        (eq_maybe_maybe) => {
            4
        };
    }

    #[test]
    fn test() {
        let type_table = TypeTable::new()
            .with_data_type(DataType {
                name: "Int".to_string(),
                methods: Default::default(),
                classes: AHashSet::from([as_class!(Add), as_class!(Ord), as_class!(Eq)]),
            })
            .with_data_type(DataType {
                name: "Float".to_string(),
                methods: Default::default(),
                classes: AHashSet::from([as_class!(Add)]),
            })
            .with_data_type(DataType {
                name: "Maybe".to_string(),
                methods: Default::default(),
                classes: AHashSet::from([as_class!(Eq)]),
            })
            .with_class(TypeClass {
                name: "Add".to_string(),
                instances: vec![
                    Instance {
                        para_type: vec![],
                        instance_type: vec![as_type!(Int), as_type!(Int), as_type!(Int)],
                        methods: AHashMap::from([("add".to_string(), as_func!(add_int_int))]),
                        constraints: vec![],
                    },
                    Instance {
                        para_type: vec![],
                        instance_type: vec![as_type!(Int), as_type!(Float), as_type!(Float)],
                        methods: AHashMap::from([("add".to_string(), as_func!(add_int_float))]),
                        constraints: vec![],
                    },
                ],
                type_parameters: vec!["a".to_string(), "b".to_string(), "c".to_string()],
                methods: AHashMap::from([(
                    "add".to_string(),
                    vec![Type::PolyFree(0), Type::PolyFree(1), Type::PolyFree(2)],
                )]),
                constraints: vec![],
            })
            .with_class(TypeClass {
                name: "Eq".to_string(),
                instances: vec![
                    Instance {
                        para_type: vec![],
                        instance_type: vec![as_type!(Int)],
                        methods: AHashMap::from([("eq".to_string(), as_func!(eq_int_int))]),
                        constraints: vec![],
                    },
                    Instance {
                        para_type: vec!["a".to_string()],
                        instance_type: vec![Type::Poly(2, vec![Type::PolyFree(0)])],
                        methods: AHashMap::from([("eq".to_string(), as_func!(eq_maybe_maybe))]),
                        constraints: vec![(as_class!(Eq), vec![Type::PolyFree(0)])],
                    },
                ],
                type_parameters: vec!["a".to_string()],
                methods: AHashMap::from([(
                    "eq".to_string(),
                    vec![Type::PolyFree(0), Type::PolyFree(0)],
                )]),
                constraints: vec![],
            })
            .with_class(TypeClass {
                name: "Ord".to_string(),
                instances: vec![
                    // instance Ord a => Ord a
                    Instance {
                        para_type: vec!["a".to_string()],
                        instance_type: vec![Type::PolyFree(0)],
                        methods: AHashMap::from([("lt".to_string(), as_func!(lt_a_a))]),
                        constraints: vec![(as_class!(Ord), vec![Type::PolyFree(0)])],
                    },
                ],
                type_parameters: vec!["a".to_string()],
                methods: AHashMap::from([(
                    "lt".to_string(),
                    vec![Type::PolyFree(0), Type::PolyFree(0)],
                )]),
                constraints: vec![],
            });

        // (+) Int Int
        let func =
            type_table.lookup_member_function(&[as_p_type!(Int), as_p_type!(Int), as_p_type!(Int)], "add");
        assert_eq!(func, Ok(as_func!(add_int_int)));

        // (<) Int Int
        //
        // instance Ord a => Ord a
        let func = type_table.lookup_member_function(&[as_p_type!(Int), as_p_type!(Int)], "lt");
        assert_eq!(func, Err(VmError::ReduceOverflow));

        // (<) Int Int Int
        let func =
            type_table.lookup_member_function(&[as_p_type!(Int), as_p_type!(Int), as_p_type!(Int)], "lt");
        assert_eq!(func, Err(VmError::ParameterWrongLength));

        // (==) Int Int
        let func = type_table.lookup_member_function(&[as_p_type!(Int), as_p_type!(Int)], "eq");
        assert_eq!(func, Ok(as_func!(eq_int_int)));

        // (+) Int Float
        let func = type_table.lookup_member_function(
            &[as_p_type!(Int), as_p_type!(Float), as_p_type!(Float)],
            "add",
        );
        assert_eq!(func, Ok(as_func!(add_int_float)));

        // (+) Float Float -- not implemented
        let func = type_table.lookup_member_function(
            &[as_p_type!(Float), as_p_type!(Float), as_p_type!(Float)],
            "add",
        );
        assert_eq!(func, Err(VmError::NotAnInstance));

        // (==) (Maybe Int) (Maybe Int)
        //
        // instance Eq a => Eq (Maybe a)
        let func = type_table.lookup_member_function(
            &[
                ParticularType::Poly(2, vec![as_p_type!(Int)]),
                ParticularType::Poly(2, vec![as_p_type!(Int)]),
            ],
            "eq",
        );
        assert_eq!(func, Ok(as_func!(eq_maybe_maybe)));

        // (==) Float -- not implemented
        let func = type_table.lookup_member_function(&[as_p_type!(Float), as_p_type!(Float)], "eq");
        assert_eq!(func, Err(VmError::NoSuchMethod));

        // (==) (Maybe Float) (Maybe Float)
        let func = type_table.lookup_member_function(
            &[
                ParticularType::Poly(2, vec![as_p_type!(Float)]),
                ParticularType::Poly(2, vec![as_p_type!(Float)]),
            ],
            "eq",
        );
        assert_eq!(func, Err(VmError::NotAnInstance));
    }
}
