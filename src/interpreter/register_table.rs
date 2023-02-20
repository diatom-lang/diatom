use ahash::AHashMap;

use crate::file_manager::Loc;

use super::FutureJump;

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum ConstantValue {
    Unit,
    Bool(bool),
    Int(i64),
    // Float must be transmuted in order to compare
    Float(u64),
    Str(String),
}

#[derive(Clone)]
pub struct Capture {
    pub rd: usize,
    pub rs: usize,
}

#[derive(Clone)]
pub struct Loop {
    pub start_inst_offset: usize,
    pub breaks: Vec<FutureJump>,
}

#[derive(Clone)]
pub struct RegisterTable {
    pub prev: Option<Box<RegisterTable>>,
    /// name, location
    pub variables: AHashMap<String, (usize, Option<Loc>)>,
    free: Vec<usize>,
    pub assigned: usize,
    pub func_id: usize,
    constant_table: AHashMap<ConstantValue, usize>,
    /// captured variables
    pub capture: Vec<Capture>,
    pub loops: Vec<Loop>,
    pub symbols: usize,
}

impl RegisterTable {
    pub fn new(func_id: usize) -> Self {
        Self {
            prev: None,
            variables: AHashMap::default(),
            free: vec![],
            assigned: 1,
            func_id,
            constant_table: AHashMap::from([(ConstantValue::Unit, 0)]),
            capture: vec![],
            loops: vec![],
            symbols: 0,
        }
    }

    /// Generate an unique symbol
    pub fn gen_sym(&mut self) -> String {
        let sym = format!("#G{:0>3}", self.symbols);
        self.symbols += 1;
        sym
    }

    /// prepare n free register for function call
    pub fn prepare_for_call(&mut self, para_size: usize) -> usize {
        let start = self.assigned;
        self.assigned += para_size;
        start
    }

    pub fn declare_variable(&mut self, name: impl AsRef<str>, loc: Option<Loc>) -> usize {
        let id = self.declare_intermediate();
        self.variables.insert(name.as_ref().to_string(), (id, loc));
        id
    }

    /// Declare a captured variable
    ///
    /// This function alloc a new register instead of recycle old one
    pub fn declare_captured_variable(&mut self, name: impl AsRef<str>, loc: Option<Loc>) -> usize {
        let id = self.assigned;
        self.assigned += 1;
        self.variables.insert(name.as_ref().to_string(), (id, loc));
        id
    }
    /// (reg_id, depth, loc)
    fn lookup_variable_(&self, name: &str, depth: usize) -> Option<(usize, usize, Option<Loc>)> {
        let var = self.variables.get(name);
        match var {
            Some((id, loc)) => Some((*id, depth, loc.clone())),
            None => match &self.prev {
                Some(prev) => prev.lookup_variable_(name, depth + 1),
                None => None,
            },
        }
    }

    /// Return (reg_id, depth, loc)
    pub fn lookup_variable(&self, name: impl AsRef<str>) -> Option<(usize, usize, Option<Loc>)> {
        self.lookup_variable_(name.as_ref(), 0)
    }

    pub fn declare_intermediate(&mut self) -> usize {
        self.free.pop().unwrap_or_else(|| {
            self.assigned += 1;
            self.assigned - 1
        })
    }

    pub fn free_intermediate(&mut self, id: usize) {
        if self.assigned >= id {
            self.free.push(id)
        }
    }

    pub fn get_or_alloc_constant(&mut self, constant: ConstantValue) -> Result<usize, usize> {
        match self.constant_table.get(&constant) {
            Some(id) => Ok(*id),
            None => {
                let id = self.declare_intermediate();
                self.constant_table.insert(constant, id);
                Err(id)
            }
        }
    }

    pub fn enter_function(&mut self, func_id: usize) {
        let old = std::mem::replace(self, RegisterTable::new(func_id));
        self.prev = Some(Box::new(old));
    }

    pub fn leave_function(&mut self) -> Vec<Capture> {
        let prev = *std::mem::take(&mut self.prev).unwrap();
        let pop = std::mem::replace(self, prev);
        pop.capture
    }
}
