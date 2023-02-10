use ahash::AHashMap;

use crate::diagnostic::Loc;

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
    pub depth: usize,
}

#[derive(Clone)]
pub struct Loop {
    pub start_inst_offset: usize,
    pub start_insert_offset: usize,
    pub breaks: Vec<FutureJump>,
}

#[derive(Clone)]
pub struct RegisterTable {
    pub prev: Option<Box<RegisterTable>>,
    /// name, location, is_defined_in_current_repl
    pub variables: AHashMap<String, (usize, Option<Loc>, bool)>,
    free: Vec<usize>,
    pub assigned: usize,
    pub func_id: usize,
    constant_table: AHashMap<ConstantValue, usize>,
    /// Count how many load constant inserted
    pub insert_offset: usize,
    /// captured variables
    pub capture: Vec<Capture>,
    pub loops: Vec<Loop>,
}

impl RegisterTable {
    pub fn new(func_id: usize) -> Self {
        Self {
            prev: None,
            variables: AHashMap::default(),
            free: vec![],
            assigned: 0,
            func_id,
            constant_table: AHashMap::default(),
            insert_offset: 0,
            capture: vec![],
            loops: vec![],
        }
    }

    pub fn declare_variable(&mut self, name: impl AsRef<str>, loc: Option<Loc>) -> usize {
        let id = self.declare_intermediate();
        self.variables
            .insert(name.as_ref().to_string(), (id, loc, true));
        id
    }

    /// Declare a captured variable
    ///
    /// This function alloc a new register instead of recycle old one
    pub fn declare_captured_variable(&mut self, name: impl AsRef<str>, loc: Option<Loc>) -> usize {
        let id = self.assigned;
        self.assigned += 1;
        self.variables
            .insert(name.as_ref().to_string(), (id, loc, true));
        id
    }
    /// (reg_id, depth, loc)
    fn lookup_variable_(
        &self,
        name: &str,
        depth: usize,
    ) -> Option<(usize, usize, Option<Loc>, bool)> {
        let var = self.variables.get(name);
        match var {
            Some((id, loc, define_in_scope)) => Some((*id, depth, loc.clone(), *define_in_scope)),
            None => match &self.prev {
                Some(prev) => prev.lookup_variable_(name, depth + 1),
                None => None,
            },
        }
    }

    /// Return (reg_id, depth, loc, is_defined_in_current_repl)
    pub fn lookup_variable(
        &self,
        name: impl AsRef<str>,
    ) -> Option<(usize, usize, Option<Loc>, bool)> {
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

    /// Return ok is constant is in table, otherwise alloc a new register for this constant
    pub fn get_or_alloc_constant(&mut self, constant: ConstantValue) -> Result<usize, usize> {
        match self.constant_table.get(&constant) {
            Some(id) => Ok(*id),
            None => {
                // must alloc new register to prevent
                // already compiled Instruction
                // overwrite constant value
                let id = self.assigned;
                self.assigned += 1;
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

    /// Invalidate variable definition location
    ///
    /// Use this function when move to a new repl ast
    pub fn invalidate_define(&mut self) {
        self.variables
            .iter_mut()
            .for_each(|(_, value)| value.2 = false);
        if let Some(rt) = self.prev.as_mut() {
            rt.invalidate_define()
        }
    }
}