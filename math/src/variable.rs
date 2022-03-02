use std::collections::BTreeMap; // TODO: Try more performant maps

use smartstring::{LazyCompact, SmartString};

/// Operand Type
pub type OpType = f64; // Only floats are available
pub type VarName = SmartString<LazyCompact>; // TODO: Char only but with indices (e.g. x1/y2)

#[derive(Default)]
pub struct Scope {
    inner: BTreeMap<VarName, Var>,
}

impl Scope {
    #[inline]
    pub fn get(&self, name: &VarName) -> Option<&Var> {
        self.inner.get(name)
    }

    #[inline]
    pub fn insert(&mut self, variable: Var) -> Option<Var> {
        self.inner.insert(variable.name.clone(), variable)
    }
}

#[derive(Debug)]
pub struct Var {
    pub name: VarName,
    pub inner: OpType,
}
