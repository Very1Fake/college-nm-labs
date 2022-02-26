use std::collections::BTreeMap;

use smartstring::{LazyCompact, SmartString};

/// Operand Type
pub type OpType = f64; // Only floats are available
pub type VarName = SmartString<LazyCompact>;
pub type Scope = BTreeMap<VarName, Var>;

#[derive(Debug)]
pub struct Var {
    pub name: VarName,
    pub inner: OpType,
}
