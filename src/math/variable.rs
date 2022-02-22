use std::collections::BTreeMap;

use smartstring::{LazyCompact, SmartString};

pub type OperableType = f32; // Only floats are available
pub type VariableName = SmartString<LazyCompact>;
pub type Scope = BTreeMap<VariableName, Variable>;

#[derive(Debug)]
pub struct Variable {
    pub name: VariableName,
    pub inner: OperableType,
}
