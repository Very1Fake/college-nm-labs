use std::collections::BTreeMap;

use smartstring::{LazyCompact, SmartString};

/// Operand Type
pub type VarName = SmartString<LazyCompact>; // TODO: Char only but with indices (e.g. x1/y2)

pub enum Scope {
    Empty,
    Single(Var),
    Multi(BTreeMap<VarName, Var>),
}

impl Scope {
    pub fn multi() -> Self {
        Self::Multi(BTreeMap::new())
    }

    #[inline]
    pub fn get(&self, name: &VarName) -> Option<&Var> {
        match self {
            Scope::Empty => None,
            Scope::Single(var) => {
                if name == &var.name {
                    Some(var)
                } else {
                    None
                }
            }
            Scope::Multi(inner) => inner.get(name),
        }
    }

    pub fn insert(&mut self, variable: Var) -> Option<Var> {
        match self {
            Scope::Empty => Some(variable),
            Scope::Single(var) => {
                *var = variable;
                None
            }
            Scope::Multi(inner) => inner.insert(variable.name.clone(), variable),
        }
    }
}

impl Default for Scope {
    #[inline]
    fn default() -> Self {
        Scope::Empty
    }
}

#[derive(Debug)]
pub struct Var {
    pub name: VarName,
    pub value: f64,
}

impl Var {
    pub fn new(name: impl Into<VarName>, value: f64) -> Self {
        Self {
            name: name.into(),
            value,
        }
    }
}
