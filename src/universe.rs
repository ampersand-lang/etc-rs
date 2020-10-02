use std::cmp::{Ordering, PartialOrd};
use std::ops::Sub;

/// Represents the universe of a node.
#[derive(Debug, Clone)]
pub enum Universe {
    /// A terminal universe, i.e. one that doesn't produce other universes.
    Terminal(i32),
    /// A mapping from some number of arbitrary universes into another universe.
    Mapping(Vec<Universe>, Box<Universe>),
    /// A mapping from some number of arbitrary universes into another universe.
    MappingStar(i32, Vec<Universe>, Box<Universe>),
}

impl Universe {
    pub const ZERO: Self = Universe::Terminal(0);
    pub const MAX: Self = Universe::Terminal(i32::MAX);
    pub const MIN: Self = Universe::Terminal(i32::MIN);

    /// Returns the proper value of the universe.
    pub fn value(&self) -> i32 {
        match self {
            Universe::Terminal(u) => *u,
            Universe::Mapping(params, result) => params
                .iter()
                .map(Universe::value)
                .min()
                .unwrap_or(result.value()),
            Universe::MappingStar(value, _, _) => *value,
        }
    }

    /// Returns the proper value of the universe.
    pub fn value_unstar(&self) -> i32 {
        match self {
            Universe::Terminal(u) => *u,
            Universe::MappingStar(_, params, result) | Universe::Mapping(params, result) => params
                .iter()
                .map(Universe::value)
                .min()
                .unwrap_or(result.value()),
        }
    }

    /// Returns the smallest of two universes in the form of a Terminal
    pub fn min(&self, other: &Self) -> Self {
        if self.value() < other.value() {
            self.clone()
        } else {
            other.clone()
        }
    }

    /// Returns the biggest of two universes in the form of a Terminal
    pub fn max(&self, other: &Self) -> Self {
        if self.value() > other.value() {
            self.clone()
        } else {
            other.clone()
        }
    }
}

impl Sub<i32> for Universe {
    type Output = Universe;

    fn sub(self, other: i32) -> Self::Output {
        Universe::Terminal(self.value() - other)
    }
}

impl<'a> Sub<i32> for &'a Universe {
    type Output = Universe;

    fn sub(self, other: i32) -> Self::Output {
        Universe::Terminal(self.value() - other)
    }
}

impl PartialEq for Universe {
    fn eq(&self, other: &Self) -> bool {
        self.value() == other.value()
    }
}

impl PartialEq<i32> for Universe {
    fn eq(&self, other: &i32) -> bool {
        self.value() == *other
    }
}

impl PartialOrd<i32> for Universe {
    fn partial_cmp(&self, other: &i32) -> Option<Ordering> {
        self.value().partial_cmp(other)
    }
}
