//! Includes various utilities for the compiler.
use std::ops::{BitAnd, Not};

use num::Integer;

/// Treats integers as pointers.
pub trait IntPtr: Integer + BitAnd<Self, Output = Self> + Not<Output = Self> + Clone {
    /// Aligns a pointer (integer) downwards according to some align.
    ///
    /// The align is given in bytes.
    #[inline]
    fn align_down(self, align: Self) -> Self {
        self & !(align - Self::one())
    }

    /// Aligns a pointer (integer) upwards according to some align.
    ///
    /// The align is given in bytes.
    #[inline]
    fn align_up(self, align: Self) -> Self {
        // PERF: cloning here is inefficient for BigInts
        // NOTE: do we even care about BigInts?
        (self + align.clone() - Self::one()).align_down(align)
    }
}

impl<T: Integer + BitAnd<T, Output = T> + Not<Output = T> + Clone> IntPtr for T {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sanity() {
        assert_eq!(15.align_down(8), 8);
        assert_eq!(12.align_down(8), 8);
        assert_eq!(12.align_down(4), 12);
        assert_eq!(15.align_up(8), 16);
        assert_eq!(12.align_up(8), 16);
        assert_eq!(12.align_up(4), 12);
    }
}
