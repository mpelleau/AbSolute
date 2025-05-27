use std::cmp::Ordering;
use rug::Rational; // For conversions to/from rational
use crate::arith_ring::RingOps; // Added for RingOps trait
use std::fmt; // For Display, Debug traits (though i64 implements them)

// Type alias for integer numbers used in this module
pub type I = i64;

// --- Constants ---
pub const ZERO: I = 0;
pub const ONE: I = 1;

// --- Operators ---
pub fn add(a: I, b: I) -> I {
    a + b
}

pub fn mul(a: I, b: I) -> I {
    a * b
}

// OCaml: let div x y = if y <> 0 && x mod y = 0 then Some (x / y) else None
pub fn div_exact_op(a: I, b: I) -> Option<I> {
    if b != 0 && a % b == 0 {
        Some(a / b)
    } else {
        None
    }
}

pub fn neg_op(i: I) -> I {
    -i
}

// --- Conversions ---
// OCaml: let to_int x = Some x
pub fn to_i64_option(i: I) -> Option<i64> {
    Some(i) // Since I is i64, this is straightforward
}

// OCaml: let to_float = float_of_int
pub fn to_f64(i: I) -> f64 {
    i as f64
}

// OCaml: let to_rational = Mpqf.of_int
pub fn to_rational_op(i: I) -> Rational {
    Rational::from(i)
}

// OCaml: let floor = Fun.id
pub fn floor_op(i: I) -> I {
    i // Already an integer
}

// OCaml: let of_int = Fun.id
pub fn from_i64_id(i: i64) -> I {
    i // Since I is i64
}

// OCaml: let of_float = int_of_float
pub fn from_f64(f: f64) -> I {
    f as I // Standard float to int conversion (truncation)
}

// OCaml: let of_rational x = of_float (Mpqf.to_float x)
pub fn from_rational_op(r: &Rational) -> I {
    r.to_f64() as I // Converts rational to float, then float to int (truncation)
}

// --- Comparisons ---
pub fn equals(a: I, b: I) -> bool {
    a == b
}

pub fn compare_i(a: I, b: I) -> Ordering {
    a.cmp(&b)
}

// --- Printing ---
// OCaml: let print fmt x = Format.fprintf fmt "%i" x
// Rust: Simplified to return a String
pub fn pp_print_i(i: I) -> String {
    format!("{}", i)
}

// OCaml: let to_string = string_of_int
pub fn to_string_i(i: I) -> String {
    i.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rug::Rational;

    #[test]
    fn test_i_constants() {
        assert_eq!(ZERO, 0);
        assert_eq!(ONE, 1);
    }

    #[test]
    fn test_i_arith_ops() {
        assert_eq!(add(5, 3), 8);
        assert_eq!(mul(5, 3), 15);
        assert_eq!(neg_op(5), -5);
    }

    #[test]
    fn test_i_div_exact_op() {
        assert_eq!(div_exact_op(10, 2), Some(5));
        assert_eq!(div_exact_op(10, 3), None);
        assert_eq!(div_exact_op(10, 0), None);
        assert_eq!(div_exact_op(0, 5), Some(0));
    }

    #[test]
    fn test_i_conversions() {
        assert_eq!(to_i64_option(123), Some(123));
        assert_eq!(to_f64(123), 123.0);
        assert_eq!(to_rational_op(123), Rational::from(123));
        
        assert_eq!(floor_op(123), 123);
        assert_eq!(from_i64_id(123), 123);
        
        assert_eq!(from_f64(123.0), 123);
        assert_eq!(from_f64(123.7), 123); // Truncation
        assert_eq!(from_f64(-123.7), -123); // Truncation
        
        assert_eq!(from_rational_op(&Rational::from(123)), 123);
        assert_eq!(from_rational_op(&Rational::from((1234, 10))), 123); // 123.4 -> 123
    }

    #[test]
    fn test_i_comparisons() {
        assert!(equals(5, 5));
        assert!(!equals(5, 3));
        assert_eq!(compare_i(5, 3), Ordering::Greater);
        assert_eq!(compare_i(3, 5), Ordering::Less);
        assert_eq!(compare_i(5, 5), Ordering::Equal);
    }

    #[test]
    fn test_i_printing() {
        assert_eq!(pp_print_i(123), "123");
        assert_eq!(to_string_i(-456), "-456");
    }
}

// Implementation of RingOps for I (i64)
impl RingOps for I {
    fn zero() -> Self {
        ZERO // Uses the const ZERO defined in this module
    }

    fn one() -> Self {
        ONE // Uses the const ONE defined in this module
    }

    fn add(&self, other: &Self) -> Self {
        add(*self, *other) // Calls add(I, I) -> I from this module
    }

    fn mul(&self, other: &Self) -> Self {
        mul(*self, *other) // Calls mul(I, I) -> I from this module
    }

    fn div_exact(&self, other: &Self) -> Option<Self> {
        div_exact_op(*self, *other) // Calls div_exact_op(I, I) -> Option<I>
    }

    fn neg(&self) -> Self {
        neg_op(*self) // Calls neg_op(I) -> I
    }

    fn to_i64_exact(&self) -> Option<i64> {
        to_i64_option(*self) // Calls to_i64_option(I) -> Option<i64>
    }

    fn to_f64(&self) -> f64 {
        to_f64(*self) // Calls to_f64(I) -> f64
    }

    fn to_rational(&self) -> Rational {
        to_rational_op(*self) // Calls to_rational_op(I) -> Rational
    }

    fn floor_as_i64(&self) -> i64 {
        floor_op(*self) // Calls floor_op(I) -> I (which is i64)
    }

    fn from_i64(val: i64) -> Self {
        from_i64_id(val) // Calls from_i64_id(i64) -> I
    }

    fn from_f64(val: f64) -> Self {
        from_f64(val) // Calls from_f64(f64) -> I
    }

    fn from_rational(r: &Rational) -> Self {
        from_rational_op(r) // Calls from_rational_op(&Rational) -> I
    }

    fn equals(&self, other: &Self) -> bool {
        equals(*self, *other) // Calls equals(I, I) -> bool
    }

    fn compare_total(&self, other: &Self) -> Ordering {
        compare_i(*self, *other) // Calls compare_i(I, I) -> Ordering
    }
}
// Note: i64 already implements Display, Debug, Clone, PartialEq from std,
// so no explicit implementation is needed for those RingOps trait bounds.
