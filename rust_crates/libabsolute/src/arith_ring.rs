use std::cmp::Ordering;
use std::fmt::{Display, Debug}; // Added Debug for convenience
use rug::Rational;

/// Trait representing ring-like operations, mirroring OCaml's Ring.T module type.
pub trait RingOps: Sized + Display + Debug + Clone + PartialEq {
    // Associated constants for zero and one
    // These might be better as methods `zero() -> Self` if Self is not Copy or easily const-constructible.
    // For types like i64, f64, const is fine. For rug::Rational, methods might be cleaner.
    // Let's try with methods for more generality, matching OCaml's `val zero : t`.
    fn zero() -> Self;
    fn one() -> Self;

    // Arithmetic operations
    // Taking arguments by reference to be general for non-Copy types like Rational.
    fn add(&self, other: &Self) -> Self;
    fn mul(&self, other: &Self) -> Self;
    
    /// Performs division if it's exact, otherwise returns None.
    fn div_exact(&self, other: &Self) -> Option<Self>;
    
    fn neg(&self) -> Self;

    // Conversions
    /// Converts to i64 if the value is an exact integer, otherwise None.
    fn to_i64_exact(&self) -> Option<i64>;
    fn to_f64(&self) -> f64;
    fn to_rational(&self) -> Rational; // Mpqf.t in OCaml
    
    /// Returns the floor of the value as an i64.
    fn floor_as_i64(&self) -> i64;

    // Constructors from other types
    fn from_i64(val: i64) -> Self;
    fn from_f64(val: f64) -> Self; // Consider if this should return Option<Self> if conversion can fail
    fn from_rational(r: &Rational) -> Self;

    // Comparisons
    // `equals` is often covered by `PartialEq` trait. This ensures it's part of the RingOps contract.
    fn equals(&self, other: &Self) -> bool; 
    
    /// Provides a total ordering.
    fn compare_total(&self, other: &Self) -> Ordering;

    // Printing:
    // The `std::fmt::Display` trait should be implemented by types satisfying `RingOps`.
    // The OCaml `to_string : t -> string` is then available via `val.to_string()`.
    // The OCaml `print : Format.formatter -> t -> unit` is specific to OCaml's Format module.
    // Rust uses `println!("{}", val)` or related macros with the `Display` trait.
}

// Example of how a type might implement this (not part of this file, just for illustration):
/*
impl RingOps for i64 {
    fn zero() -> Self { 0 }
    fn one() -> Self { 1 }
    fn add(&self, other: &Self) -> Self { *self + *other }
    // ... other implementations ...
}
*/
