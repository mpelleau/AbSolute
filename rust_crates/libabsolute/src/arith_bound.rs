use std::cmp::Ordering;
use std::fmt::{Display, Debug}; // For trait bounds and Display impl for BoundKind
use rug::Rational; // For Mpqf.t

// OCaml: type kind = FINITE | MINF | INF | INVALID
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BoundKind {
    Finite,    // Finite number
    NegativeInfinity, // -oo
    PositiveInfinity, // +oo
    Invalid,   // For NaN or other invalid numbers
}

impl Display for BoundKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BoundKind::Finite => write!(f, "FINITE"),
            BoundKind::NegativeInfinity => write!(f, "MINF"),
            BoundKind::PositiveInfinity => write!(f, "INF"),
            BoundKind::Invalid => write!(f, "INVALID"),
        }
    }
}

/// Signature for bounds: numeric values enriched with +oo and -oo.
/// Corresponds to OCaml's `Bound.T` module type.
pub trait BoundOps: Sized + Display + Debug + Clone + PartialEq {
    // --- Ordering ---
    fn compare(&self, other: &Self) -> Ordering;
    // fn equal(&self, other: &Self) -> bool; // Provided by PartialEq
    fn leq(&self, other: &Self) -> bool;
    fn geq(&self, other: &Self) -> bool;
    fn lt(&self, other: &Self) -> bool;
    fn gt(&self, other: &Self) -> bool;
    // fn neq(&self, other: &Self) -> bool; // Provided by PartialEq

    fn is_odd(&self) -> bool; // Note: Meaning of odd/even for non-finite or non-integer types?
    fn is_even(&self) -> bool; // OCaml sig has this. Impl will define semantics.

    fn min_val(&self, other: &Self) -> Self; // Renamed from min to avoid conflict
    fn max_val(&self, other: &Self) -> Self; // Renamed from max to avoid conflict
    
    fn sign(&self) -> i32; // OCaml returns int, typically -1, 0, 1

    // --- Construction (with directed rounding) ---
    fn from_i64_up(val: i64) -> Self;
    fn from_i64_down(val: i64) -> Self;

    // OCaml had of_z_up/down (GMP integers). We can use i64 or rug::Integer if needed later.
    // For now, stick to explicit types from signature.

    fn from_f64_up(val: f64) -> Self;
    fn from_f64_down(val: f64) -> Self;

    fn from_rational_up(r: &Rational) -> Self;
    fn from_rational_down(r: &Rational) -> Self;
    
    // of_string_up/down might be tricky due to parsing logic.
    // Let's assume they parse to an intermediate (like f64 or Rational) then convert with rounding.
    // Or, the implementation itself handles direct parsing with rounding.
    // For the trait, we just define the signature.
    fn from_string_up(s: &str) -> Result<Self, String>; // Return Result for parse errors
    fn from_string_down(s: &str) -> Result<Self, String>;


    // --- Printing ---
    // `to_string` is covered by `Display` trait.
    // `output`, `sprint`, `bprint` are OCaml specific output methods.
    // `pp_print` is also OCaml Formatter specific.
    // Implementing `Display` is the Rust idiomatic way.

    // --- Conversion (with directed rounding for floats) ---
    fn to_f64_up(&self) -> f64;
    fn to_f64_down(&self) -> f64;
    fn to_rational(&self) -> Rational; // Assumes this can represent Self, or is a best effort for finite.

    // --- Classification ---
    fn classify(&self) -> BoundKind;

    // --- Useful Constants ---
    // Made these methods as Self may not be Copy or easily const-constructible (e.g. if it contains Rational)
    fn zero() -> Self;
    fn one() -> Self;
    fn two() -> Self;
    fn minus_one() -> Self;
    fn positive_infinity() -> Self; // Renamed from inf
    fn negative_infinity() -> Self; // Renamed from minus_inf

    // --- Operators ---
    // Exact operators
    fn neg_op(&self) -> Self; // Renamed from neg
    fn abs_op(&self) -> Self; // Renamed from abs

    // Operators with rounding
    fn add_up(&self, other: &Self) -> Self;
    fn sub_up(&self, other: &Self) -> Self;
    fn mul_up(&self, other: &Self) -> Self;
    fn div_up(&self, other: &Self) -> Self;

    fn add_down(&self, other: &Self) -> Self;
    fn sub_down(&self, other: &Self) -> Self;
    fn mul_down(&self, other: &Self) -> Self;
    fn div_down(&self, other: &Self) -> Self;

    // OCaml: val bound_mul : (t -> t -> t) -> t -> t -> t
    // This seems to take a multiplication function (e.g. specific rounding) and applies it.
    // In Rust, this might be a higher-order method or handled by specific mul_up/mul_down.
    // For now, let's translate it directly. F must be a function pointer or closure.
    // fn call_bound_mul<F: Fn(&Self, &Self) -> Self>(&self, other: &Self, mul_fn: F) -> Self;
    // This is a bit unusual for a trait like this.
    // The OCaml version implies `bound_mul` is a static function in the module, not a method.
    // Let's omit `bound_mul` and `bound_div` from the trait for now, as their nature as
    // higher-order functions operating on `t` suggests they might be utility functions
    // built using the trait's methods, rather than core trait methods themselves.
    // Or they are passed the specific `mul_up` or `mul_down` of the type.

    fn sqrt_up(&self) -> Self;
    fn sqrt_down(&self) -> Self;

    fn pow_up(&self, exp: i32) -> Self; // OCaml `int` for exp
    fn pow_down(&self, exp: i32) -> Self;
    
    fn root_up(&self, n: i32) -> Self; // nth root
    fn root_down(&self, n: i32) -> Self;

    // Trigonometric and exponential functions with rounding
    fn cos_up(&self) -> Self; fn cos_down(&self) -> Self;
    fn sin_up(&self) -> Self; fn sin_down(&self) -> Self;
    fn tan_up(&self) -> Self; fn tan_down(&self) -> Self;
    fn acos_up(&self) -> Self; fn acos_down(&self) -> Self;
    fn asin_up(&self) -> Self; fn asin_down(&self) -> Self;
    fn atan_up(&self) -> Self; fn atan_down(&self) -> Self;
    fn exp_up(&self) -> Self; fn exp_down(&self) -> Self; // e^x
    fn ln_up(&self) -> Self; fn ln_down(&self) -> Self;   // natural log
    fn log_up(&self) -> Self; fn log_down(&self) -> Self; // Assuming log base 10 or configurable, OCaml often means natural log for `log` too.
                                                         // For now, let's assume it's natural log like `ln` if not specified.
                                                         // Or it could be log2. This needs clarification from usage.
                                                         // Let's assume natural log for `log_up/down` for now.

    // Integer rounding for the bound type itself
    fn floor_val(&self) -> Self; // Renamed from floor
    fn ceil_val(&self) -> Self;  // Renamed from ceil
}
