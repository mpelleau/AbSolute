use std::fmt::Debug;
use crate::arith_bound::BoundOps;
use crate::arith_q::Q; // For Rational type used in signatures
use crate::lang::ast_constraint::Constraint; // For to_constraint
use crate::misc_consistency::Consistency;   // For filter results
use super::itv::Interval; // The Interval struct these traits will operate on

// Corresponds to OCaml's ITV_EVAL module type
pub trait IntervalEvalOps<B: BoundOps>: Sized + Display + Debug + Clone + PartialEq {
    // The interval type itself, often Self for implementors like Interval<B>
    // type Itv = Interval<B>; // This can't be here, Self will be Interval<B>

    // --- Constructors ---
    fn top_int() -> Self; // Represents [-inf, +inf] for integers (might map to B's inf)
    fn top_real() -> Self; // Represents [-inf, +inf] for reals

    fn of_ints(low: i64, high: i64) -> Self;
    fn of_rats(low: &Q, high: &Q) -> Self;
    fn of_floats(low: f64, high: f64) -> Self;

    // Singletons
    fn of_i64(val: i64) -> Self; // Renamed from of_int
    fn of_rat(r: &Q) -> Self;
    fn of_f64(val: f64) -> Self; // Renamed from of_float

    // --- Printing and Conversions ---
    fn to_float_range(&self) -> (f64, f64);
    fn to_rational_range(&self) -> (Q, Q);
    // `print` is covered by `Display` trait.
    fn to_constraint(&self, var_name: &str) -> Constraint;

    // --- Measure ---
    fn float_size(&self) -> f64; // Width of the interval as float
    fn score(&self) -> f64;      // Some heuristic score

    // --- Set-theoretic Operations ---
    fn join(&self, other: &Self) -> Self; // Union
    fn meet(&self, other: &Self) -> Self; // Intersection
    fn meet_opt(&self, other: &Self) -> Option<Self>; // Intersection, None if empty

    fn split(&self) -> Vec<Self>; // Split into a list of intervals (e.g., for non-contiguous from meet)
    
    // `prune : (t -> t -> t list) option` - This is complex.
    // It's an optional function that takes two intervals and returns a list of intervals.
    // This might be a strategy passed to some algorithms.
    // For a trait, it's hard to represent directly unless it's a method that takes such a function.
    // Skipping `prune` from the trait for now, can be a standalone utility or part of specific algos.

    // --- Predicates ---
    fn contains_f64(&self, val: f64) -> bool; // Renamed from contains_float
    fn is_positive(&self) -> bool; // True if low >= 0
    fn is_negative(&self) -> bool; // True if high <= 0
    fn is_singleton(&self) -> bool; // True if low == high

    // --- Interval Arithmetic (Forward Evaluation) ---
    fn neg_itv(&self) -> Self; // Renamed from neg
    fn abs_itv(&self) -> Self; // Renamed from abs
    fn add_itv(&self, other: &Self) -> Self; // Renamed from add
    fn sub_itv(&self, other: &Self) -> Self; // Renamed from sub
    fn mul_itv(&self, other: &Self) -> Self; // Renamed from mul
    fn div_itv(&self, other: &Self) -> Option<Self>; // Renamed from div, returns Option for division by zero interval

    fn min_itv(&self, other: &Self) -> Self; // Renamed from min
    fn max_itv(&self, other: &Self) -> Self; // Renamed from max

    fn pow_itv(&self, exp: &Self) -> Self; // Exponent is also an interval. OCaml note says "singleton positive integer".
                                        // This implies exp interval should be a singleton int.
    fn n_root_itv(&self, n: &Self) -> Option<Self>; // Nth root, n is an interval (likely singleton int)

    fn exp_itv(&self) -> Self; // e^self
    fn ln_itv(&self) -> Option<Self>;  // Natural log of self
    fn log_itv(&self) -> Option<Self>; // Log of self (assuming natural log)

    // `eval_fun : string -> t list -> t option`
    // This is for generic function evaluation. Might be hard to put in a trait directly
    // without dynamic dispatch or a large enum of known functions.
    // Skipping `eval_fun` from the trait for now. Can be a method on a concrete Interval type.

    fn spawn_f64(&self) -> f64; // Generate random float in interval. Renamed.
}


// Corresponds to OCaml's ITV module type (includes ITV_EVAL + filtering)
pub trait IntervalOps<B: BoundOps>: IntervalEvalOps<B> {
    // --- Filtering (Backward Evaluation / Constraint Propagation) ---
    // Results use Consistency type.
    // `filter_leq(a,b)` means `a <= b`. Refines `a` and `b`.
    fn filter_leq(&self, other: &Self) -> Consistency<(Self, Self)>;
    fn filter_lt(&self, other: &Self) -> Consistency<(Self, Self)>;
    fn filter_neq(&self, other: &Self) -> Consistency<(Self, Self)>;
    
    // `filter_eq(a,b)` means `a = b`. Result is refined intersection.
    fn filter_eq(&self, other: &Self) -> Consistency<Self>;

    // `filter_neg(arg, res)` means `res = -arg`. Refines `arg`.
    fn filter_neg_arg(&self, result_itv: &Self) -> Option<Self>; // Renamed from filter_neg
    
    // `filter_abs(arg, res)` means `res = abs(arg)`. Refines `arg`.
    fn filter_abs_arg(&self, result_itv: &Self) -> Option<Self>; // Renamed from filter_abs

    // `filter_add(arg1, arg2, res)` means `res = arg1 + arg2`. Refines `arg1`, `arg2`.
    fn filter_add_args(&self, other: &Self, result_itv: &Self) -> Option<(Self, Self)>; // Renamed from filter_add
    
    // Similar for sub, mul, div, pow
    fn filter_sub_args(&self, other: &Self, result_itv: &Self) -> Option<(Self, Self)>;
    fn filter_mul_args(&self, other: &Self, result_itv: &Self) -> Option<(Self, Self)>;
    fn filter_div_args(&self, other: &Self, result_itv: &Self) -> Option<(Self, Self)>;
    fn filter_pow_args(&self, exp_itv: &Self, result_itv: &Self) -> Option<(Self, Self)>; // Refines base and exp_itv

    // `filter_fun : string -> t list -> t -> t list option`
    // Similar to `eval_fun`, skipping from trait for now.
}
