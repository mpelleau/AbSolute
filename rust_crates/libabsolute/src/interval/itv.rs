use std::fmt::{self, Display, Debug};
use crate::arith_bound::BoundOps;

// Basic Interval struct. Traits will be implemented for this.
// low <= high should always hold, except for an empty/bottom interval.
#[derive(Clone, Debug, PartialEq, Eq)] // Eq if B: Eq
pub struct Interval<B: BoundOps> {
    pub low: B,
    pub high: B,
}

impl<B: BoundOps> Interval<B> {
    /// Creates a new interval. Ensures low <= high.
    /// If low > high, it might represent an empty interval, or should be an error.
    /// For now, allow creation; validity checks can be part of operations or a specific constructor.
    pub fn new(low: B, high: B) -> Self {
        // Basic check: if low > high after construction, it could indicate an empty set.
        // Some interval libraries use a special "bottom" interval or ensure low <= high.
        // For now, direct construction. Canonicalization (e.g. swapping if low > high,
        // or specific "bottom" representation) can be added.
        // Let's enforce low <= high for valid, non-empty intervals.
        if low.gt(&high) {
            // Return a canonical "empty" interval or panic.
            // OCaml often raises Exit for bottom.
            // For now, let's create an "invalid" interval that ops should handle.
            // Or, better, provide specific constructors for empty/bottom.
            // This simple `new` assumes low <= high is maintained by caller or handled by specific ops.
            // For a truly canonical form, if low > high, it should represent bottom.
            // Let's assume for now new creates what's given, and ops handle validity.
        }
        Interval { low, high }
    }

    /// Represents an empty interval. This might be one way to handle bottom.
    /// For example, low = +Inf, high = -Inf.
    pub fn bottom() -> Self {
        Interval {
            low: B::positive_infinity(),
            high: B::negative_infinity(),
        }
    }

    pub fn is_bottom(&self) -> bool {
        // An interval is bottom if low > high
        self.low.gt(&self.high)
    }
}


impl<B: BoundOps> Display for Interval<B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_bottom() {
            write!(f, "[empty]")
        } else {
            write!(f, "[{}; {}]", self.low, self.high)
        }
    }
}

use crate::interval::traits::IntervalEvalOps; // Import the trait
use crate::arith_q::Q; // For Rational type
use crate::lang::ast_constraint::{Constraint, Comparison, CmpOp}; // For to_constraint
use crate::lang::ast_expr::Expr; // For to_constraint
// Note: misc_consistency::Consistency will be needed for IntervalOps, not yet for these methods.


impl<B: BoundOps> IntervalEvalOps<B> for Interval<B> {
    // --- Constructors ---
    fn top_int() -> Self { // Represents [-inf, +inf] for integers
        // For integer intervals, this would be B::neg_inf, B::pos_inf using an IntBound.
        // Since B is generic, this is equivalent to top_real for now.
        Interval::new(B::negative_infinity(), B::positive_infinity())
    }
    fn top_real() -> Self {
        Interval::new(B::negative_infinity(), B::positive_infinity())
    }

    fn of_ints(low: i64, high: i64) -> Self {
        let l = B::from_i64_down(low); // Use _down for low bound for safety, though i64->B is exact
        let h = B::from_i64_up(high);   // Use _up for high bound
        if l.gt(&h) { return Self::bottom(); } // Ensure low <= high
        Interval::new(l, h)
    }
    fn of_rats(low: &Q, high: &Q) -> Self {
        let l = B::from_rational_down(low);
        let h = B::from_rational_up(high);
        if l.gt(&h) { return Self::bottom(); }
        Interval::new(l, h)
    }
    fn of_floats(low: f64, high: f64) -> Self {
        let l = B::from_f64_down(low);
        let h = B::from_f64_up(high);
        if l.gt(&h) { return Self::bottom(); }
        Interval::new(l, h)
    }

    // Singletons
    fn of_i64(val: i64) -> Self {
        Interval::new(B::from_i64_down(val), B::from_i64_up(val))
    }
    fn of_rat(r: &Q) -> Self {
        Interval::new(B::from_rational_down(r), B::from_rational_up(r))
    }
    fn of_f64(val: f64) -> Self {
        Interval::new(B::from_f64_down(val), B::from_f64_up(val))
    }

    // --- Printing and Conversions (Implement some, others later) ---
    fn to_float_range(&self) -> (f64, f64) {
        if self.is_bottom() {
            // Represent bottom as (NaN, NaN) or (+Inf, -Inf) for floats
            return (f64::NAN, f64::NAN); // Or (f64::INFINITY, f64::NEG_INFINITY)
        }
        (self.low.to_f64_down(), self.high.to_f64_up())
    }

    fn to_rational_range(&self) -> (Q, Q) {
        if self.is_bottom() {
            // How to represent bottom with two Rationals?
            // Panic, or return specific values like (1/0, -1/0) if Rational had Inf.
            // For now, panic, as Rational from arith_q cannot be Inf.
            panic!("to_rational_range called on a bottom interval");
        }
        // This assumes B::to_rational can panic if B is Inf.
        (self.low.to_rational(), self.high.to_rational())
    }

    fn to_constraint(&self, var_name: &str) -> Constraint {
        if self.is_bottom() {
            return Constraint::False; // x in [empty] is false
        }
        
        let var_expr = Expr::Var(var_name.to_string());
        
        // Create low_bound_expr and high_bound_expr from self.low and self.high
        // This requires converting B to Expr::Cst(Q).
        // If B is FloatBound, self.low.to_rational() might panic for Inf.
        // We need a way to represent infinity in Expr if bounds are infinite.
        // For now, if bounds are infinite, we might omit that part of the constraint.

        let low_is_neg_inf = self.low.classify() == crate::arith_bound::BoundKind::NegativeInfinity;
        let high_is_pos_inf = self.high.classify() == crate::arith_bound::BoundKind::PositiveInfinity;

        let mut constraints = Vec::new();

        if !low_is_neg_inf {
            // This will panic if self.low is +Inf (which would mean a bottom interval, caught above)
            // or if to_rational() panics for some other reason for self.low
            let low_val_q = self.low.to_rational();
            constraints.push(Constraint::Cmp(Comparison {
                e1: var_expr.clone(),
                op: CmpOp::Geq,
                e2: Expr::Cst(low_val_q),
            }));
        }

        if !high_is_pos_inf {
            let high_val_q = self.high.to_rational();
            constraints.push(Constraint::Cmp(Comparison {
                e1: var_expr.clone(), // Must clone var_expr if used in previous constraint
                op: CmpOp::Leq,
                e2: Expr::Cst(high_val_q),
            }));
        }

        if constraints.is_empty() { // Both were infinite, so effectively no constraint or "true"
            Constraint::True 
        } else if constraints.len() == 1 {
            constraints.pop().unwrap()
        } else {
            Constraint::And(Box::new(constraints[0].clone()), Box::new(constraints[1].clone()))
        }
    }
    

    // --- Measure (Implement some, others later) ---
    fn float_size(&self) -> f64 {
        if self.is_bottom() { return 0.0; } // Or NAN
        let (low_f, high_f) = self.to_float_range();
        if low_f.is_infinite() || high_f.is_infinite() {
            f64::INFINITY
        } else {
            high_f - low_f // This should use rounded subtraction for a bound on the size
                           // For now, standard f64 subtraction
        }
    }
    fn score(&self) -> f64 { unimplemented!("score for Interval") } // OCaml details needed


    // --- Set-theoretic Operations ---
    fn join(&self, other: &Self) -> Self { // Union
        if self.is_bottom() { return other.clone(); }
        if other.is_bottom() { return self.clone(); }
        Interval::new(self.low.min_val(&other.low), self.high.max_val(&other.high))
    }

    fn meet(&self, other: &Self) -> Self { // Intersection
        if self.is_bottom() || other.is_bottom() { return Self::bottom(); }
        let low_meet = self.low.max_val(&other.low);
        let high_meet = self.high.min_val(&other.high);
        if low_meet.gt(&high_meet) { // Check if intersection is empty
            Self::bottom()
        } else {
            Interval::new(low_meet, high_meet)
        }
    }

    fn meet_opt(&self, other: &Self) -> Option<Self> {
        let result = self.meet(other);
        if result.is_bottom() { None } else { Some(result) }
    }

    // Split and Prune are more complex, defer for now.
    fn split(&self) -> Vec<Self> { unimplemented!("split for Interval") }


    // --- Predicates ---
    fn contains_f64(&self, val: f64) -> bool {
        if self.is_bottom() { return false; }
        let b_val_low = B::from_f64_down(val); // val as a bound, rounded down
        let b_val_up = B::from_f64_up(val);   // val as a bound, rounded up
        // For val to be contained in [low, high]:
        // low <= val_as_bound_down  AND  val_as_bound_up <= high
        self.low.leq(&b_val_low) && self.high.geq(&b_val_up)
    }

    fn is_positive(&self) -> bool { // True if low >= 0
        if self.is_bottom() { return false; } // Or true, depending on convention for empty set
        self.low.geq(&B::zero())
    }
    fn is_negative(&self) -> bool { // True if high <= 0
        if self.is_bottom() { return false; }
        self.high.leq(&B::zero())
    }
    fn is_singleton(&self) -> bool {
        if self.is_bottom() { return false; }
        self.low.equals(&self.high)
    }

    // --- Interval Arithmetic (Forward Evaluation) ---
    fn neg_itv(&self) -> Self {
        if self.is_bottom() { return Self::bottom(); }
        Interval::new(self.high.neg_op(), self.low.neg_op()) // Negation swaps and negates bounds
    }

    fn abs_itv(&self) -> Self {
        if self.is_bottom() { return Self::bottom(); }
        // If 0 is in [low, high], then abs low is 0. Otherwise min(abs(low), abs(high)).
        // Max is max(abs(low), abs(high)).
        let low_abs = self.low.abs_op();
        let high_abs = self.high.abs_op();
        
        if self.low.sign() * self.high.sign() <= 0 { // Interval contains 0 (or is [0,0] or [neg,pos])
            Interval::new(B::zero(), low_abs.max_val(&high_abs))
        } else { // Interval is all positive or all negative
            Interval::new(low_abs.min_val(&high_abs), low_abs.max_val(&high_abs))
        }
    }

    fn add_itv(&self, other: &Self) -> Self {
        if self.is_bottom() || other.is_bottom() { return Self::bottom(); }
        Interval::new(self.low.add_down(&other.low), self.high.add_up(&other.high))
    }

    fn sub_itv(&self, other: &Self) -> Self {
        if self.is_bottom() || other.is_bottom() { return Self::bottom(); }
        Interval::new(self.low.sub_down(&other.high), self.high.sub_up(&other.low))
    }

    // mul_itv, div_itv, pow_itv etc. are more complex due to case analysis with signs and infinities.
    // Deferring these to a subsequent subtask.
    fn mul_itv(&self, other: &Self) -> Self { unimplemented!("mul_itv for Interval") }
    fn div_itv(&self, other: &Self) -> Option<Self> { unimplemented!("div_itv for Interval") }
    fn min_itv(&self, other: &Self) -> Self { unimplemented!("min_itv for Interval") }
    fn max_itv(&self, other: &Self) -> Self { unimplemented!("max_itv for Interval") }
    fn pow_itv(&self, exp: &Self) -> Self { unimplemented!("pow_itv for Interval") }
    fn n_root_itv(&self, n: &Self) -> Option<Self> { unimplemented!("n_root_itv for Interval") }
    fn exp_itv(&self) -> Self { unimplemented!("exp_itv for Interval") }
    fn ln_itv(&self) -> Option<Self> { unimplemented!("ln_itv for Interval") }
    fn log_itv(&self) -> Option<Self> { unimplemented!("log_itv for Interval") }
    fn spawn_f64(&self) -> f64 { unimplemented!("spawn_f64 for Interval") }
}

// IntervalOps will be implemented in a separate step.

// Add basic tests for implemented IntervalEvalOps methods
#[cfg(test)]
mod tests {
    use super::*;
    use crate::interval::bound_float::FloatBound; // Example Bound type
    type TestInterval = Interval<FloatBound>;

    #[test]
    fn test_interval_constructors_and_bottom() {
        assert!(TestInterval::new(FloatBound(1.0), FloatBound(0.0)).is_bottom()); // low > high
        assert!(TestInterval::bottom().is_bottom());
        assert_eq!(TestInterval::bottom().to_string(), "[empty]");

        let i1 = TestInterval::of_ints(1, 5);
        assert_eq!(i1.low, FloatBound(1.0));
        assert_eq!(i1.high, FloatBound(5.0));
        assert_eq!(i1.to_string(), "[1; 5]"); // FloatBound(1.0) displays as "1"

        let top = TestInterval::top_real();
        assert_eq!(top.low, FloatBound::negative_infinity());
        assert_eq!(top.high, FloatBound::positive_infinity());
        assert_eq!(top.to_string(), "[-oo; +oo]");
    }

    #[test]
    fn test_interval_to_constraint() {
        let i1 = TestInterval::of_ints(1, 5);
        // x >= 1 && x <= 5
        let c1 = i1.to_constraint("x");
        if let Constraint::And(ref geq_box, ref leq_box) = c1 {
            if let Constraint::Cmp(ref geq_cmp) = **geq_box { assert_eq!(geq_cmp.op, CmpOp::Geq); } 
            else { panic!("Expected Geq"); }
            if let Constraint::Cmp(ref leq_cmp) = **leq_box { assert_eq!(leq_cmp.op, CmpOp::Leq); }
            else { panic!("Expected Leq"); }
        } else { panic!("Expected And constraint, got {}", c1); }

        let i_pos_inf = TestInterval::new(FloatBound(0.0), FloatBound::positive_infinity());
        // x >= 0
        let c_pos_inf = i_pos_inf.to_constraint("x");
        if let Constraint::Cmp(ref geq_cmp) = c_pos_inf { assert_eq!(geq_cmp.op, CmpOp::Geq); }
        else { panic!("Expected single Geq constraint for [0, +oo), got {}", c_pos_inf); }
        
        let i_neg_inf = TestInterval::new(FloatBound::negative_infinity(), FloatBound(0.0));
        // x <= 0
        let c_neg_inf = i_neg_inf.to_constraint("x");
         if let Constraint::Cmp(ref leq_cmp) = c_neg_inf { assert_eq!(leq_cmp.op, CmpOp::Leq); }
        else { panic!("Expected single Leq constraint for (-oo, 0], got {}", c_neg_inf); }

        assert_eq!(TestInterval::top_real().to_constraint("x"), Constraint::True);
        assert_eq!(TestInterval::bottom().to_constraint("x"), Constraint::False);
    }

    #[test]
    fn test_interval_set_ops() {
        let i1 = TestInterval::of_ints(0, 5);
        let i2 = TestInterval::of_ints(3, 7);
        let i3 = TestInterval::of_ints(8, 10);

        // Join
        assert_eq!(i1.join(&i2), TestInterval::of_ints(0, 7));
        assert_eq!(i1.join(&TestInterval::bottom()), i1);

        // Meet
        assert_eq!(i1.meet(&i2), TestInterval::of_ints(3, 5));
        assert!(i1.meet(&i3).is_bottom());
        assert!(i1.meet(&TestInterval::bottom()).is_bottom());
        
        assert_eq!(i1.meet_opt(&i2), Some(TestInterval::of_ints(3,5)));
        assert_eq!(i1.meet_opt(&i3), None);
    }

    #[test]
    fn test_interval_predicates() {
        let i1 = TestInterval::of_ints(1, 5);
        let i2 = TestInterval::of_ints(-5, -1);
        let i3 = TestInterval::of_ints(-2, 2);
        let i_singleton = TestInterval::of_i64(3);

        assert!(i1.contains_f64(3.0));
        assert!(!i1.contains_f64(0.0));
        assert!(!TestInterval::bottom().contains_f64(0.0));

        assert!(i1.is_positive());
        assert!(!i2.is_positive());
        assert!(!i3.is_positive()); // Contains 0, but not all > 0

        assert!(i2.is_negative());
        assert!(!i1.is_negative());
        assert!(!i3.is_negative());

        assert!(i_singleton.is_singleton());
        assert!(!i1.is_singleton());
    }
    
    #[test]
    fn test_interval_core_arith() {
        let i1 = TestInterval::of_ints(1, 5);    // [1,5]
        let i2 = TestInterval::of_ints(-1, 3);   // [-1,3]

        // Negation: -[1,5] = [-5, -1]
        assert_eq!(i1.neg_itv(), TestInterval::of_ints(-5, -1));
        assert!(TestInterval::bottom().neg_itv().is_bottom());

        // Absolute value
        assert_eq!(i1.abs_itv(), TestInterval::of_ints(1, 5));      // abs([1,5]) = [1,5]
        assert_eq!(TestInterval::of_ints(-5, -1).abs_itv(), TestInterval::of_ints(1,5)); // abs([-5,-1]) = [1,5]
        assert_eq!(i2.abs_itv(), TestInterval::of_ints(0, 3));      // abs([-1,3]) = [0,3] (max_abs is 3, contains 0)

        // Addition: [1,5] + [-1,3] = [1+(-1), 5+3] = [0, 8]
        assert_eq!(i1.add_itv(&i2), TestInterval::of_ints(0, 8));

        // Subtraction: [1,5] - [-1,3] = [1-3, 5-(-1)] = [-2, 6]
        assert_eq!(i1.sub_itv(&i2), TestInterval::of_ints(-2, 6));
    }
}
