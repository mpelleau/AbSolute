use std::cmp::Ordering;
use std::fmt::{self, Display, Debug};
use std::str::FromStr; // For from_string_up/down parsing Rational
use rug::{Rational, Float}; // Rational is Q, Float is for f64 conversions with precision

use crate::arith_bound::{BoundOps, BoundKind};
use crate::arith_q::Q; // Alias for Rational, use directly for clarity sometimes
use crate::interval::bound_float::FloatBound; // For converting via float operations

// Enum to represent a rational bound that can be finite or +/- infinity.
#[derive(Clone, Debug, PartialEq, Eq)] // rug::Rational supports Eq
pub enum RationalBound {
    Finite(Rational),
    NegativeInfinity,
    PositiveInfinity,
    // No explicit 'Invalid' like FloatBound for NaN, as Rational doesn't have NaN.
    // Operations resulting in undefined (e.g. 0/0) should panic or return error/specific bound.
}

impl Display for RationalBound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RationalBound::Finite(r) => crate::arith_q::pp_print_q(r).fmt(f), // Use existing q_print
            RationalBound::NegativeInfinity => write!(f, "-oo"),
            RationalBound::PositiveInfinity => write!(f, "+oo"),
        }
    }
}

// Implementation of BoundOps for RationalBound
impl BoundOps for RationalBound {
    // --- Ordering ---
    fn compare(&self, other: &Self) -> Ordering {
        match (self, other) {
            (RationalBound::NegativeInfinity, RationalBound::NegativeInfinity) => Ordering::Equal,
            (RationalBound::NegativeInfinity, _) => Ordering::Less,
            (_, RationalBound::NegativeInfinity) => Ordering::Greater,

            (RationalBound::PositiveInfinity, RationalBound::PositiveInfinity) => Ordering::Equal,
            (RationalBound::PositiveInfinity, _) => Ordering::Greater,
            (_, RationalBound::PositiveInfinity) => Ordering::Less,

            (RationalBound::Finite(r1), RationalBound::Finite(r2)) => r1.cmp(r2),
        }
    }

    fn leq(&self, other: &Self) -> bool { self.compare(other).is_le() }
    fn geq(&self, other: &Self) -> bool { self.compare(other).is_ge() }
    fn lt(&self, other: &Self) -> bool { self.compare(other).is_lt() }
    fn gt(&self, other: &Self) -> bool { self.compare(other).is_gt() }

    fn is_odd(&self) -> bool {
        match self {
            RationalBound::Finite(r) => r.is_integer() && r.to_integer_ref().map_or(false, |i| i.is_odd()),
            _ => false, // Infinity is not odd/even
        }
    }
    fn is_even(&self) -> bool {
        match self {
            RationalBound::Finite(r) => r.is_integer() && r.to_integer_ref().map_or(false, |i| i.is_even()),
            _ => false,
        }
    }

    fn min_val(&self, other: &Self) -> Self {
        if self.leq(other) { self.clone() } else { other.clone() }
    }
    fn max_val(&self, other: &Self) -> Self {
        if self.geq(other) { self.clone() } else { other.clone() }
    }
    
    fn sign(&self) -> i32 {
        match self {
            RationalBound::Finite(r) => r.cmp0().into(), // r.cmp0() is Ordering, .into() gives -1,0,1
            RationalBound::NegativeInfinity => -1,
            RationalBound::PositiveInfinity => 1,
        }
    }

    // --- Classification ---
    fn classify(&self) -> BoundKind {
        match self {
            RationalBound::Finite(_) => BoundKind::Finite,
            RationalBound::NegativeInfinity => BoundKind::NegativeInfinity,
            RationalBound::PositiveInfinity => BoundKind::PositiveInfinity,
            // No RationalBound::Invalid, so this case shouldn't be hit from Self.
            // If an operation produced an invalid state not representable, it should have panicked
            // or returned an error earlier.
        }
    }
    
    // --- Useful Constants ---
    fn zero() -> Self { RationalBound::Finite(Q::from(0)) }
    fn one() -> Self { RationalBound::Finite(Q::from(1)) }
    fn two() -> Self { RationalBound::Finite(Q::from(2)) }
    fn minus_one() -> Self { RationalBound::Finite(Q::from(-1)) }
    fn positive_infinity() -> Self { RationalBound::PositiveInfinity }
    fn negative_infinity() -> Self { RationalBound::NegativeInfinity }

    // --- Construction (with directed rounding) ---
    // For rationals, _up and _down are often same as exact for from_i64, from_rational.
    // Rounding applies mainly when converting from f64 or string that might be non-exact.
    fn from_i64_up(val: i64) -> Self { RationalBound::Finite(Q::from(val)) }
    fn from_i64_down(val: i64) -> Self { RationalBound::Finite(Q::from(val)) }

    fn from_f64_up(val: f64) -> Self {
        if val.is_nan() { panic!("Cannot convert NaN to RationalBound"); } // Or return a specific error/convention
        if val == f64::INFINITY { return RationalBound::PositiveInfinity; }
        if val == f64::NEG_INFINITY { return RationalBound::NegativeInfinity; }
        // Convert f64 to Rational. Standard conversion is typically best effort or round-to-nearest.
        // To ensure "upward" for RationalBound means "at least val":
        // If Rational::from(val) is exact or already rounded up by f64, it's fine.
        // If not, we need the smallest Rational >= val.
        // This is complex. For now, use standard conversion, then compare and adjust if possible.
        // rug::Rational::from_f64 is Option<Rational>.
        let r_opt = Rational::from_f64(val);
        match r_opt {
            Some(r) => {
                // If r < val (due to round-to-nearest being lower), we need next rational up.
                // This is non-trivial. For now, assume r is "good enough" or from_f64 is best-effort.
                // A more robust approach might involve converting f64 to FloatBound, using its _up logic,
                // then converting that FloatBound result (which is f64) back to Rational.
                // Or, use Float with sufficient precision.
                let fb_equiv = FloatBound::from_f64_up(val); // This gives an f64 rounded up
                if fb_equiv.classify() == BoundKind::PositiveInfinity { RationalBound::PositiveInfinity }
                else if fb_equiv.classify() == BoundKind::NegativeInfinity { RationalBound::NegativeInfinity }
                else {
                    // Convert the (potentially rounded by f64 precision) f64 back to Rational
                    // This might not be perfectly "up" in rational sense if f64 precision was limiting.
                    Rational::from_f64(fb_equiv.0).map(RationalBound::Finite)
                        .unwrap_or_else(|| panic!("Could not convert f64_up result back to Rational"))
                }
            }
            None => panic!("Cannot convert f64 to RationalBound: {}", val), // Should be caught by NaN/Inf earlier
        }
    }
    fn from_f64_down(val: f64) -> Self {
        if val.is_nan() { panic!("Cannot convert NaN to RationalBound"); }
        if val == f64::INFINITY { return RationalBound::PositiveInfinity; }
        if val == f64::NEG_INFINITY { return RationalBound::NegativeInfinity; }
        
        let fb_equiv = FloatBound::from_f64_down(val); // This gives an f64 rounded down
        if fb_equiv.classify() == BoundKind::PositiveInfinity { RationalBound::PositiveInfinity }
        else if fb_equiv.classify() == BoundKind::NegativeInfinity { RationalBound::NegativeInfinity }
        else {
             Rational::from_f64(fb_equiv.0).map(RationalBound::Finite)
                .unwrap_or_else(|| panic!("Could not convert f64_down result back to Rational"))
        }
    }

    fn from_rational_up(r: &Rational) -> Self { RationalBound::Finite(r.clone()) } // Exact
    fn from_rational_down(r: &Rational) -> Self { RationalBound::Finite(r.clone()) } // Exact
    
    fn from_string_up(s: &str) -> Result<Self, String> {
        let s_lower = s.trim().to_lowercase();
        match s_lower.as_str() {
            "inf" | "+inf" | "infinity" | "+infinity" => Ok(RationalBound::PositiveInfinity),
            "-inf" | "-infinity" => Ok(RationalBound::NegativeInfinity),
            "nan" => Err("NaN is not a valid RationalBound".to_string()), // No NaN for Rationals
            _ => Rational::from_str_radix(&s_lower, 10)
                .map(RationalBound::Finite)
                .map_err(|e: rug::rational::ParseRationalError| format!("Failed to parse string '{}' as Rational: {:?}", s, e.kind())),
        }
    }
    fn from_string_down(s: &str) -> Result<Self, String> {
        // For rationals, up/down from string is same as exact parse if format is valid
        Self::from_string_up(s)
    }

    // --- Conversion ---
    fn to_f64_up(&self) -> f64 {
        match self {
            RationalBound::Finite(r) => FloatBound::from_rational_up(r).0, // Use FloatBound's logic
            RationalBound::PositiveInfinity => f64::INFINITY,
            RationalBound::NegativeInfinity => f64::NEG_INFINITY,
        }
    }
    fn to_f64_down(&self) -> f64 {
        match self {
            RationalBound::Finite(r) => FloatBound::from_rational_down(r).0, // Use FloatBound's logic
            RationalBound::PositiveInfinity => f64::INFINITY,
            RationalBound::NegativeInfinity => f64::NEG_INFINITY,
        }
    }
    fn to_rational(&self) -> Rational {
        match self {
            RationalBound::Finite(r) => r.clone(),
            RationalBound::PositiveInfinity => panic!("Cannot convert +oo RationalBound to finite Rational"),
            RationalBound::NegativeInfinity => panic!("Cannot convert -oo RationalBound to finite Rational"),
        }
    }
    
    // --- Operators (exact for finite rationals) ---
    // (neg_op, abs_op, add_up/down etc. to be implemented in next subtask)
    fn neg_op(&self) -> Self { unimplemented!("neg_op for RationalBound") }
    fn abs_op(&self) -> Self { unimplemented!("abs_op for RationalBound") }
    fn add_up(&self, other: &Self) -> Self { unimplemented!("add_up for RationalBound") }
    // ... rest of arithmetic and math functions ...
    fn sub_up(&self, other: &Self) -> Self { unimplemented!("sub_up for RationalBound") }
    fn mul_up(&self, other: &Self) -> Self { unimplemented!("mul_up for RationalBound") }
    fn div_up(&self, other: &Self) -> Self { unimplemented!("div_up for RationalBound") }
    fn add_down(&self, other: &Self) -> Self { unimplemented!("add_down for RationalBound") }
    fn sub_down(&self, other: &Self) -> Self { unimplemented!("sub_down for RationalBound") }
    fn mul_down(&self, other: &Self) -> Self { unimplemented!("mul_down for RationalBound") }
    fn div_down(&self, other: &Self) -> Self { unimplemented!("div_down for RationalBound") }
    fn sqrt_up(&self) -> Self { unimplemented!("sqrt_up for RationalBound") }
    fn sqrt_down(&self) -> Self { unimplemented!("sqrt_down for RationalBound") }
    fn pow_up(&self, exp: i32) -> Self { unimplemented!("pow_up for RationalBound") }
    fn pow_down(&self, exp: i32) -> Self { unimplemented!("pow_down for RationalBound") }
    fn root_up(&self, n: i32) -> Self { unimplemented!("root_up for RationalBound") }
    fn root_down(&self, n: i32) -> Self { unimplemented!("root_down for RationalBound") }
    fn cos_up(&self) -> Self { unimplemented!("cos_up for RationalBound") }
    fn cos_down(&self) -> Self { unimplemented!("cos_down for RationalBound") }
    fn sin_up(&self) -> Self { unimplemented!("sin_up for RationalBound") }
    fn sin_down(&self) -> Self { unimplemented!("sin_down for RationalBound") }
    fn tan_up(&self) -> Self { unimplemented!("tan_up for RationalBound") }
    fn tan_down(&self) -> Self { unimplemented!("tan_down for RationalBound") }
    fn acos_up(&self) -> Self { unimplemented!("acos_up for RationalBound") }
    fn acos_down(&self) -> Self { unimplemented!("acos_down for RationalBound") }
    fn asin_up(&self) -> Self { unimplemented!("asin_up for RationalBound") }
    fn asin_down(&self) -> Self { unimplemented!("asin_down for RationalBound") }
    fn atan_up(&self) -> Self { unimplemented!("atan_up for RationalBound") }
    fn atan_down(&self) -> Self { unimplemented!("atan_down for RationalBound") }
    fn exp_up(&self) -> Self { unimplemented!("exp_up for RationalBound") }
    fn exp_down(&self) -> Self { unimplemented!("exp_down for RationalBound") }
    fn ln_up(&self) -> Self { unimplemented!("ln_up for RationalBound") }
    fn ln_down(&self) -> Self { unimplemented!("ln_down for RationalBound") }
    fn log_up(&self) -> Self { unimplemented!("log_up for RationalBound") }
    fn log_down(&self) -> Self { unimplemented!("log_down for RationalBound") }
    fn floor_val(&self) -> Self { unimplemented!("floor_val for RationalBound") }
    fn ceil_val(&self) -> Self { unimplemented!("ceil_val for RationalBound") }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arith_bound::BoundKind;
    use rug::Rational;

    #[test]
    fn test_rational_bound_display() {
        assert_eq!(RationalBound::Finite(Rational::from((5,1))).to_string(), "5");
        assert_eq!(RationalBound::Finite(Rational::from((1,2))).to_string(), "1/2"); // Uses arith_q::pp_print_q
        assert_eq!(RationalBound::PositiveInfinity.to_string(), "+oo");
        assert_eq!(RationalBound::NegativeInfinity.to_string(), "-oo");
    }

    #[test]
    fn test_rational_bound_ordering_and_constants() {
        assert!(RationalBound::negative_infinity() < RationalBound::zero());
        assert!(RationalBound::zero() < RationalBound::positive_infinity());
        assert_eq!(RationalBound::Finite(Rational::from(5)).compare(&RationalBound::Finite(Rational::from(2))), Ordering::Greater);
        assert_eq!(RationalBound::positive_infinity().classify(), BoundKind::PositiveInfinity);
    }

    #[test]
    fn test_rational_bound_from_i64() {
        assert_eq!(RationalBound::from_i64_up(123), RationalBound::Finite(Rational::from(123)));
        assert_eq!(RationalBound::from_i64_down(-123), RationalBound::Finite(Rational::from(-123)));
    }
    
    #[test]
    fn test_rational_bound_from_f64() {
        // Relies on FloatBound's from_f64_up/down and then Rational::from_f64
        // This test is more of an integration check for that path
        assert_eq!(RationalBound::from_f64_up(0.5), RationalBound::Finite(Rational::from((1,2))));
        assert_eq!(RationalBound::from_f64_down(0.5), RationalBound::Finite(Rational::from((1,2))));
        
        // Test with a number f64 can't represent perfectly but Rational can via FloatBound intermediate
        // 0.1 is exact in f64, so this should be exact.
        assert_eq!(RationalBound::from_f64_up(0.1), RationalBound::Finite(Rational::from_f64(0.1).unwrap()));

        assert_eq!(RationalBound::from_f64_up(f64::INFINITY), RationalBound::PositiveInfinity);
        assert_eq!(RationalBound::from_f64_down(f64::NEG_INFINITY), RationalBound::NegativeInfinity);
    }
    #[test]
    #[should_panic]
    fn test_rational_bound_from_f64_nan() {
        RationalBound::from_f64_up(f64::NAN);
    }

    #[test]
    fn test_rational_bound_from_rational() {
        let r = Rational::from((123, 456));
        assert_eq!(RationalBound::from_rational_up(&r), RationalBound::Finite(r.clone()));
        assert_eq!(RationalBound::from_rational_down(&r), RationalBound::Finite(r));
    }
    
    #[test]
    fn test_rational_bound_from_string() {
        assert_eq!(RationalBound::from_string_up("123/456"), Ok(RationalBound::Finite(Rational::from((123,456)))));
        assert_eq!(RationalBound::from_string_down("inf"), Ok(RationalBound::PositiveInfinity));
        assert_eq!(RationalBound::from_string_up("-inf"), Ok(RationalBound::NegativeInfinity));
        assert!(RationalBound::from_string_up("NaN").is_err()); // No NaN for RationalBound
        assert!(RationalBound::from_string_up("invalid").is_err());
    }

    #[test]
    fn test_rational_bound_to_f64() {
        // Relies on FloatBound's from_rational_up/down
        assert_eq!(RationalBound::Finite(Rational::from((1,2))).to_f64_up(), 0.5);
        assert_eq!(RationalBound::Finite(Rational::from((1,2))).to_f64_down(), 0.5);
        assert_eq!(RationalBound::PositiveInfinity.to_f64_up(), f64::INFINITY);
        assert_eq!(RationalBound::NegativeInfinity.to_f64_down(), f64::NEG_INFINITY);
        // Test for a rational that may not be perfectly f64 representable
        // e.g. 1/3. FloatBound::from_rational_up(&(1/3)) should give f64 slightly > 1/3
        let one_third = Rational::from((1,3));
        let fb_one_third_up = FloatBound::from_rational_up(&one_third).0;
        let fb_one_third_down = FloatBound::from_rational_down(&one_third).0;
        assert_eq!(RationalBound::Finite(one_third.clone()).to_f64_up(), fb_one_third_up);
        assert_eq!(RationalBound::Finite(one_third).to_f64_down(), fb_one_third_down);
    }

    #[test]
    #[should_panic]
    fn test_rational_bound_to_rational_inf() {
        RationalBound::PositiveInfinity.to_rational();
    }
}
