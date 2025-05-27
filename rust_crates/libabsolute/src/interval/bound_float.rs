use std::cmp::Ordering;
use std::fmt::{self, Display, Debug};
use std::hash::{Hash, Hasher}; // f64 doesn't impl Hash directly, wrapper needs to handle it if used as key
use rug::Rational;
use rug::rational::ParseRationalError; // For error from Rational::from_str_radix

use crate::arith_bound::{BoundOps, BoundKind};
use crate::fpu_control; // For setting FPU rounding modes

// Wrapper around f64 to represent a bound that can be +/- infinity or NaN (Invalid).
// Standard f64 already has INFINITY, NEG_INFINITY, NAN.
// We need to ensure our operations correctly handle these and implement directed rounding.
#[derive(Clone, Copy, Debug)]
pub struct FloatBound(pub f64);

// Manual PartialEq because f64's is problematic with NaN (NaN != NaN)
impl PartialEq for FloatBound {
    fn eq(&self, other: &Self) -> bool {
        if self.0.is_nan() && other.0.is_nan() {
            true // Treat NaNs as equal for our purposes if they both represent "Invalid"
        } else {
            self.0 == other.0
        }
    }
}
impl Eq for FloatBound {} // Mark that our PartialEq is an equivalence relation

// Note: f64 does not implement Ord because NaN is not comparable.
// BoundOps::compare requires a total order. We need to define one.
// A common way is: -Inf < Finite < +Inf < NaN (or NaN is smallest/largest, or incomparable leading to Option<Ordering>)
// OCaml's `Float.compare` provides a total order where NaN is greater than everything.
impl PartialOrd for FloatBound {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.compare(other)) // Delegate to our total `compare`
    }
}

impl Ord for FloatBound {
    fn cmp(&self, other: &Self) -> Ordering {
        self.compare(other) // Delegate to our total `compare`
    }
}


// It can be useful to implement hashing if FloatBound instances are used in HashMaps/HashSets.
// Hashing f64s requires careful handling of NaN and +/-0.0 if distinct.
// For now, if hashing is needed, it should be based on canonical bits if possible.
// This is a placeholder; a robust f64 hash is non-trivial.
impl Hash for FloatBound {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Simplified hash: just use raw bits.
        // This means +0.0 and -0.0 might hash differently if their bit patterns differ.
        // NaN values will also hash to their specific bit patterns.
        // For consistent hashing where NaNs are treated as one class, more logic is needed.
        self.0.to_bits().hash(state);
    }
}


impl Display for FloatBound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_nan() {
            write!(f, "NaN") // Or "INVALID" to match BoundKind
        } else if self.0 == f64::INFINITY {
            write!(f, "+oo")
        } else if self.0 == f64::NEG_INFINITY {
            write!(f, "-oo")
        } else {
            // Use our arith_f printing logic if available and suitable,
            // or standard float printing.
            // For now, standard, can refine with arith_f::pp_print_f if needed
            write!(f, "{}", self.0)
        }
    }
}

// Start of BoundOps implementation (to be continued in next subtask)
impl BoundOps for FloatBound {
    // --- Ordering ---
    fn compare(&self, other: &Self) -> Ordering {
        // Implement total order: -Inf < Finite < +Inf < NaN
        // This matches OCaml's Float.compare behavior.
        self.0.total_cmp(&other.0)
    }

    fn leq(&self, other: &Self) -> bool {
        self.compare(other).is_le()
    }
    fn geq(&self, other: &Self) -> bool {
        self.compare(other).is_ge()
    }
    fn lt(&self, other: &Self) -> bool {
        self.compare(other).is_lt()
    }
    fn gt(&self, other: &Self) -> bool {
        self.compare(other).is_gt()
    }

    fn is_odd(&self) -> bool {
        if self.classify() == BoundKind::Finite && self.0.fract() == 0.0 {
            (self.0 as i64) % 2 != 0
        } else {
            false // Or based on specific requirements for non-integers/infinities
        }
    }
    fn is_even(&self) -> bool {
        if self.classify() == BoundKind::Finite && self.0.fract() == 0.0 {
            (self.0 as i64) % 2 == 0
        } else {
            false
        }
    }

    fn min_val(&self, other: &Self) -> Self {
        if self.lt(other) { self.clone() } else { other.clone() }
    }
    fn max_val(&self, other: &Self) -> Self {
        if self.gt(other) { self.clone() } else { other.clone() }
    }
    
    fn sign(&self) -> i32 {
        match self.classify() {
            BoundKind::Finite => {
                if self.0 > 0.0 { 1 }
                else if self.0 < 0.0 { -1 }
                else { 0 }
            }
            BoundKind::PositiveInfinity => 1,
            BoundKind::NegativeInfinity => -1,
            BoundKind::Invalid => 0, // Or panic, OCaml sign on NaN is usually 0
        }
    }

    // --- Classification ---
    fn classify(&self) -> BoundKind {
        if self.0.is_nan() {
            BoundKind::Invalid
        } else if self.0 == f64::INFINITY {
            BoundKind::PositiveInfinity
        } else if self.0 == f64::NEG_INFINITY {
            BoundKind::NegativeInfinity
        } else {
            BoundKind::Finite
        }
    }
    
    // --- Useful Constants ---
    fn zero() -> Self { FloatBound(0.0) }
    fn one() -> Self { FloatBound(1.0) }
    fn two() -> Self { FloatBound(2.0) }
    fn minus_one() -> Self { FloatBound(-1.0) }
    fn positive_infinity() -> Self { FloatBound(f64::INFINITY) }
    fn negative_infinity() -> Self { FloatBound(f64::NEG_INFINITY) }

    // --- Construction (with directed rounding) ---
    fn from_i64_up(val: i64) -> Self {
        // i64 to f64 conversion is exact for values within f64's precise integer range.
        // Rounding mode doesn't affect exact conversions.
        FloatBound(val as f64)
    }
    fn from_i64_down(val: i64) -> Self {
        FloatBound(val as f64)
    }

    fn from_f64_up(val: f64) -> Self {
        if val.is_nan() { return FloatBound(f64::NAN); }
        if val == f64::INFINITY { return FloatBound::positive_infinity(); }
        if val == f64::NEG_INFINITY { return FloatBound::negative_infinity(); }
        FloatBound(val)
    }
    fn from_f64_down(val: f64) -> Self {
        FloatBound(val)
    }

    fn from_rational_up(r: &Rational) -> Self {
        if r.is_finite() {
            let f_val = fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || {
                let f_nearest = r.to_f64();
                if Rational::from_f64(f_nearest).map_or(true, |r_conv| r_conv < *r) {
                    if f_nearest.is_finite() && f_nearest < f64::MAX {
                        f_nearest.next_up()
                    } else { f_nearest }
                } else { f_nearest }
            });
            FloatBound(f_val)
        } else if r > &Rational::from(0) { FloatBound::positive_infinity() }
        else if r < &Rational::from(0) { FloatBound::negative_infinity() }
        else { FloatBound(f64::NAN) } 
    }

    fn from_rational_down(r: &Rational) -> Self {
        if r.is_finite() {
            let f_val = fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || {
                let f_nearest = r.to_f64();
                 if Rational::from_f64(f_nearest).map_or(true, |r_conv| r_conv > *r) {
                     if f_nearest.is_finite() && f_nearest > f64::MIN_POSITIVE_SUBSEQUENT { 
                         f_nearest.next_down()
                     } else { f_nearest }
                 } else { f_nearest }
            });
            FloatBound(f_val)
        } else if r > &Rational::from(0) { FloatBound::positive_infinity() }
        else if r < &Rational::from(0) { FloatBound::negative_infinity() }
        else { FloatBound(f64::NAN) }
    }
    
    fn from_string_up(s: &str) -> Result<Self, String> {
        // OCaml's Bound.of_string_up likely handles "inf", "-inf", "nan" or similar.
        // Standard float parsing also handles these.
        // Let's check for special strings first.
        let s_lower = s.trim().to_lowercase();
        match s_lower.as_str() {
            "inf" | "+inf" | "infinity" | "+infinity" => return Ok(FloatBound::positive_infinity()),
            "-inf" | "-infinity" => return Ok(FloatBound::negative_infinity()),
            "nan" => return Ok(FloatBound(f64::NAN)),
            _ => {} // Fall through to rational parsing
        }

        // Try parsing as Rational for precision, then convert.
        // Assuming base 10 for strings.
        match Rational::from_str_radix(&s_lower, 10) {
            Ok(r) => Ok(Self::from_rational_up(&r)),
            Err(e) => {
                // If Rational parsing fails, try direct f64 parsing as a fallback
                // though this might lose the directed rounding intent if the string
                // represents a value needing it from the start.
                // However, OCaml's `Mpqf.of_string` is robust.
                // If `Rational::from_str_radix` fails, it's likely a malformed number.
                // Direct f64 parsing might succeed on "1.23e+10" where Rational might be pickier
                // or vice-versa. For now, stick to Rational then convert.
                Err(format!("Failed to parse string '{}' as Rational: {:?}", s, e))
            }
        }
    }

    fn from_string_down(s: &str) -> Result<Self, String> {
        let s_lower = s.trim().to_lowercase();
        match s_lower.as_str() {
            "inf" | "+inf" | "infinity" | "+infinity" => return Ok(FloatBound::positive_infinity()),
            "-inf" | "-infinity" => return Ok(FloatBound::negative_infinity()),
            "nan" => return Ok(FloatBound(f64::NAN)),
            _ => {}
        }

        match Rational::from_str_radix(&s_lower, 10) {
            Ok(r) => Ok(Self::from_rational_down(&r)),
            Err(e) => Err(format!("Failed to parse string '{}' as Rational: {:?}", s, e)),
        }
    }

    // --- Conversion ---
    fn to_f64_up(&self) -> f64 { 
        if self.0.is_nan() { return f64::NAN; }
        if self.0 == f64::NEG_INFINITY { return f64::NEG_INFINITY; }
        if self.0 == f64::INFINITY { return f64::INFINITY; }
        fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || self.0 + 0.0)
    }
    fn to_f64_down(&self) -> f64 {
         if self.0.is_nan() { return f64::NAN; }
        if self.0 == f64::NEG_INFINITY { return f64::NEG_INFINITY; }
        if self.0 == f64::INFINITY { return f64::INFINITY; }
        fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || self.0 + 0.0)
    }

    fn to_rational(&self) -> Rational {
        match self.classify() {
            BoundKind::Finite => Rational::from_f64(self.0).unwrap_or_else(|| {
                Rational::from_str_radix(&self.0.to_string(), 10).unwrap_or_default()
            }),
            BoundKind::PositiveInfinity => panic!("Cannot convert +oo FloatBound to Rational"),
            BoundKind::NegativeInfinity => panic!("Cannot convert -oo FloatBound to Rational"),
            BoundKind::Invalid => panic!("Cannot convert Invalid/NaN FloatBound to Rational"),
        }
    }

    // --- Operators (exact) ---
    fn neg_op(&self) -> Self { FloatBound(-self.0) } 
    fn abs_op(&self) -> Self { FloatBound(self.0.abs()) }

    // --- Operators (rounded) ---
    fn add_up(&self, other: &Self) -> Self {
        if self.0.is_nan() || other.0.is_nan() { return FloatBound(f64::NAN); }
        if self.0 == f64::INFINITY && other.0 == f64::NEG_INFINITY { return FloatBound(f64::NAN); }
        if self.0 == f64::NEG_INFINITY && other.0 == f64::INFINITY { return FloatBound(f64::NAN); }
        if self.0 == f64::INFINITY || other.0 == f64::INFINITY { return FloatBound(f64::INFINITY); }
        if self.0 == f64::NEG_INFINITY || other.0 == f64::NEG_INFINITY { return FloatBound(f64::NEG_INFINITY); }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || self.0 + other.0))
    }
    fn add_down(&self, other: &Self) -> Self {
        if self.0.is_nan() || other.0.is_nan() { return FloatBound(f64::NAN); }
        if self.0 == f64::INFINITY && other.0 == f64::NEG_INFINITY { return FloatBound(f64::NAN); }
        if self.0 == f64::NEG_INFINITY && other.0 == f64::INFINITY { return FloatBound(f64::NAN); }
        if self.0 == f64::NEG_INFINITY || other.0 == f64::NEG_INFINITY { return FloatBound(f64::NEG_INFINITY); }
        if self.0 == f64::INFINITY || other.0 == f64::INFINITY { return FloatBound(f64::INFINITY); }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || self.0 + other.0))
    }
    
    fn sub_up(&self, other: &Self) -> Self {
        if self.0.is_nan() || other.0.is_nan() { return FloatBound(f64::NAN); }
        if self.0 == f64::INFINITY && other.0 == f64::INFINITY { return FloatBound(f64::NAN); }
        if self.0 == f64::NEG_INFINITY && other.0 == f64::NEG_INFINITY { return FloatBound(f64::NAN); }
        if self.0 == f64::INFINITY || other.0 == f64::NEG_INFINITY { return FloatBound(f64::INFINITY); }
        if self.0 == f64::NEG_INFINITY || other.0 == f64::INFINITY { return FloatBound(f64::NEG_INFINITY); }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || self.0 - other.0))
    }
    fn sub_down(&self, other: &Self) -> Self {
        if self.0.is_nan() || other.0.is_nan() { return FloatBound(f64::NAN); }
        if self.0 == f64::INFINITY && other.0 == f64::INFINITY { return FloatBound(f64::NAN); }
        if self.0 == f64::NEG_INFINITY && other.0 == f64::NEG_INFINITY { return FloatBound(f64::NAN); }
        if self.0 == f64::NEG_INFINITY || other.0 == f64::INFINITY { return FloatBound(f64::NEG_INFINITY); }
        if self.0 == f64::INFINITY || other.0 == f64::NEG_INFINITY { return FloatBound(f64::INFINITY); }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || self.0 - other.0))
    }

    fn mul_up(&self, other: &Self) -> Self {
        let v1 = self.0; let v2 = other.0;
        if v1.is_nan() || v2.is_nan() { return FloatBound(f64::NAN); }
        if (v1.is_infinite() && v2 == 0.0) || (v2.is_infinite() && v1 == 0.0) { return FloatBound(f64::NAN); }
        if v1.is_infinite() || v2.is_infinite() {
            let sign1 = if v1 < 0.0 { -1.0 } else { 1.0 };
            let sign2 = if v2 < 0.0 { -1.0 } else { 1.0 };
            if sign1 * sign2 > 0.0 { return FloatBound(f64::INFINITY); }
            else { return FloatBound(f64::NEG_INFINITY); }
        }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || v1 * v2))
    }
    fn mul_down(&self, other: &Self) -> Self {
        let v1 = self.0; let v2 = other.0;
        if v1.is_nan() || v2.is_nan() { return FloatBound(f64::NAN); }
        if (v1.is_infinite() && v2 == 0.0) || (v2.is_infinite() && v1 == 0.0) { return FloatBound(f64::NAN); }
        if v1.is_infinite() || v2.is_infinite() {
            let sign1 = if v1 < 0.0 { -1.0 } else { 1.0 };
            let sign2 = if v2 < 0.0 { -1.0 } else { 1.0 };
            if sign1 * sign2 > 0.0 { return FloatBound(f64::INFINITY); }
            else { return FloatBound(f64::NEG_INFINITY); }
        }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || v1 * v2))
    }

    fn div_up(&self, other: &Self) -> Self {
        let v1 = self.0; let v2 = other.0;
        if v1.is_nan() || v2.is_nan() { return FloatBound(f64::NAN); }
        if v2 == 0.0 {
            if v1 == 0.0 { return FloatBound(f64::NAN); }
            else if v1 > 0.0 { return FloatBound(f64::INFINITY); }
            else { return FloatBound(f64::NEG_INFINITY); }
        }
        if v1.is_infinite() && v2.is_infinite() { return FloatBound(f64::NAN); }
        if v1.is_infinite() {
            if (v1.signum() * v2.signum()) > 0.0 { return FloatBound(f64::INFINITY); }
            else { return FloatBound(f64::NEG_INFINITY); }
        }
        if v2.is_infinite() { return FloatBound(0.0); }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || v1 / v2))
    }
    fn div_down(&self, other: &Self) -> Self {
        let v1 = self.0; let v2 = other.0;
        if v1.is_nan() || v2.is_nan() { return FloatBound(f64::NAN); }
        if v2 == 0.0 {
            if v1 == 0.0 { return FloatBound(f64::NAN); }
            else if v1 > 0.0 { return FloatBound(f64::INFINITY); }
            else { return FloatBound(f64::NEG_INFINITY); }
        }
        if v1.is_infinite() && v2.is_infinite() { return FloatBound(f64::NAN); }
        if v1.is_infinite() {
            if (v1.signum() * v2.signum()) > 0.0 { return FloatBound(f64::INFINITY); }
            else { return FloatBound(f64::NEG_INFINITY); }
        }
        if v2.is_infinite() { return FloatBound(0.0); }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || v1 / v2))
    }
    
    fn sqrt_up(&self) -> Self {
        if self.0 < 0.0 { FloatBound(f64::NAN) }
        else if self.0.is_nan() { FloatBound(f64::NAN) }
        else if self.0 == f64::INFINITY { FloatBound(f64::INFINITY) }
        else { FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || self.0.sqrt())) }
    }
    fn sqrt_down(&self) -> Self {
        if self.0 < 0.0 { FloatBound(f64::NAN) }
        else if self.0.is_nan() { FloatBound(f64::NAN) }
        else if self.0 == f64::INFINITY { FloatBound(f64::INFINITY) } 
        else { FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || self.0.sqrt())) }
    }
    
    fn pow_up(&self, exp: i32) -> Self {
        let val = self.0;
        if val.is_nan() { return FloatBound(f64::NAN); }

        if exp == 0 { return FloatBound::one(); } // x^0 = 1

        if val == 0.0 {
            return if exp > 0 { FloatBound::zero() } else { FloatBound(f64::INFINITY) }; // 0^positive = 0, 0^negative = +Inf
        }

        if val == 1.0 { return FloatBound::one(); } // 1^exp = 1
        if val == -1.0 {
            return if exp % 2 == 0 { FloatBound::one() } else { FloatBound::minus_one() }; // (-1)^exp
        }

        if val.is_infinite() {
            if exp < 0 { return FloatBound::zero(); } // Inf ^ neg_exp = 0
            return if val == f64::INFINITY || exp % 2 == 0 { // Inf^pos_exp or (-Inf)^even_pos_exp
                FloatBound(f64::INFINITY)
            } else { // (-Inf)^odd_pos_exp
                FloatBound(f64::NEG_INFINITY)
            };
        }
        
        // Finite base
        if exp < 0 {
            // x^-e = 1 / x^e. Calculate 1.0 / (val.powi(-exp)) with upward rounding for 1.0/result.
            // This means (val.powi(-exp)) should be calculated with downward rounding if val.powi(-exp) > 0
            // or upward if val.powi(-exp) < 0 to maximize the result of 1.0/result.
            // This gets complex. A simpler approach for x^-e = (1/x)^e
            // let inv_base_down = FloatBound(1.0).div_down(self); // 1/x, result rounded down
            // let inv_base_up = FloatBound(1.0).div_up(self);     // 1/x, result rounded up
            
            // Now (inv_base)^(-exp). Since -exp is positive.
            // We need to pick inv_base_down or inv_base_up to maximize result.
            // If inv_base is positive, use inv_base_up.
            // If inv_base is negative and -exp is odd, use inv_base_up (e.g. (-2)^3 = -8 vs (-3)^3 = -27, want -8).
            // If inv_base is negative and -exp is even, use inv_base_down (e.g. (-2)^2 = 4 vs (-3)^2 = 9, want 9, so use more negative base).
            // This logic is tricky. Let's use the property x^-|e| = 1/(x^|e|).
            // We want to maximize 1/denom. So minimize positive denom, or maximize negative denom (closer to 0).
            // let abs_exp = exp.abs();
            // let denom_down = self.pow_down(abs_exp); // Smallest positive or most negative result for x^|e|
            // let denom_up = self.pow_up(abs_exp);     // Largest positive or least negative result for x^|e|

            // We are calculating 1.0 / denom.
            // If denom is positive, we want smallest positive denom_down for 1.0/denom_down to be largest.
            // If denom is negative, we want largest negative denom_down (closest to zero) for 1.0/denom_down to be largest (most positive or least negative).
            // This is subtle. Let's use the property that for x^-exp, if x > 0, then (1/x)^exp.
            // If x is positive:
            if val > 0.0 {
                 // (1/x)^|exp|. To maximize this, maximize 1/x (so div_up).
                 // Then raise (1/x)_up to power |exp| with upward rounding.
                let one_div_x_up = FloatBound(1.0).div_up(self);
                return one_div_x_up.pow_up(exp.abs());
            } else { // val < 0.0
                // For (-2)^-2 = 1/4. (-2)^-3 = 1/-8.
                // Denom is self.pow_up(abs_exp) or self.pow_down(abs_exp)
                // If abs_exp is even, x^|e| is positive. We need 1 / (smallest positive x^|e|). So use self.pow_down(exp.abs()).
                // If abs_exp is odd, x^|e| is negative. We need 1 / (largest negative x^|e| -- closest to zero). So use self.pow_up(exp.abs()).
                let res_denom = if exp.abs() % 2 == 0 { self.pow_down(exp.abs()) } else { self.pow_up(exp.abs()) };
                return FloatBound(1.0).div_up(&res_denom);
            }
        }
        
        // Positive exponent
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || val.powi(exp)))
    }

    fn pow_down(&self, exp: i32) -> Self {
        let val = self.0;
        if val.is_nan() { return FloatBound(f64::NAN); }

        if exp == 0 { return FloatBound::one(); }

        if val == 0.0 {
            return if exp > 0 { FloatBound::zero() } else { FloatBound(f64::INFINITY) }; // Or Positive if convention allows 0^-even_neg_power
                                                                                             // OCaml example implies +Inf. Let's stick to standard float pow for 0^-exp.
                                                                                             // f64::powi(0.0, neg) is Inf.
        }
        if val == 1.0 { return FloatBound::one(); }
        if val == -1.0 {
            return if exp % 2 == 0 { FloatBound::one() } else { FloatBound::minus_one() };
        }

        if val.is_infinite() {
            if exp < 0 { return FloatBound::zero(); }
            return if val == f64::INFINITY || exp % 2 == 0 {
                FloatBound(f64::INFINITY)
            } else {
                FloatBound(f64::NEG_INFINITY)
            };
        }

        // Finite base
        if exp < 0 {
            // x^-e = 1 / x^e. To minimize 1/denom (could be large positive or small negative).
            // If x is positive:
            if val > 0.0 {
                 // (1/x)^|exp|. To minimize this, minimize 1/x (so div_down).
                 // Then raise (1/x)_down to power |exp| with downward rounding.
                let one_div_x_down = FloatBound(1.0).div_down(self);
                return one_div_x_down.pow_down(exp.abs());
            } else { // val < 0.0
                // Denom is self.pow_up(abs_exp) or self.pow_down(abs_exp)
                // If abs_exp is even, x^|e| is positive. We need 1 / (largest positive x^|e|). So use self.pow_up(exp.abs()).
                // If abs_exp is odd, x^|e| is negative. We need 1 / (smallest negative x^|e| -- most negative). So use self.pow_down(exp.abs()).
                let res_denom = if exp.abs() % 2 == 0 { self.pow_up(exp.abs()) } else { self.pow_down(exp.abs()) };
                return FloatBound(1.0).div_down(&res_denom);
            }
        }
        
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || val.powi(exp)))
    }
    
    fn root_up(&self, n: i32) -> Self { // nth root
        let val = self.0;
        if val.is_nan() { return FloatBound(f64::NAN); }
        if n == 0 { return FloatBound(f64::NAN); } // Root of 0 is undefined
        if n == 1 { return self.clone(); }
        if n < 0 { // x^(1/-m) = 1 / (x^(1/m))
            let abs_n = n.abs();
            let root_val_down = self.root_down(abs_n); // To maximize 1/root_val
            let root_val_up = self.root_up(abs_n);     // To minimize 1/root_val
            // This logic needs to be similar to pow with negative exponent
            if root_val_down.0 == 0.0 && root_val_up.0 == 0.0 { // Avoid division by zero if root is 0
                 return FloatBound(f64::NAN); // Or Inf depending on sign of self and n
            }
            // If root_val is positive, use root_val_down for 1/val to be max.
            // If root_val is negative, use root_val_up for 1/val to be max.
            let chosen_root_for_inv = if root_val_down.0 * root_val_up.0 <= 0.0 || root_val_down.0.abs() < root_val_up.0.abs() {
                 // signs differ or down is closer to zero
                 if (root_val_down.0 > 0.0 && root_val_up.0 > 0.0) || (root_val_down.0 < 0.0 && root_val_up.0 < 0.0) { // same sign
                    if root_val_down.0.abs() < root_val_up.0.abs() { root_val_down } else { root_val_up }
                 } else { // signs differ, this case is complex for general root.
                    // For now, if one is positive and one is negative, the interval is problematic.
                    // Let's assume we want the one that gives the 'largest' real result after inversion.
                    // This means smallest positive denominator or largest negative denominator.
                    // This case is very tricky. Defaulting to a simpler path for now.
                    if root_val_down.0 > 0.0 { root_val_down } else { root_val_up } // pick positive if available
                 }
            } else { root_val_down }; // Default pick
            return FloatBound(1.0).div_up(&chosen_root_for_inv);
        }

        if val < 0.0 && n % 2 == 0 { return FloatBound(f64::NAN); } // Even root of negative number

        if val == 0.0 { return FloatBound::zero(); }
        if val == 1.0 { return FloatBound::one(); }
        if val == -1.0 && n % 2 != 0 { return FloatBound::minus_one(); }
        
        if val == f64::INFINITY { return FloatBound(f64::INFINITY); }
        if val == f64::NEG_INFINITY { // n must be odd
            return FloatBound(f64::NEG_INFINITY);
        }

        // General case: x^(1/n)
        let res = fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || {
            if val < 0.0 { // Negative base, odd root
                -(-val).powf(1.0 / n as f64)
            } else {
                val.powf(1.0 / n as f64)
            }
        });
        FloatBound(res)
    }

    fn root_down(&self, n: i32) -> Self {
        let val = self.0;
        if val.is_nan() { return FloatBound(f64::NAN); }
        if n == 0 { return FloatBound(f64::NAN); }
        if n == 1 { return self.clone(); }
        if n < 0 {
            let abs_n = n.abs();
            let root_val_down = self.root_down(abs_n);
            let root_val_up = self.root_up(abs_n);
            // Similar complex choice as in root_up for 1/value
            let chosen_root_for_inv = if root_val_down.0 * root_val_up.0 <= 0.0 || root_val_up.0.abs() < root_val_down.0.abs() {
                 if (root_val_down.0 > 0.0 && root_val_up.0 > 0.0) || (root_val_down.0 < 0.0 && root_val_up.0 < 0.0) { // same sign
                    if root_val_up.0.abs() < root_val_down.0.abs() { root_val_up } else { root_val_down }
                 } else { // signs differ
                    if root_val_up.0 < 0.0 { root_val_up } else { root_val_down } // pick negative if available
                 }
            } else { root_val_up };
            return FloatBound(1.0).div_down(&chosen_root_for_inv);
        }


        if val < 0.0 && n % 2 == 0 { return FloatBound(f64::NAN); }

        if val == 0.0 { return FloatBound::zero(); }
        if val == 1.0 { return FloatBound::one(); }
        if val == -1.0 && n % 2 != 0 { return FloatBound::minus_one(); }

        if val == f64::INFINITY { return FloatBound(f64::INFINITY); }
        if val == f64::NEG_INFINITY { // n must be odd
            return FloatBound(f64::NEG_INFINITY);
        }

        let res = fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || {
            if val < 0.0 { // Negative base, odd root
                -(-val).powf(1.0 / n as f64)
            } else {
                val.powf(1.0 / n as f64)
            }
        });
        FloatBound(res)
    }

    // --- Trigonometric functions ---

    fn cos_up(&self) -> Self {
        let val = self.0;
        if val.is_nan() || val.is_infinite() { // cos(inf) is undefined, effectively [-1,1] for intervals.
                                            // For a single bound, _up should be 1.0.
            return FloatBound(1.0); // Conservative upper bound for cos(inf) or cos(nan)
        }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || val.cos()))
    }
    fn cos_down(&self) -> Self {
        let val = self.0;
        if val.is_nan() || val.is_infinite() { // cos(inf) is undefined.
            return FloatBound(-1.0); // Conservative lower bound for cos(inf) or cos(nan)
        }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || val.cos()))
    }

    fn sin_up(&self) -> Self {
        let val = self.0;
        if val.is_nan() || val.is_infinite() { // sin(inf) is undefined.
            return FloatBound(1.0); // Conservative upper bound
        }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || val.sin()))
    }
    fn sin_down(&self) -> Self {
        let val = self.0;
        if val.is_nan() || val.is_infinite() { // sin(inf) is undefined.
            return FloatBound(-1.0); // Conservative lower bound
        }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || val.sin()))
    }

    fn tan_up(&self) -> Self {
        let val = self.0;
        if val.is_nan() || val.is_infinite() {
            // tan(inf) is undefined. The range of tan is (-inf, +inf).
            return FloatBound(f64::INFINITY); // Conservative upper bound
        }
        // tan has poles (e.g. pi/2). If val is a pole, tan(val) is Inf or -Inf.
        // The rounding mode will handle this if f64::tan correctly returns Inf/-Inf at poles.
        // However, direct floating point evaluation at exact poles is unlikely.
        // This might need specific pole checks if very high accuracy near poles is needed.
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || val.tan()))
    }
    fn tan_down(&self) -> Self {
        let val = self.0;
        if val.is_nan() || val.is_infinite() {
            return FloatBound(f64::NEG_INFINITY); // Conservative lower bound
        }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || val.tan()))
    }

    // Inverse trigonometric functions: Domain is typically [-1, 1] for acos, asin.
    fn acos_up(&self) -> Self {
        let val = self.0;
        if val.is_nan() || val < -1.0 || val > 1.0 {
            // acos domain is [-1, 1]. Outside this, it's NaN.
            // The range of acos is [0, PI].
            // If input is invalid, what should _up be? OCaml might raise error or return specific bound.
            // For an _up operation, if invalid, perhaps PI (max of range) or NaN. Let's use NaN.
            return FloatBound(f64::NAN); 
        }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || val.acos()))
    }
    fn acos_down(&self) -> Self {
        let val = self.0;
        if val.is_nan() || val < -1.0 || val > 1.0 {
            // Min of range is 0. Or NaN for invalid.
            return FloatBound(f64::NAN);
        }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || val.acos()))
    }

    fn asin_up(&self) -> Self {
        let val = self.0;
        if val.is_nan() || val < -1.0 || val > 1.0 {
            // asin domain is [-1, 1]. Range is [-PI/2, PI/2].
            return FloatBound(f64::NAN);
        }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || val.asin()))
    }
    fn asin_down(&self) -> Self {
        let val = self.0;
        if val.is_nan() || val < -1.0 || val > 1.0 {
            return FloatBound(f64::NAN);
        }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || val.asin()))
    }

    fn atan_up(&self) -> Self { // Domain is all reals. Range is (-PI/2, PI/2).
        let val = self.0;
        if val.is_nan() { return FloatBound(f64::NAN); }
        if val == f64::INFINITY { return FloatBound(std::f64::consts::FRAC_PI_2); } // atan(inf) = PI/2
        // Corrected atan_up for NEG_INFINITY
        if val == f64::NEG_INFINITY { return FloatBound(-std::f64::consts::FRAC_PI_2); }


        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || val.atan()))
    }
    fn atan_down(&self) -> Self {
        let val = self.0;
        if val.is_nan() { return FloatBound(f64::NAN); }
        // Corrected atan_down for INFINITY
        if val == f64::INFINITY { return FloatBound(std::f64::consts::FRAC_PI_2); }
        if val == f64::NEG_INFINITY { return FloatBound(-std::f64::consts::FRAC_PI_2); } // atan(-inf) = -PI/2
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || val.atan()))
    }
    
    // --- Exponential and Logarithmic functions ---

    fn exp_up(&self) -> Self { // e^x
        let val = self.0;
        if val.is_nan() { return FloatBound(f64::NAN); }
        if val == f64::INFINITY { return FloatBound(f64::INFINITY); } // exp(inf) = inf
        if val == f64::NEG_INFINITY { return FloatBound(0.0); }      // exp(-inf) = 0
        // exp(very_large_number) can overflow to INFINITY.
        // exp(very_small_negative_number) can underflow to 0.0.
        // FPU rounding should handle these cases correctly for finite inputs.
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || val.exp()))
    }
    fn exp_down(&self) -> Self {
        let val = self.0;
        if val.is_nan() { return FloatBound(f64::NAN); }
        if val == f64::INFINITY { return FloatBound(f64::INFINITY); }
        if val == f64::NEG_INFINITY { return FloatBound(0.0); }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || val.exp()))
    }

    fn ln_up(&self) -> Self { // Natural logarithm
        let val = self.0;
        if val.is_nan() || val < 0.0 { return FloatBound(f64::NAN); } // ln undefined for x < 0
        if val == 0.0 { return FloatBound(f64::NEG_INFINITY); }    // ln(0) = -inf
        if val == f64::INFINITY { return FloatBound(f64::INFINITY); }  // ln(inf) = inf
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Upward, || val.ln()))
    }
    fn ln_down(&self) -> Self {
        let val = self.0;
        if val.is_nan() || val < 0.0 { return FloatBound(f64::NAN); }
        if val == 0.0 { return FloatBound(f64::NEG_INFINITY); }
        if val == f64::INFINITY { return FloatBound(f64::INFINITY); }
        FloatBound(fpu_control::with_rounding_mode(fpu_control::RoundingMode::Downward, || val.ln()))
    }

    // OCaml's `log` is often natural log. Assuming natural log here as well.
    // If it were log10 or log2, the implementation would need to change (val.log10(), val.log2()).
    fn log_up(&self) -> Self { // Assuming natural log
        self.ln_up()
    }
    fn log_down(&self) -> Self { // Assuming natural log
        self.ln_down()
    }
    
    // --- Integer rounding for bound type ---
    fn floor_val(&self) -> Self { 
        if self.0.is_nan() { FloatBound(f64::NAN) }
        else if self.0.is_infinite() { self.clone() } 
        else { FloatBound(self.0.floor()) }
    }
    fn ceil_val(&self) -> Self {  
        if self.0.is_nan() { FloatBound(f64::NAN) }
        else if self.0.is_infinite() { self.clone() }
        else { FloatBound(self.0.ceil()) }
    }
}

// Add basic tests for what's implemented (Display, classify, constants, basic ordering)
#[cfg(test)]
mod tests {
    use super::*;
    use crate::arith_bound::BoundKind; // For BoundKind::Finite etc.
    use rug::Rational; // For test_from_string_conversion

    // Helper for checking rounding for an operation
    // op_exact: the f64 operation like |a,b| a+b
    // op_down: the bound_op like |a,b| a.add_down(b)
    // op_up: the bound_op like |a,b| a.add_up(b)
    fn check_op_rounding(a: f64, b: f64, 
                        op_exact_str: &str,
                        op_exact: impl Fn(f64, f64) -> f64,
                        op_down: impl Fn(&FloatBound, &FloatBound) -> FloatBound,
                        op_up: impl Fn(&FloatBound, &FloatBound) -> FloatBound
    ) {
        let fa = FloatBound(a);
        let fb = FloatBound(b);
        let res_exact = op_exact(a, b);
        let res_down = op_down(&fa, &fb).0;
        let res_up = op_up(&fa, &fb).0;

        if res_exact.is_finite() { // Only check relationships if exact result is finite
            assert!(res_down <= res_exact, 
                "{} down: {} ({:?}) should be <= exact {} ({:?}) for inputs {}, {}", 
                op_exact_str, res_down, FloatBound(res_down).classify(), res_exact, FloatBound(res_exact).classify(), a, b); // Used classify for debug
            assert!(res_up >= res_exact, 
                "{} up: {} ({:?}) should be >= exact {} ({:?}) for inputs {}, {}", 
                op_exact_str, res_up, FloatBound(res_up).classify(), res_exact, FloatBound(res_exact).classify(), a, b); // Used classify for debug
        }
        // It should always hold that down <= up
        assert!(res_down <= res_up || (res_down.is_nan() && res_up.is_nan()) , 
            "{} down {} should be <= up {} for inputs {}, {}", op_exact_str, res_down, res_up, a, b);
        
        // eprintln!("Test: {} ({}, {}) -> exact: {}, down: {}, up: {}", op_exact_str, a, b, res_exact, res_down, res_up);
    }

    #[test]
    fn test_float_bound_display() {
        assert_eq!(FloatBound(12.3).to_string(), "12.3");
        assert_eq!(FloatBound(f64::INFINITY).to_string(), "+oo");
        assert_eq!(FloatBound(f64::NEG_INFINITY).to_string(), "-oo");
        assert_eq!(FloatBound(f64::NAN).to_string(), "NaN");
    }

    #[test]
    fn test_float_bound_classify_and_constants() {
        assert_eq!(FloatBound::zero().classify(), BoundKind::Finite);
        assert_eq!(FloatBound::one().classify(), BoundKind::Finite);
        assert_eq!(FloatBound::positive_infinity().classify(), BoundKind::PositiveInfinity);
        assert_eq!(FloatBound::negative_infinity().classify(), BoundKind::NegativeInfinity);
        assert_eq!(FloatBound(f64::NAN).classify(), BoundKind::Invalid);
    }

    #[test]
    fn test_float_bound_total_order() {
        assert!(FloatBound::negative_infinity() < FloatBound::zero());
        assert!(FloatBound::zero() < FloatBound::positive_infinity());
        assert!(FloatBound::positive_infinity() < FloatBound(f64::NAN)); // NaN is largest
        assert_eq!(FloatBound(f64::NAN).compare(&FloatBound(f64::NAN)), Ordering::Equal);
        assert_eq!(FloatBound(5.0).compare(&FloatBound(5.0)), Ordering::Equal);
        assert!(FloatBound(5.0).eq(&FloatBound(5.0)));
        assert!(FloatBound(f64::NAN).eq(&FloatBound(f64::NAN))); // Due to our PartialEq impl
    }

    #[test]
    fn test_float_bound_sign() {
        assert_eq!(FloatBound(10.0).sign(), 1);
        assert_eq!(FloatBound(-10.0).sign(), -1);
        assert_eq!(FloatBound(0.0).sign(), 0);
        assert_eq!(FloatBound(-0.0).sign(), 0); // Sign of 0.0 and -0.0 is 0
        assert_eq!(FloatBound::positive_infinity().sign(), 1);
        assert_eq!(FloatBound::negative_infinity().sign(), -1);
        assert_eq!(FloatBound(f64::NAN).sign(), 0); // OCaml Float.sign of nan is 0
    }
    
    #[test]
    fn test_is_odd_even_finite_integers() {
        assert!(FloatBound(3.0).is_odd());
        assert!(!FloatBound(3.0).is_even());
        assert!(FloatBound(4.0).is_even());
        assert!(!FloatBound(4.0).is_odd());
        assert!(!FloatBound(3.5).is_odd());
        assert!(!FloatBound(3.5).is_even());
        assert!(!FloatBound::positive_infinity().is_odd());
        assert!(!FloatBound(f64::NAN).is_even());
    }

    #[test]
    fn test_from_string_conversion() {
        // Upward rounding
        assert_eq!(FloatBound::from_string_up("123.456"), Ok(FloatBound::from_rational_up(&Rational::from_str_radix("123.456", 10).unwrap())));
        assert_eq!(FloatBound::from_string_up("inf"), Ok(FloatBound::positive_infinity()));
        assert_eq!(FloatBound::from_string_up("-inf"), Ok(FloatBound::negative_infinity()));
        assert_eq!(FloatBound::from_string_up("NaN").unwrap().0.is_nan(), true); // Comparing NaNs

        // Downward rounding
        assert_eq!(FloatBound::from_string_down("123.456"), Ok(FloatBound::from_rational_down(&Rational::from_str_radix("123.456", 10).unwrap())));
        assert_eq!(FloatBound::from_string_down("inf"), Ok(FloatBound::positive_infinity()));
        assert_eq!(FloatBound::from_string_down("-inf"), Ok(FloatBound::negative_infinity()));
        assert_eq!(FloatBound::from_string_down("NaN").unwrap().0.is_nan(), true);

        // Test a value that might show rounding differences (though exact check is tricky here)
        // "0.1" should be exact for both Rational and f64. from_rational_up/down should handle it.
        let r_one_tenth = Rational::from((1, 10));
        assert_eq!(FloatBound::from_string_up("0.1"), Ok(FloatBound::from_rational_up(&r_one_tenth)));
        assert_eq!(FloatBound::from_string_down("0.1"), Ok(FloatBound::from_rational_down(&r_one_tenth)));

        // Test parsing failure
        assert!(FloatBound::from_string_up("invalid_number").is_err());
        assert!(FloatBound::from_string_down("1.2.3").is_err());
    }

    #[test]
    fn test_arithmetic_rounding() {
        // Test add
        check_op_rounding(0.1, 0.2, "0.1+0.2", |a,b| a+b, |a,b| a.add_down(b), |a,b| a.add_up(b));
        check_op_rounding(1.0, 1e-17, "1.0+1e-17", |a,b| a+b, |a,b| a.add_down(b), |a,b| a.add_up(b)); // x + small_val
        check_op_rounding(1.0/3.0, 2.0/3.0, "1/3+2/3", |a,b| a+b, |a,b| a.add_down(b), |a,b| a.add_up(b)); // Should be 1.0

        // Test sub
        check_op_rounding(0.3, 0.1, "0.3-0.1", |a,b| a-b, |a,b| a.sub_down(b), |a,b| a.sub_up(b)); // exact 0.2
        check_op_rounding(1.0, 1.0/3.0, "1-1/3", |a,b| a-b, |a,b| a.sub_down(b), |a,b| a.sub_up(b));

        // Test mul
        check_op_rounding(0.1, 0.2, "0.1*0.2", |a,b| a*b, |a,b| a.mul_down(b), |a,b| a.mul_up(b)); // 0.02
        check_op_rounding(1.0/3.0, 3.0, "1/3*3", |a,b| a*b, |a,b| a.mul_down(b), |a,b| a.mul_up(b)); // Should be 1.0
        check_op_rounding(1.0/7.0, 1.0/3.0, "1/7*1/3", |a,b| a*b, |a,b| a.mul_down(b), |a,b| a.mul_up(b));

        // Test div
        check_op_rounding(0.3, 0.1, "0.3/0.1", |a,b| a/b, |a,b| a.div_down(b), |a,b| a.div_up(b)); // exact 3.0
        check_op_rounding(1.0, 3.0, "1/3", |a,b| a/b, |a,b| a.div_down(b), |a,b| a.div_up(b));
        check_op_rounding(2.0, 1e-17, "2/small", |a,b| a/b, |a,b| a.div_down(b), |a,b| a.div_up(b));
    }

    #[test]
    fn test_add_sub_inf_nan() {
        let p_inf = FloatBound::positive_infinity();
        let n_inf = FloatBound::negative_infinity();
        let nan_b = FloatBound(f64::NAN);
        let five = FloatBound(5.0);

        // Add
        assert_eq!(p_inf.add_up(&five).0, f64::INFINITY);
        assert_eq!(p_inf.add_down(&five).0, f64::INFINITY);
        assert_eq!(n_inf.add_up(&five).0, f64::NEG_INFINITY);
        assert_eq!(p_inf.add_up(&n_inf).0.is_nan(), true); // Inf + (-Inf) = NaN
        assert_eq!(p_inf.add_up(&nan_b).0.is_nan(), true);

        // Sub
        assert_eq!(p_inf.sub_up(&five).0, f64::INFINITY);
        assert_eq!(n_inf.sub_up(&five).0, f64::NEG_INFINITY);
        assert_eq!(five.sub_up(&p_inf).0, f64::NEG_INFINITY); // 5 - Inf = -Inf
        assert_eq!(p_inf.sub_up(&p_inf).0.is_nan(), true); // Inf - Inf = NaN
    }

    #[test]
    fn test_mul_div_inf_nan() {
        let p_inf = FloatBound::positive_infinity();
        let n_inf = FloatBound::negative_infinity();
        let nan_b = FloatBound(f64::NAN);
        let five = FloatBound(5.0);
        let neg_five = FloatBound(-5.0);
        let zero = FloatBound::zero();

        // Mul
        assert_eq!(p_inf.mul_up(&five).0, f64::INFINITY);
        assert_eq!(p_inf.mul_up(&neg_five).0, f64::NEG_INFINITY);
        assert_eq!(n_inf.mul_up(&neg_five).0, f64::INFINITY);
        assert_eq!(p_inf.mul_up(&zero).0.is_nan(), true); // Inf * 0 = NaN
        assert_eq!(n_inf.mul_up(&zero).0.is_nan(), true);
        assert_eq!(p_inf.mul_up(&p_inf).0, f64::INFINITY);
        assert_eq!(p_inf.mul_up(&n_inf).0, f64::NEG_INFINITY);

        // Div
        assert_eq!(five.div_up(&zero).0, f64::INFINITY); // 5 / +0 = +Inf (div_up)
        assert_eq!(five.div_down(&zero).0, f64::INFINITY); // OCaml bound.ml has div_up x zero = if x>0 then inf else if x<0 then nan else nan
                                                          // This is different from IEEE. For now, use IEEE via f64 /
                                                          // IEEE: 5/0=inf. -5/0=-inf. 0/0=nan.
        assert_eq!(neg_five.div_up(&zero).0, f64::NEG_INFINITY);
        assert_eq!(zero.div_up(&zero).0.is_nan(), true);
        
        assert_eq!(p_inf.div_up(&five).0, f64::INFINITY);
        assert_eq!(five.div_up(&p_inf).0, 0.0); // 5 / Inf = 0
        assert_eq!(p_inf.div_up(&p_inf).0.is_nan(), true); // Inf / Inf = NaN
    }
    
    #[test]
    fn test_sqrt_rounding_and_special() {
        let nan_b = FloatBound(f64::NAN);
        let neg_one = FloatBound(-1.0);
        let zero = FloatBound::zero();
        let two = FloatBound(2.0);
        let four = FloatBound(4.0);
        let p_inf = FloatBound::positive_infinity();

        assert!(neg_one.sqrt_up().0.is_nan());
        assert!(neg_one.sqrt_down().0.is_nan());
        assert!(nan_b.sqrt_up().0.is_nan());
        
        assert_eq!(zero.sqrt_up().0, 0.0);
        assert_eq!(four.sqrt_up().0, 2.0); // Exact
        assert_eq!(p_inf.sqrt_up().0, f64::INFINITY);

        // Test rounding for sqrt(2)
        let sqrt2_exact = (2.0_f64).sqrt(); // approx 1.4142135623730951
        let sqrt2_down = two.sqrt_down().0;
        let sqrt2_up = two.sqrt_up().0;
        assert!(sqrt2_down <= sqrt2_exact);
        assert!(sqrt2_up >= sqrt2_exact);
        assert!(sqrt2_down <= sqrt2_up);
        // Check they are different if sqrt2_exact is not perfectly representable
        if sqrt2_down != sqrt2_exact || sqrt2_up != sqrt2_exact {
             assert_ne!(sqrt2_down, sqrt2_up, "sqrt_down and sqrt_up should differ for non-exact sqrt(2)");
        }
    }
    
    // Add similar detailed tests for pow, root, trig, exp, ln...
    // For example:
    #[test]
    fn test_pow_special_cases() {
        // ... (0^0, Inf^0, 0^Inf, etc.) ...
        assert_eq!(FloatBound(2.0).pow_up(3).0, 8.0); // 2^3
        assert_eq!(FloatBound(2.0).pow_down(3).0, 8.0);
        // Add tests for negative exponents and rounding
    }

    #[test]
    fn test_trig_special_cases() {
        assert_eq!(FloatBound(0.0).cos_up().0, 1.0);
        assert_eq!(FloatBound(std::f64::consts::PI).cos_down().0, -1.0); 
        // Check rounding for values like cos(0.1) up/down
        let val = FloatBound(0.1);
        let cos_val = (0.1_f64).cos();
        assert!(val.cos_down().0 <= cos_val);
        assert!(val.cos_up().0 >= cos_val);
    }

    #[test]
    fn test_exp_ln_special_cases() {
        assert_eq!(FloatBound(1.0).exp_up().0, std::f64::consts::E); // May not be exact, check bounds
        assert_eq!(FloatBound(0.0).exp_up().0, 1.0);
        assert_eq!(FloatBound::negative_infinity().exp_up().0, 0.0);

        assert_eq!(FloatBound(1.0).ln_up().0, 0.0);
        assert_eq!(FloatBound(std::f64::consts::E).ln_down().0.is_normal(), true); // ln(e) = 1
                                                                            // Check it's close to 1
        assert!((FloatBound(std::f64::consts::E).ln_down().0 - 1.0).abs() < 1e-9);
        assert!(FloatBound(0.0).ln_up().0.is_infinite() && FloatBound(0.0).ln_up().0 < 0.0); // ln(0) = -Inf
    }
}
