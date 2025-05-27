use rug::{Rational, Integer, Float};
use once_cell::sync::Lazy;
use std::cmp::Ordering;
use crate::arith_f; // Added for direct access to arith_f functions
use crate::arith_ring::RingOps; // Added for RingOps trait
use std::fmt; // For Display, Debug traits (though rug::Rational implements them)

// Type alias for rational numbers
pub type Q = Rational;

// --- Constructors as static values ---
pub static ZERO: Lazy<Q> = Lazy::new(|| Q::from(0));
pub static ONE: Lazy<Q> = Lazy::new(|| Q::from(1));
pub static TWO: Lazy<Q> = Lazy::new(|| Q::from(2));
pub static MINUS_ONE: Lazy<Q> = Lazy::new(|| Q::from(-1));

// --- Operators ---
pub fn add(a: &Q, b: &Q) -> Q {
    a + b
}

pub fn sub(a: &Q, b: &Q) -> Q {
    a - b
}

pub fn mul(a: &Q, b: &Q) -> Q {
    a * b
}

pub fn div_op(a: &Q, b: &Q) -> Option<Q> {
    if b.is_zero() {
        None
    } else {
        Some(a / b)
    }
}

pub fn pow_op(base: &Q, exp: i32) -> Q {
    if exp == 0 {
        return Q::from(1);
    }
    let abs_exp = exp.unsigned_abs();
    let mut result = base.clone();
    // rug::Rational::pow takes u32. Direct power for positive exponents.
    // For more general integer powers, a loop or explicit power method is needed if not directly supported.
    // rug::Rational doesn't have a direct pow method for signed integers.
    // We can use `rug::ops::Pow` trait or simple loop for small powers.
    // For simplicity, we'll implement the OCaml's recursive logic.
    
    fn recursive_pow(b: &Q, e: u32) -> Q {
        if e == 0 { return Q::from(1); }
        if e == 1 { return b.clone(); }
        let half_pow = recursive_pow(b, e / 2);
        let mut res_val = &half_pow * &half_pow;
        if e % 2 != 0 {
            res_val *= b;
        }
        res_val
    }

    let val = recursive_pow(base, abs_exp);

    if exp < 0 {
        if val.is_zero() {
            // This would mean base was 0, and 0^-N is problematic.
            // OCaml's Mpqf.div by zero would error. Here we return a representation if possible
            // or panic, depending on desired behavior for 1/0.
            // For now, let's assume OCaml's Mpqf.div would handle this (e.g. by exception)
            // and we mirror it by potentially creating an "infinite" or erroring.
            // However, Mpqf.div by zero is caught by our div_op.
            // So, 1 / (pow q (-n)) would become 1 / 0.
            // Let's return 0 for now if 1/0 is encountered, or handle error appropriately.
            // This case needs careful consideration based on how AbSolute expects it.
            // OCaml would raise Division_by_zero.
            // For now, to avoid panic, if val is zero (meaning base was zero), and exp is negative,
            // this is division by zero. The original `pow` in OCaml uses Mpqf.div one (pow q (-n)).
            // If `pow q (-n)` is zero, then it's `Mpqf.div one zero`, which would error.
            // So, this path should ideally return an error or panic.
            // For this translation, we'll panic to match the likely OCaml behavior.
             panic!("Division by zero in pow_op: base was zero and exponent was negative.");
        }
        Q::from(1) / val
    } else {
        val
    }
}


pub fn neg_op(q: &Q) -> Q {
    -q
}

pub fn abs_op(q: &Q) -> Q {
    q.abs()
}

pub fn ceil_op(q: &Q) -> i64 {
    // rug::Rational::to_f64 can lose precision for very large rationals.
    // A more robust ceil would use integer and fractional parts.
    // q.fract() gives num/den. q.trunc() gives integer part.
    // If fract is positive, result is trunc + 1 (unless fract is zero).
    // If fract is negative, result is trunc.
    // If fract is zero, result is trunc.
    let (num, den) = q.as_numer_denom();
    let (quotient, remainder) = num.div_rem_ref(den).into(); // into() converts (Integer, Integer)

    if remainder.is_zero() {
        quotient.to_i64().unwrap_or_else(|| if quotient > 0 { i64::MAX } else { i64::MIN })
    } else if q.is_positive() {
         (quotient + 1).to_i64().unwrap_or_else(|| i64::MAX)
    } else { // q is negative and remainder is not zero
        quotient.to_i64().unwrap_or_else(|| i64::MIN) // For negative numbers, quotient of div_rem_ref is already the floor
    }
}

pub fn floor_op(q: &Q) -> i64 {
    // Similar to ceil, but adjusted logic for floor
    let (num, den) = q.as_numer_denom();
    let (quotient, remainder) = num.div_rem_ref(den).into();

    if remainder.is_zero() {
        quotient.to_i64().unwrap_or_else(|| if quotient > 0 { i64::MAX } else { i64::MIN })
    } else if q.is_negative() {
        (quotient - 1).to_i64().unwrap_or_else(|| i64::MIN)
    } else { // q is positive and remainder is not zero
        quotient.to_i64().unwrap_or_else(|| i64::MAX)
    }
}


// --- Conversions ---
pub fn to_i64_exact(q: &Q) -> Option<i64> {
    if q.is_integer() {
        q.to_integer_ref().and_then(|i| i.to_i64())
    } else {
        None
    }
}

pub fn to_f64(q: &Q) -> f64 {
    q.to_f64()
}

pub fn clone_q(q: &Q) -> Q { // Corresponds to OCaml's to_rational = Fun.id
    q.clone()
}

pub fn from_i64(i: i64) -> Q {
    Q::from(i)
}

pub fn from_f64(f: f64) -> Option<Q> { // Mpqf.of_float can fail (e.g. NaN, Inf)
    Q::from_f64(f)
}

// Corresponds to OCaml's of_rational = Fun.id
// Assuming the input 'q_other' might be from a different but compatible rational type if such existed.
// In our case, it's just Q -> Q, so clone or direct use.
pub fn clone_q_from(q_other: &Q) -> Q {
    q_other.clone()
}

// --- Comparisons ---
pub fn equals(a: &Q, b: &Q) -> bool {
    a == b
}

pub fn compare_q(a: &Q, b: &Q) -> Ordering {
    a.cmp(b)
}

// --- Printing ---

// Helper for pp_print_size_of_0_suffix
fn bump_0(s: &str, n: usize) -> String {
    let ls = s.len();
    if ls > n {
        let integer_part = &s[0..(ls - n)];
        let decimal_part = &s[(ls - n)..];
        format!("{}.{}", integer_part, decimal_part)
    } else {
        let zeros = "0".repeat(n - ls);
        format!("0.{}{}", zeros, s)
    }
}

// OCaml: pp_print_size_of_0_suffix
fn format_rational_decimal_suffix(q_val: &Q) -> String {
    if q_val.is_zero() { return "0".to_string(); }

    let (num, den) = q_val.as_numer_denom();
    let num_abs_str = num.abs().to_string(); // Work with absolute value for string manipulation

    if den.is_one() {
        return if q_val.is_negative() { format!("-{}", num_abs_str) } else { num_abs_str };
    }

    let den_str = den.to_string(); // Denominator is always positive in rug::Rational

    if den_str.starts_with('1') && den_str[1..].chars().all(|c| c == '0') {
        let num_zeros = den_str.len() - 1;
        if num_zeros > 0 {
            let formatted_num = bump_0(&num_abs_str, num_zeros);
            return if q_val.is_negative() { format!("-{}", formatted_num) } else { formatted_num };
        }
    }
    // Fallback to standard string representation
    q_val.to_string()
}

// OCaml: pp_print fmt (m : t)
pub fn pp_print_q(q_val: &Q) -> String {
    let f_val = q_val.to_f64(); // Convert to f64
    // Check if converting back to Rational from this f64 yields the same rational value
    if let Some(r_back) = Q::from_f64(f_val) {
        if r_back == *q_val {
            return arith_f::pp_print_f(f_val); // Use the actual arith_f print
        }
    }
    format_rational_decimal_suffix(q_val)
}

// OCaml: let print = pp_print (now pp_print_q)
// The direct OCaml `print` takes a formatter. This version returns String.

// OCaml: let to_string : t -> string = Format.asprintf "%a" pp_print
pub fn to_string_q(q_val: &Q) -> String {
    pp_print_q(q_val)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rug::Integer;

    #[test]
    fn test_constructors() {
        assert_eq!(*ZERO, Q::from(0));
        assert_eq!(*ONE, Q::from(1));
        assert_eq!(*TWO, Q::from(2));
        assert_eq!(*MINUS_ONE, Q::from(-1));
    }

    #[test]
    fn test_arith_ops() {
        let q1 = Q::from((2, 3)); // 2/3
        let q2 = Q::from((1, 2)); // 1/2
        assert_eq!(add(&q1, &q2), Q::from((7, 6)));
        assert_eq!(sub(&q1, &q2), Q::from((1, 6)));
        assert_eq!(mul(&q1, &q2), Q::from((1, 3)));
        assert_eq!(div_op(&q1, &q2), Some(Q::from((4, 3))));
        assert_eq!(div_op(&q1, &ZERO), None);
    }

    #[test]
    fn test_pow_op() {
        let base = Q::from((2, 3));
        assert_eq!(pow_op(&base, 0), *ONE);
        assert_eq!(pow_op(&base, 1), base);
        assert_eq!(pow_op(&base, 2), Q::from((4, 9)));
        assert_eq!(pow_op(&base, 3), Q::from((8, 27)));
        assert_eq!(pow_op(&base, -1), Q::from((3, 2)));
        assert_eq!(pow_op(&base, -2), Q::from((9, 4)));
        
        let neg_base = Q::from((-2,3));
        assert_eq!(pow_op(&neg_base, 2), Q::from((4,9)));
        assert_eq!(pow_op(&neg_base, 3), Q::from((-8,27)));

        assert_eq!(pow_op(&ZERO, 2), *ZERO);
        assert_eq!(pow_op(&ONE, 10), *ONE);
    }

    #[test]
    #[should_panic(expected = "Division by zero in pow_op")]
    fn test_pow_op_zero_neg_exp() {
        pow_op(&ZERO, -2);
    }


    #[test]
    fn test_neg_abs() {
        let q1 = Q::from((2, 3));
        let q2 = Q::from((-2, 3));
        assert_eq!(neg_op(&q1), q2);
        assert_eq!(neg_op(&q2), q1);
        assert_eq!(abs_op(&q1), q1);
        assert_eq!(abs_op(&q2), q1);
    }

    #[test]
    fn test_ceil_floor() {
        assert_eq!(ceil_op(&Q::from((7, 2))), 4); // 3.5
        assert_eq!(floor_op(&Q::from((7, 2))), 3);
        assert_eq!(ceil_op(&Q::from((-7, 2))), -3); // -3.5
        assert_eq!(floor_op(&Q::from((-7, 2))), -4);
        assert_eq!(ceil_op(&Q::from(3)), 3);
        assert_eq!(floor_op(&Q::from(3)), 3);
    }

    #[test]
    fn test_conversions() {
        assert_eq!(to_i64_exact(&Q::from(5)), Some(5i64));
        assert_eq!(to_i64_exact(&Q::from((10, 2))), Some(5i64));
        assert_eq!(to_i64_exact(&Q::from((7, 2))), None);

        assert_eq!(to_f64(&Q::from((1, 2))), 0.5);
        
        assert_eq!(from_i64(5), Q::from(5));
        assert_eq!(from_f64(0.5), Some(Q::from((1,2))));
        assert_eq!(from_f64(f64::NAN), None);
        assert_eq!(from_f64(f64::INFINITY), None);

        let q = Q::from(3);
        assert_eq!(clone_q(&q), q);
        assert_eq!(clone_q_from(&q), q);
    }

    #[test]
    fn test_comparisons() {
        let q1 = Q::from((1, 2));
        let q2 = Q::from((2, 4));
        let q3 = Q::from((1, 3));
        assert!(equals(&q1, &q2));
        assert!(!equals(&q1, &q3));
        assert_eq!(compare_q(&q1, &q2), Ordering::Equal);
        assert_eq!(compare_q(&q1, &q3), Ordering::Greater);
        assert_eq!(compare_q(&q3, &q1), Ordering::Less);
    }

    #[test]
    fn test_format_rational_decimal_suffix() {
        assert_eq!(format_rational_decimal_suffix(&Q::from(0)), "0");
        assert_eq!(format_rational_decimal_suffix(&Q::from(123)), "123");
        assert_eq!(format_rational_decimal_suffix(&Q::from(-123)), "-123");
        assert_eq!(format_rational_decimal_suffix(&Q::from((123, 10))), "12.3");
        assert_eq!(format_rational_decimal_suffix(&Q::from((-123, 100))), "-1.23");
        assert_eq!(format_rational_decimal_suffix(&Q::from((1, 1000))), "0.001");
        assert_eq!(format_rational_decimal_suffix(&Q::from((12345, 100))), "123.45");
        assert_eq!(format_rational_decimal_suffix(&Q::from((5, 1))), "5");
        assert_eq!(format_rational_decimal_suffix(&Q::from((1, 3))), "1/3"); // Fallback
        assert_eq!(format_rational_decimal_suffix(&Q::from((123, 10000))), "0.0123");
        assert_eq!(format_rational_decimal_suffix(&Q::from((-1,10))), "-0.1");
    }

    #[test]
    fn test_pp_print_q() {
        // Exact float conversion
        assert_eq!(pp_print_q(&Q::from((1, 2))), "0.5"); // Assumes arith_f_pp_print_placeholder
        assert_eq!(pp_print_q(&Q::from(-0.25)), "-0.25");
        // Fallback to decimal suffix
        assert_eq!(pp_print_q(&Q::from((123, 10))), "12.3");
        // Fallback to fraction
        assert_eq!(pp_print_q(&Q::from((1, 3))), "1/3");
        // A rational that might not be exact as f64 but then becomes exact with more precision
        // For example, 1/10 is exact. 1/10000000000000000 is exact.
        // The check `Q::from_f64(f_val).map_or(false, |r_back| r_back == *q_val)` is key.
        let large_den = Q::new_from_str("1/1000000000000000000000000000000").unwrap();
        assert_eq!(pp_print_q(&large_den), "0.000000000000000000000000000001"); // Uses decimal suffix
    }

     #[test]
    fn test_to_string_q() {
        assert_eq!(to_string_q(&Q::from((1,2))), "0.5");
        assert_eq!(to_string_q(&Q::from((1,3))), "1/3");
    }
}

impl RingOps for Q { // Q is type alias for rug::Rational
    fn zero() -> Self {
        ZERO.clone() // Clone the static Lazy<Q> value
    }

    fn one() -> Self {
        ONE.clone() // Clone the static Lazy<Q> value
    }

    fn add(&self, other: &Self) -> Self {
        add(self, other) // Calls add(&Q, &Q) -> Q from this module
    }

    fn mul(&self, other: &Self) -> Self {
        mul(self, other) // Calls mul(&Q, &Q) -> Q from this module
    }

    fn div_exact(&self, other: &Self) -> Option<Self> {
        div_op(self, other) // Calls div_op(&Q, &Q) -> Option<Q>
    }

    fn neg(&self) -> Self {
        neg_op(self) // Calls neg_op(&Q) -> Q
    }

    fn to_i64_exact(&self) -> Option<i64> {
        to_i64_exact(self) // Calls to_i64_exact(&Q) -> Option<i64>
    }

    fn to_f64(&self) -> f64 {
        to_f64(self) // Calls to_f64(&Q) -> f64
    }

    fn to_rational(&self) -> Rational { // Rational is rug::Rational
        clone_q(self) // Calls clone_q(&Q) -> Q, effectively self.clone()
    }

    fn floor_as_i64(&self) -> i64 {
        floor_op(self) // Calls floor_op(&Q) -> i64
    }

    fn from_i64(val: i64) -> Self {
        from_i64(val) // Calls from_i64(i64) -> Q
    }

    fn from_f64(val: f64) -> Self {
        // OCaml's Mpqf.of_float can raise an exception (e.g. for NaN/Inf).
        // RingOps trait expects direct Self. Panicking mimics this.
        from_f64(val).unwrap_or_else(|| {
            panic!("Failed to convert non-finite f64 to Rational: {}", val)
        })
    }

    fn from_rational(r: &Rational) -> Self {
        clone_q_from(r) // Calls clone_q_from(&Q) -> Q, effectively r.clone()
    }

    fn equals(&self, other: &Self) -> bool {
        equals(self, other) // Calls equals(&Q, &Q) -> bool
    }

    fn compare_total(&self, other: &Self) -> Ordering {
        compare_q(self, other) // Calls compare_q(&Q, &Q) -> Ordering
    }
}

// rug::Rational satisfies Display, Debug, Clone, PartialEq.
// For Display, it uses its own standard format (e.g., "1/2", "123").
// The `pp_print_q` function provides the custom OCaml-like formatting.
// The RingOps trait only requires `Display`, which `rug::Rational` provides.
