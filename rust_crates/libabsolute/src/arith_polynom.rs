use std::fmt::{self, Display, Debug};
use std::cmp::Ordering;
use std::ops::{Add, Sub, Mul, Neg}; // For operator overloading eventually

use crate::arith_ring::RingOps;
use crate::arith_i::I; // i64
use crate::arith_f::F; // f64
use crate::arith_q::Q; // rug::Rational

// --- Data Structures ---

pub type Id = String;
pub type Exp = i32; // OCaml int for exponent

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Var {
    pub id: Id,
    pub exp: Exp,
}

// Implement Ord for Var to sort them by id, then by exp (though exp usually isn't needed for id-sorting)
impl PartialOrd for Var {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for Var {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id).then_with(|| self.exp.cmp(&other.exp))
    }
}

impl Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.exp == 1 {
            write!(f, "{}", self.id)
        } else {
            write!(f, "{}^{}", self.id, self.exp)
        }
    }
}

#[derive(Clone, Debug)]
pub struct Monomial<C: RingOps> {
    pub coeff: C,
    pub vars: Vec<Var>, // Kept sorted by Var.id
}

// Equality for Monomial (ignoring coefficient for var list comparison for sorting, but coeff matters for overall equality)
impl<C: RingOps> PartialEq for Monomial<C> {
    fn eq(&self, other: &Self) -> bool {
        self.coeff.equals(&other.coeff) && self.vars == other.vars
    }
}
impl<C: RingOps> Eq for Monomial<C> {}


// Order monomials based on their variable lists (lexicographically)
// This is crucial for keeping polynomials in a canonical form.
impl<C: RingOps> PartialOrd for Monomial<C> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl<C: RingOps> Ord for Monomial<C> {
    fn cmp(&self, other: &Self) -> Ordering {
        // Lexicographical comparison of sorted var lists
        self.vars.iter().cmp(other.vars.iter())
    }
}


impl<C: RingOps> Monomial<C> {
    pub fn new(coeff: C, mut vars: Vec<Var>) -> Self {
        vars.sort_unstable(); // Ensure vars are sorted
        // Combine duplicate variables by summing exponents (e.g. x*x -> x^2)
        let mut compacted_vars = Vec::new();
        if !vars.is_empty() {
            let mut current_var = vars[0].clone();
            for i in 1..vars.len() {
                if vars[i].id == current_var.id {
                    current_var.exp += vars[i].exp;
                } else {
                    if current_var.exp != 0 { // Don't add var^0
                        compacted_vars.push(current_var);
                    }
                    current_var = vars[i].clone();
                }
            }
             if current_var.exp != 0 {
                compacted_vars.push(current_var);
            }
        }
        Self { coeff, vars: compacted_vars }
    }

    pub fn is_constant(&self) -> bool {
        self.vars.is_empty()
    }

    pub fn to_constant_val(&self) -> Option<C> {
        if self.is_constant() {
            Some(self.coeff.clone())
        } else {
            None
        }
    }
}

impl<C: RingOps> Display for Monomial<C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.coeff.equals(&C::zero()) && !self.is_constant() { // Distinguish 0 constant from 0*vars
             return write!(f, "0"); // Or handle upstream in polynomial print
        }

        let is_one_coeff = self.coeff.equals(&C::one());
        let is_minus_one_coeff = self.coeff.equals(&C::one().neg());

        if self.is_constant() {
            return self.coeff.fmt(f);
        }
        
        if is_minus_one_coeff {
            write!(f, "-")?;
        } else if !is_one_coeff || self.vars.is_empty() { // Always print coeff if it's a constant term, or not 1
            self.coeff.fmt(f)?;
            // Add a multiplication sign if there are variables and coeff isn't 1 or -1 (implicit)
            // Or if coeff is displayed with a sign that might be ambiguous e.g. complex numbers later.
            // For now, simple check.
            if !self.vars.is_empty() && (!is_one_coeff && !is_minus_one_coeff) {
                 // Optionally print "*" here if style dictates, e.g. "2*x" vs "2x"
                 // OCaml version seems to not print it: "%a%a" R.print coeff print_varlist varl
            }
        }
        
        for var in &self.vars {
            var.fmt(f)?; // Assumes no separator like "*" between vars, e.g. "x^2y"
        }
        Ok(())
    }
}


#[derive(Clone, Debug, PartialEq, Eq)] // Eq requires C: Eq, RingOps implies C: PartialEq
pub struct Polynomial<C: RingOps> {
    // Terms are kept sorted by Monomial's Ord impl (lexicographical on vars)
    // and coefficients are non-zero.
    pub terms: Vec<Monomial<C>>,
}

impl<C: RingOps> Polynomial<C> {
    pub fn new(mut terms: Vec<Monomial<C>>) -> Self {
        // Filter out zero-coefficient terms and sort
        terms.retain(|m| !m.coeff.equals(&C::zero()));
        terms.sort_unstable(); // Relies on Monomial's Ord implementation
        Self { terms }
    }

    /// Creates a polynomial from a single coefficient (a constant).
    pub fn from_coeff(c: C) -> Self {
        if c.equals(&C::zero()) {
            Self::new(Vec::new()) // Zero polynomial
        } else {
            Self::new(vec![Monomial::new(c, Vec::new())])
        }
    }
    
    /// Creates a polynomial from a single variable name (coeff=1, exp=1).
    pub fn from_var_id(id: Id) -> Self {
        Self::new(vec![Monomial::new(C::one(), vec![Var { id, exp: 1 }])])
    }

    pub fn zero() -> Self {
        Self::new(Vec::new())
    }

    pub fn one() -> Self {
        Self::from_coeff(C::one())
    }
    
    pub fn from_i64(val: i64) -> Self {
        Self::from_coeff(C::from_i64(val))
    }

    pub fn from_f64(val: f64) -> Self {
        Self::from_coeff(C::from_f64(val))
    }

    pub fn from_rational(r: &Q) -> Self {
        Self::from_coeff(C::from_rational(r))
    }

    pub fn is_zero(&self) -> bool {
        self.terms.is_empty()
    }

    pub fn is_constant(&self) -> bool {
        self.terms.len() <= 1 && self.terms.first().map_or(true, |m| m.is_constant())
    }

    pub fn to_constant_val(&self) -> Option<C> {
        if self.terms.len() == 1 {
            self.terms[0].to_constant_val()
        } else if self.terms.is_empty() { // 0 polynomial
            Some(C::zero())
        }
        else {
            None
        }
    }
    
    pub fn is_monomial(&self) -> bool {
        self.terms.len() == 1
    }

    pub fn to_monomial_opt(&self) -> Option<&Monomial<C>> {
        if self.is_monomial() {
            self.terms.first()
        } else {
            None
        }
    }

    // OCaml: let is_linear (p : t) : bool = List.for_all (function _, [(_, 1)] -> true | _ -> false) p
    // This checks if all monomials in the polynomial are of the form c * v^1
    pub fn is_linear(&self) -> bool {
        self.terms.iter().all(|m| {
            m.vars.len() == 1 && m.vars[0].exp == 1
        })
    }
    
    // --- Arithmetic ---

    // OCaml: let add_cell (e : t) ((c, v) as cell : cell) : t = ...
    // Helper: adds a monomial to a polynomial (represented as sorted list of monomials)
    fn add_monomial_to_list(mut terms: Vec<Monomial<C>>, mono_to_add: Monomial<C>) -> Vec<Monomial<C>> {
        if mono_to_add.coeff.equals(&C::zero()) {
            return terms;
        }

        match terms.binary_search_by(|m| m.vars.iter().cmp(mono_to_add.vars.iter())) {
            Ok(idx) => { // Found monomial with same variables
                let existing_mono = &mut terms[idx];
                existing_mono.coeff = existing_mono.coeff.add(&mono_to_add.coeff);
                if existing_mono.coeff.equals(&C::zero()) {
                    terms.remove(idx);
                }
            }
            Err(idx) => { // No monomial with same variables, insert at sorted position
                terms.insert(idx, mono_to_add);
            }
        }
        terms
    }
    
    // OCaml: let add (e1 : t) (e2 : t) : t = List.fold_left add_cell e1 e2
    pub fn add_poly(&self, other: &Self) -> Self {
        let mut new_terms = self.terms.clone();
        for term_other in &other.terms {
            new_terms = Self::add_monomial_to_list(new_terms, term_other.clone());
        }
        // add_monomial_to_list ensures sorting and zero removal implicitly by its logic
        // but if it didn't, Polynomial::new would clean it.
        Polynomial { terms: new_terms } // relies on add_monomial_to_list maintaining invariants
    }

    // OCaml: let mul_cell_cell ((c1, vars1) : cell) ((c2, vars2) : cell) : cell = ...
    fn mul_monomial_monomial(m1: &Monomial<C>, m2: &Monomial<C>) -> Monomial<C> {
        let new_coeff = m1.coeff.mul(&m2.coeff);
        if new_coeff.equals(&C::zero()) {
            return Monomial::new(C::zero(), Vec::new());
        }

        let mut new_vars = Vec::new();
        let mut i = 0;
        let mut j = 0;
        while i < m1.vars.len() && j < m2.vars.len() {
            match m1.vars[i].id.cmp(&m2.vars[j].id) {
                Ordering::Less => {
                    new_vars.push(m1.vars[i].clone());
                    i += 1;
                }
                Ordering::Greater => {
                    new_vars.push(m2.vars[j].clone());
                    j += 1;
                }
                Ordering::Equal => {
                    new_vars.push(Var {
                        id: m1.vars[i].id.clone(),
                        exp: m1.vars[i].exp + m2.vars[j].exp,
                    });
                    i += 1;
                    j += 1;
                }
            }
        }
        new_vars.extend_from_slice(&m1.vars[i..]);
        new_vars.extend_from_slice(&m2.vars[j..]);
        
        // Filter out var^0
        new_vars.retain(|var| var.exp != 0);
        
        Monomial { coeff: new_coeff, vars: new_vars } // vars are already sorted due to merge logic
    }

    // OCaml: let mul_ex_cell (e : t) (c : cell) : t = List.map (mul_cell_cell c) e
    fn mul_poly_monomial(&self, mono: &Monomial<C>) -> Self {
        if mono.coeff.equals(&C::zero()) {
            return Self::zero();
        }
        let new_terms = self.terms.iter()
            .map(|self_mono| Self::mul_monomial_monomial(self_mono, mono))
            .filter(|m| !m.coeff.equals(&C::zero())) // Remove zero terms resulting from multiplication
            .collect();
        // Sorting might be needed if mul_monomial_monomial changes effective var order for sorting,
        // but it shouldn't if vars were already sorted.
        // However, the combination of variables could result in a list of monomials that are not
        // sorted relative to each other. So, Polynomial::new is important.
        Polynomial::new(new_terms)
    }

    // OCaml: let mul (e1 : t) (e2 : t) = List.fold_left (fun acc c -> add acc (mul_ex_cell e1 c)) zero e2
    pub fn mul_poly(&self, other: &Self) -> Self {
        if self.is_zero() || other.is_zero() {
            return Self::zero();
        }
        let mut acc = Polynomial::zero();
        for term_other in &other.terms {
            let prod_part = self.mul_poly_monomial(term_other);
            acc = acc.add_poly(&prod_part);
        }
        acc // relies on add_poly and mul_poly_monomial to maintain invariants
    }
    
    pub fn neg_poly(&self) -> Self {
        let new_terms = self.terms.iter().map(|m| {
            Monomial { coeff: m.coeff.neg(), vars: m.vars.clone() }
        }).collect();
        Polynomial { terms: new_terms } // Sorting is preserved, zero coeffs not introduced by neg
    }

    pub fn sub_poly(&self, other: &Self) -> Self {
        self.add_poly(&other.neg_poly())
    }

    // OCaml: let div (e1 : t) (d : coeff) : t = List.map (...) e1
    // Divides polynomial by a scalar coefficient. Returns None if any monomial division fails.
    pub fn div_scalar(&self, d: &C) -> Option<Self> {
        if d.equals(&C::zero()) {
            return None; // Division by zero scalar
        }
        let mut new_terms = Vec::new();
        for term in &self.terms {
            if let Some(new_coeff) = term.coeff.div_exact(d) {
                if !new_coeff.equals(&C::zero()) { // Keep if not zero after division
                    new_terms.push(Monomial { coeff: new_coeff, vars: term.vars.clone() });
                }
            } else {
                return None; // Division was not exact for one term
            }
        }
        // Order is preserved.
        Some(Polynomial { terms: new_terms })
    }

    // OCaml: let pow (p : t) (e : int) : t = ...
    pub fn pow_poly(&self, exp: i32) -> Self {
        if exp < 0 {
            // OCaml version doesn't handle negative exponents for polynomials
            // (would result in rational functions, not polynomials).
            panic!("Negative exponentiation for polynomials is not supported.");
        }
        if exp == 0 {
            return Self::one();
        }
        let mut res = self.clone();
        for _ in 1..exp {
            res = res.mul_poly(self);
        }
        res
    }
}

impl<C: RingOps> Display for Polynomial<C> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.terms.is_empty() {
            return write!(f, "0");
        }
        for (i, term) in self.terms.iter().enumerate() {
            if i > 0 {
                // Check if the coefficient is negative to avoid " + -"
                // This requires RingOps to have a way to check sign, or C to be Ord.
                // For simplicity, always print "+ " and rely on coeff's Display.
                // A more sophisticated print would handle this better.
                // If coeff prints with a leading "-", like "-5", then " + -5" is okay.
                // If it's just "5", then " + 5".
                // Let's assume Display for C handles its own sign.
                write!(f, " + ")?;
            }
            term.fmt(f)?;
        }
        Ok(())
    }
}


// --- Concrete Polynomial Types ---
pub type IntPolynomial = Polynomial<I>;
pub type FloatPolynomial = Polynomial<F>;
pub type RationalPolynomial = Polynomial<Q>;


#[cfg(test)]
mod tests {
    use super::*;
    use crate::arith_q::Q; // For RationalPolynomial tests

    // Helper to create rational polynomials for tests
    fn rp(terms: Vec<(Rational, Vec<(&str, Exp)>)>) -> RationalPolynomial {
        let mut monomials = Vec::new();
        for (coeff, var_tuples) in terms {
            let vars = var_tuples.into_iter().map(|(s,e)| Var {id: s.to_string(), exp: e}).collect();
            monomials.push(Monomial::new(coeff, vars));
        }
        Polynomial::new(monomials)
    }
     fn r(val: i64) -> Rational { Rational::from(val) }
     fn r_frac(num: i64, den: i64) -> Rational { Rational::from((num, den)) }


    #[test]
    fn test_var_display() {
        assert_eq!(Var { id: "x".to_string(), exp: 1 }.to_string(), "x");
        assert_eq!(Var { id: "y".to_string(), exp: 2 }.to_string(), "y^2");
    }

    #[test]
    fn test_monomial_new_sorting_compacting() {
        // x^1 * y^2 * x^2  => x^3 * y^2
        let vars = vec![
            Var{id: "x".to_string(), exp: 1},
            Var{id: "y".to_string(), exp: 2},
            Var{id: "x".to_string(), exp: 2},
        ];
        let mono = Monomial::new(r(1), vars);
        assert_eq!(mono.vars.len(), 2);
        assert_eq!(mono.vars[0], Var {id: "x".to_string(), exp: 3});
        assert_eq!(mono.vars[1], Var {id: "y".to_string(), exp: 2});

        let mono_x_0 = Monomial::new(r(1), vec![Var{id:"x".to_string(), exp:0}]);
        assert!(mono_x_0.is_constant());
        assert_eq!(mono_x_0.vars.len(), 0);
    }


    #[test]
    fn test_monomial_display() {
        let mono1 = Monomial::new(r(3), vec![Var { id: "x".to_string(), exp: 2 }, Var { id: "y".to_string(), exp: 1 }]);
        assert_eq!(mono1.to_string(), "3x^2y"); // Assuming no "*" printed by default
        
        let mono_const = Monomial::new(r(5), Vec::new());
        assert_eq!(mono_const.to_string(), "5");

        let mono_one_x = Monomial::new(r(1), vec![Var { id: "x".to_string(), exp: 1 }]);
        assert_eq!(mono_one_x.to_string(), "x");
        
        let mono_minus_one_x = Monomial::new(r(-1), vec![Var { id: "x".to_string(), exp: 1 }]);
        assert_eq!(mono_minus_one_x.to_string(), "-x");

        let mono_zero_x = Monomial::new(r(0), vec![Var { id: "x".to_string(), exp: 1 }]);
        assert_eq!(mono_zero_x.to_string(), "0");
    }

    #[test]
    fn test_polynomial_new_cleaning() {
        let p = rp(vec![
            (r(1), vec![("x",1)]), (r(0), vec![("y",1)]), (r(2), vec![("z",1)])
        ]);
        assert_eq!(p.terms.len(), 2); // Zero term removed
        assert_eq!(p.to_string(), "1x + 2z"); // Assuming default Display for Q, and sorted terms
    }
    
    #[test]
    fn test_polynomial_constants() {
        let pz = RationalPolynomial::zero();
        let po = RationalPolynomial::one();
        assert_eq!(pz.to_string(), "0");
        assert_eq!(po.to_string(), "1");
        assert!(pz.is_constant());
        assert!(po.is_constant());
        assert_eq!(po.to_constant_val(), Some(r(1)));
    }

    #[test]
    fn test_polynomial_from_var() {
        let px = RationalPolynomial::from_var_id("x".to_string());
        assert_eq!(px.to_string(), "1x"); // or just "x" depending on Monomial print for coeff=1
                                        // Current Monomial Display leads to "x"
        assert_eq!(px.to_string(), "x");
    }

    #[test]
    fn test_polynomial_add() {
        let p1 = rp(vec![(r(1), vec![("x",1)]), (r(2), vec![("y",1)])]); // x + 2y
        let p2 = rp(vec![(r(3), vec![("x",1)]), (r(-1), vec![("z",1)])]); // 3x - z
        // (x+2y) + (3x-z) = 4x + 2y - z
        let p_sum = p1.add_poly(&p2);
        // Order: x, y, z. So "4x + 2y + -1z"
        assert_eq!(p_sum.to_string(), "4x + 2y + -1z");

        let p3 = rp(vec![(r(1), vec![("x",1)])]); // x
        let p4 = rp(vec![(r(-1), vec![("x",1)])]); // -x
        let p_sum_zero = p3.add_poly(&p4);
        assert_eq!(p_sum_zero.to_string(), "0"); // Should be empty terms, prints "0"
        assert!(p_sum_zero.is_zero());
    }

    #[test]
    fn test_polynomial_mul_monomial_monomial() {
        let m1 = Monomial::new(r(2), vec![Var{id:"x".into(), exp:1}, Var{id:"y".into(), exp:2}]); // 2xy^2
        let m2 = Monomial::new(r(3), vec![Var{id:"x".into(), exp:2}, Var{id:"z".into(), exp:1}]); // 3x^2z
        // 6x^3y^2z
        let m_prod = Polynomial::mul_monomial_monomial(&m1, &m2);
        assert_eq!(m_prod.coeff, r(6));
        assert_eq!(m_prod.vars, vec![
            Var{id:"x".into(), exp:3}, Var{id:"y".into(), exp:2}, Var{id:"z".into(), exp:1}
        ]);
    }

    #[test]
    fn test_polynomial_mul() {
        let p1 = rp(vec![(r(1), vec![("x",1)]), (r(2), vec![])]); // x + 2
        let p2 = rp(vec![(r(1), vec![("x",1)]), (r(3), vec![])]); // x + 3
        // (x+2)(x+3) = x^2 + 3x + 2x + 6 = x^2 + 5x + 6
        let p_prod = p1.mul_poly(&p2);
        // Terms: 6 (const), 5x, x^2. Sorted: x^2, 5x, 6.
        // Display: "1x^2 + 5x + 6" or "x^2 + 5x + 6"
        assert_eq!(p_prod.to_string(), "1x^2 + 5x + 6");


        let p3 = rp(vec![(r(1), vec![("x",1)])]); // x
        let p_zero = RationalPolynomial::zero(); // 0
        assert_eq!(p3.mul_poly(&p_zero).to_string(), "0");
        assert!(p3.mul_poly(&p_zero).is_zero());
    }
    
    #[test]
    fn test_polynomial_neg_sub() {
        let p1 = rp(vec![(r(1), vec![("x",1)]), (r(2), vec![("y",1)])]); // x + 2y
        let p_neg = p1.neg_poly(); // -x - 2y
        assert_eq!(p_neg.to_string(), "-1x + -2y");

        let p_sub = p1.sub_poly(&p_neg); // (x+2y) - (-x-2y) = 2x + 4y
        assert_eq!(p_sub.to_string(), "2x + 4y");
    }

    #[test]
    fn test_div_scalar() {
        let p1 = rp(vec![(r(2), vec![("x",1)]), (r(4), vec![("y",1)])]); // 2x + 4y
        let p_div = p1.div_scalar(&r(2));
        assert!(p_div.is_some());
        assert_eq!(p_div.unwrap().to_string(), "1x + 2y");

        let p_div_fail = p1.div_scalar(&r(3)); // Not exact
        assert!(p_div_fail.is_none());
        
        let p_div_by_zero = p1.div_scalar(&r(0));
        assert!(p_div_by_zero.is_none());
    }
    
    #[test]
    fn test_pow_poly() {
        let p1 = rp(vec![(r(1), vec![("x",1)]), (r(2), vec![])]); // x + 2
        // (x+2)^0 = 1
        assert_eq!(p1.pow_poly(0).to_string(), "1");
        // (x+2)^1 = x + 2
        assert_eq!(p1.pow_poly(1).to_string(), "1x + 2");
        // (x+2)^2 = x^2 + 4x + 4
        let p_sq = p1.pow_poly(2);
        assert_eq!(p_sq.to_string(), "1x^2 + 4x + 4");
    }

     #[test]
    #[should_panic(expected = "Negative exponentiation for polynomials is not supported.")]
    fn test_pow_poly_negative_exp() {
        let p1 = RationalPolynomial::from_var_id("x".to_string());
        p1.pow_poly(-1);
    }
}
