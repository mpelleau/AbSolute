use std::fmt::{self, Display, Debug};
use crate::arith_q::Q; // Using Q = rug::Rational from arith_q.rs

#[derive(Debug, Clone, PartialEq, Eq, Hash)] // Added Hash for BinOp for potential use in HashMap/Set
pub enum BinOp {
    Add, Sub, Mul, Div, Pow,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Pow => write!(f, "^"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)] // rug::Rational doesn't impl Eq directly
pub enum Expr {
    Funcall(String, Vec<Expr>),
    Neg(Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Var(String),
    Cst(Q),
}

// Manual Display implementation to match OCaml's Expr.print
impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Funcall(name, args) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    arg.fmt(f)?;
                }
                write!(f, ")")
            }
            Expr::Neg(e) => write!(f, "(- {})", e),
            Expr::Binary(op, e1, e2) => write!(f, "({} {} {})", e1, op, e2),
            Expr::Var(v) => write!(f, "{}", v),
            Expr::Cst(c) => crate::arith_q::pp_print_q(c).fmt(f), // Use custom q_print
        }
    }
}

// Placeholder for OCaml's 'a annot_t types. Not implemented in detail yet.
// pub type AnnotatedExpr<A> = (Expr, A);
