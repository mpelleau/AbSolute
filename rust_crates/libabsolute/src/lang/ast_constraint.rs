use std::fmt::{self, Display, Debug};
use super::ast_expr::Expr; // Relative import from sibling module

#[derive(Debug, Clone, PartialEq, Eq, Hash)] // Added Hash for CmpOp
pub enum CmpOp {
    Eq, Leq, Geq, Neq, Gt, Lt,
}

impl Display for CmpOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CmpOp::Eq => write!(f, "="),
            CmpOp::Leq => write!(f, "<="),
            CmpOp::Geq => write!(f, ">="),
            CmpOp::Neq => write!(f, "<>"), // OCaml uses != or <> based on context, parser.mly uses !=
            CmpOp::Gt => write!(f, ">"),
            CmpOp::Lt => write!(f, "<"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Comparison {
    pub e1: Expr,
    pub op: CmpOp,
    pub e2: Expr,
}

impl Display for Comparison {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.e1, self.op, self.e2)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Cmp(Comparison),
    And(Box<Constraint>, Box<Constraint>),
    Or(Box<Constraint>, Box<Constraint>),
    Not(Box<Constraint>),
    True,
    False,
}

impl Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Cmp(c) => c.fmt(f),
            Constraint::And(c1, c2) => write!(f, "({} && {})", c1, c2),
            Constraint::Or(c1, c2) => write!(f, "({} || {})", c1, c2),
            Constraint::Not(c) => write!(f, "not ({})", c),
            Constraint::True => write!(f, "true"),
            Constraint::False => write!(f, "false"),
        }
    }
}
