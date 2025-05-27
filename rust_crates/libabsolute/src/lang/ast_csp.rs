use std::fmt::{self, Display, Debug};
use std::collections::BTreeMap;
use crate::arith_q::Q; // rug::Rational
use super::ast_domain::DomainPlaceholder; // Relative import
use super::ast_expr::Expr;
use super::ast_constraint::Constraint;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VarType { Int, Real }

impl Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VarType::Int => write!(f, "int"),
            VarType::Real => write!(f, "real"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub var_type: VarType,
    pub name: String,
    pub domain: DomainPlaceholder, // Placeholder for Dom.t
}

impl Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} = {}", self.var_type, self.name, self.domain)
    }
}

// OCaml: type instance = Q.t VarMap.t
// VarMap from misc_tools was BTreeMap<String, V>
pub type Instance = BTreeMap<String, Q>;

#[derive(Debug, Clone, PartialEq)]
pub enum SolutionInfo {
    Exact(Vec<Instance>),
    Unfeasible,
    Known(Vec<(Instance, bool)>),
}

// Basic display for SolutionInfo
impl Display for SolutionInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SolutionInfo::Exact(instances) => write!(f, "Exact solutions: {:?}", instances), // Basic debug print for now
            SolutionInfo::Unfeasible => write!(f, "Unfeasible"),
            SolutionInfo::Known(known_instances) => write!(f, "Known instances: {:?}", known_instances),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct Csp {
    pub variables: Vec<Declaration>,
    pub constraints: Vec<Constraint>,
    pub objective: Option<Expr>,
    pub solutions: Option<SolutionInfo>,
}

impl Display for Csp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "init {{")?;
        for decl in &self.variables {
            writeln!(f, "  {};", decl)?;
        }
        writeln!(f, "}}")?;
        
        writeln!(f, "constraints {{")?;
        for constraint in &self.constraints {
            writeln!(f, "  {};", constraint)?;
        }
        writeln!(f, "}}")?;

        if let Some(obj) = &self.objective {
            writeln!(f, "objective {{ {} }}", obj)?;
        }
        if let Some(sol) = &self.solutions {
            writeln!(f, "solutions {{ {} }}", sol)?;
        }
        Ok(())
    }
}
