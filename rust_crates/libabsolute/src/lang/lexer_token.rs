use rug::Rational;
use std::fmt::{self, Display, Debug};

#[derive(Clone, Debug, PartialEq)] // rug::Rational doesn't impl Eq directly
pub enum Token {
    // Keywords
    Init, Objective, Constraints, ConstantsDecl, Solutions, NoneKw,
    IntKw, RealKw, InfKw, InKw, NotInKw, ConvexKw,

    // Symbols
    LParen, RParen, LBrace, RBrace, LBracket, RBracket,
    Comma, Semicolon, Plus, Minus, Multiply, Divide, Pow,
    Less, Greater, LessEqual, GreaterEqual, NotEqual, Assign,
    And, Or, NotOp, // NotOp to avoid conflict with std::ops::Not

    // Literals
    TokId(String),
    TokConst(Rational),

    // Control
    Eof,

    // For unrecognized characters or lexing errors
    Error(String), 
}

impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Init => write!(f, "init"),
            Token::Objective => write!(f, "objective"),
            // ... (add all other keywords and symbols for completeness) ...
            // For brevity in this example, not all are explicitly listed.
            // A full implementation would list them like Token::Init.
            Token::Constraints => write!(f, "constraints"),
            Token::ConstantsDecl => write!(f, "constants"),
            Token::Solutions => write!(f, "solutions"),
            Token::NoneKw => write!(f, "none"),
            Token::IntKw => write!(f, "int"),
            Token::RealKw => write!(f, "real"),
            Token::InfKw => write!(f, "oo"),
            Token::InKw => write!(f, "in"),
            Token::NotInKw => write!(f, "notin"),
            Token::ConvexKw => write!(f, "convex"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Multiply => write!(f, "*"),
            Token::Divide => write!(f, "/"),
            Token::Pow => write!(f, "^"),
            Token::Less => write!(f, "<"),
            Token::Greater => write!(f, ">"),
            Token::LessEqual => write!(f, "<="),
            Token::GreaterEqual => write!(f, ">="),
            Token::NotEqual => write!(f, "!="),
            Token::Assign => write!(f, "="),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::NotOp => write!(f, "!"),
            Token::TokId(s) => write!(f, "ID({})", s),
            Token::TokConst(q) => write!(f, "CONST({})", crate::arith_q::pp_print_q(q)), // Use our q_print
            Token::Eof => write!(f, "EOF"),
            Token::Error(s) => write!(f, "LEX_ERROR({})", s),
            // Default for simple tokens:
            // _ => write!(f, "{:?}", self), // Basic debug print for other tokens // Covered above
        }
    }
}

// Helper to get line and column from a byte offset (span start) in the input string
pub fn get_line_col(input: &str, byte_offset: usize) -> (usize, usize) {
    let mut line_num = 1;
    let mut line_start_offset = 0;
    for (i, char_val) in input.char_indices() {
        if i >= byte_offset {
            break;
        }
        if char_val == '\n' {
            line_num += 1;
            line_start_offset = i + 1;
        }
    }
    let col_num = input[line_start_offset..byte_offset].chars().count() + 1;
    (line_num, col_num)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexicalError {
    pub message: String,
    pub filename: String, // Or Option<String>
    pub line: usize,
    pub column: usize,
}

impl Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Lexical error in '{}' at line {}, column {}: {}", 
               self.filename, self.line, self.column, self.message)
    }
}
impl std::error::Error for LexicalError {}
