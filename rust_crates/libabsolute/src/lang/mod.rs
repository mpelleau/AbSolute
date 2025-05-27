// Declare modules for the lang sub-crate
pub mod ast_domain;
pub mod ast_expr;
pub mod ast_constraint;
pub mod ast_csp;
pub mod lexer_token;
pub mod lexer;
// pub mod libabsolute_parser; // We don't declare the generated module directly
pub mod parser_wrapper;     // New wrapper module

// Re-export key types for easier access
pub use ast_domain::*;
pub use ast_expr::*;
pub use ast_constraint::*;
pub use ast_csp::*;
pub use lexer_token::{Token, LexicalError};
pub use lexer::MainLexer;
pub use parser_wrapper::{parse_csp_str, parse_expr_str, parse_constraint_str, ParseError}; // Re-export parse functions
