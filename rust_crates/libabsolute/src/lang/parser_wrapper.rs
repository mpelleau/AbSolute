// This module will include the LALRPOP-generated parser
// and provide a clean API to it.

use crate::lang::lexer::MainLexer; // Our lexer
use crate::lang::lexer_token::{Token, LexicalError}; // Our token and error types
use crate::lang::ast_csp::Csp;
use crate::lang::ast_expr::Expr;
use crate::lang::ast_constraint::Constraint;

// Include the LALRPOP-generated code.
// The actual name `libabsolute_parser` here must match what LALRPOP generates,
// which is derived from the .lalrpop filename.
// If filename is `libabsolute_parser.lalrpop`, then it's `libabsolute_parser`.
mod generated_parser {
    #![allow(clippy::all)] // Disable clippy for generated code
    #![allow(unused_imports)] // To handle potential unused imports in generated code
    #![allow(dead_code)] // To handle potential unused functions/types
    include!(concat!(env!("OUT_DIR"), "/libabsolute_parser.rs"));
}

// Define a unified ParseError type
pub type ParseError = lalrpop_util::ParseError<usize, Token, LexicalError>;

// Public API functions for parsing
// These functions will take the input string and a filename (for error reporting).

/// Parses a complete CSP file content.
pub fn parse_csp_str(input: &str, filename: String) -> Result<Csp, ParseError> {
    let lexer = MainLexer::new(input, filename);
    // The generated parser struct will be named after the grammar file.
    // If grammar file is `libabsolute_parser.lalrpop`, struct is `LibabsoluteParserParser`.
    generated_parser::LibabsoluteParserParser::new().parse_File(lexer)
}

/// Parses a single expression string.
pub fn parse_expr_str(input: &str, filename: String) -> Result<Expr, ParseError> {
    let lexer = MainLexer::new(input, filename);
    generated_parser::LibabsoluteParserParser::new().parse_ExprEof(lexer)
}

/// Parses a single constraint string.
pub fn parse_constraint_str(input: &str, filename: String) -> Result<Constraint, ParseError> {
    let lexer = MainLexer::new(input, filename);
    generated_parser::LibabsoluteParserParser::new().parse_BExprEof(lexer)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::arith_q::Q;

    #[test]
    fn test_parse_simple_expr() {
        let result = parse_expr_str("1 + 2", "test_expr.abs".to_string());
        match result {
            Ok(Expr::Binary(op, e1, e2)) => {
                assert_eq!(op, crate::lang::ast_expr::BinOp::Add);
                if let Expr::Cst(val1) = *e1 { assert_eq!(val1, Q::from(1)); } 
                else { panic!("Expected Cst(1)"); }
                if let Expr::Cst(val2) = *e2 { assert_eq!(val2, Q::from(2)); }
                else { panic!("Expected Cst(2)"); }
            }
            _ => panic!("Parsing failed or produced unexpected AST: {:?}", result),
        }
    }

    #[test]
    fn test_parse_expr_with_id() {
        let result = parse_expr_str("my_var * (another_var + 3)", "test_expr_id.abs".to_string());
        assert!(result.is_ok(), "Parsing error: {:?}", result.err());
        // Deeper structural validation can be added here.
        eprintln!("Parsed expr: {}", result.unwrap());
    }
    
    #[test]
    fn test_parse_simple_constraint() {
        let result = parse_constraint_str("x = 10", "test_constr.abs".to_string());
        match result {
            Ok(Constraint::Cmp(cmp)) => {
                if let Expr::Var(v) = cmp.e1 { assert_eq!(v, "x"); }
                else { panic!("Expected Var(x)"); }
                assert_eq!(cmp.op, crate::lang::ast_constraint::CmpOp::Eq);
                if let Expr::Cst(val) = cmp.e2 { assert_eq!(val, Q::from(10)); }
                else { panic!("Expected Cst(10)"); }
            }
            _ => panic!("Parsing failed or produced unexpected AST for constraint: {:?}", result),
        }
    }

    #[test]
    fn test_parse_full_csp_minimal() {
        // Minimal valid CSP based on grammar structure so far
        // (assuming InitDomainRule can parse something simple for now)
        let input = r#"
            init {
                real x = [-10; 10];
            }
            constraints {
                x >= 0;
            }
        "#;
        // Note: InitDomainRule is currently a placeholder in LALRPOP, so this test might fail
        // if LALRPOP cannot parse `[-10; 10]` with the current InitDomainRule.
        // The test is more about structure and integration.
        // For now, let's simplify the domain to something the placeholder `InitDomainRule` might accept
        // or just test parsing an empty init/constraints block if the rules are more robust for that.
        // The current InitDomainRule placeholder is `=> DomainPlaceholder::Unknown,`
        // which means it would match an empty production for domain.
        // The OCaml grammar `DeclarationRule`: `vt id ASSIGN LBRACKET dom RBRACKET`
        // The current LALRPOP `InitDomainRule` has:
        // `<Token::Minus> <Token::InfKw> <Token::Semicolon> <Token::Plus?> <Token::InfKw> => DomainPlaceholder::Interval("-oo".to_string(), "oo".to_string()),`
        // `<r1:RationalConst> <Token::Semicolon> <r2:RationalConst> => DomainPlaceholder::Interval(r1.to_string(), r2.to_string()),`
        // `=> DomainPlaceholder::Unknown,` -- this last rule is problematic as it makes InitDomainRule match empty.
        // It should not match empty unless explicitly intended.
        // Let's assume for the test that `real x = [-10;10]` can be parsed by the RationalConst rule.
        
        let result = parse_csp_str(input, "test_csp.abs".to_string());
        if let Err(e) = &result {
            eprintln!("CSP Parse Error: {:?}", e);
            // Detailed error logging:
            if let lalrpop_util::ParseError::User { error: lex_error } = e {
                eprintln!("Lexical Error: {}", lex_error);
            }
        }
        assert!(result.is_ok());
        let csp = result.unwrap();
        assert_eq!(csp.variables.len(), 1);
        assert_eq!(csp.constraints.len(), 1);
        eprintln!("Parsed CSP: {}", csp);
    }
    
    #[test]
    fn test_parse_error_handling() {
        let result = parse_expr_str("1 + @", "bad_expr.abs".to_string());
        assert!(result.is_err());
        eprintln!("Parse error (expected): {:?}", result.err().unwrap());
    }

}
