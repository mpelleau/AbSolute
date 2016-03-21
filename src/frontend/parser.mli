type token =
  | TOK_LBRACE
  | TOK_RBRACE
  | TOK_LBRACKET
  | TOK_RBRACKET
  | TOK_LPAREN
  | TOK_RPAREN
  | TOK_COMMA
  | TOK_SEMICOLON
  | TOK_PLUS
  | TOK_MINUS
  | TOK_MULTIPLY
  | TOK_DIVIDE
  | TOK_POW
  | TOK_LESS
  | TOK_GREATER
  | TOK_LESS_EQUAL
  | TOK_GREATER_EQUAL
  | TOK_EQUAL_EQUAL
  | TOK_NOT_EQUAL
  | TOK_ASSIGN
  | TOK_AND
  | TOK_OR
  | TOK_NOT
  | TOK_INT
  | TOK_REAL
  | TOK_COS
  | TOK_SIN
  | TOK_SQRT
  | TOK_INIT
  | TOK_CONSTR
  | TOK_MINF
  | TOK_INF
  | TOK_id of (string)
  | TOK_const of (float)
  | TOK_EOF

val file :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.prog
