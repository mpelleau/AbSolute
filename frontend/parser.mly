%{
open Syntax
%}


/* tokens */
%token TOK_LBRACE
%token TOK_RBRACE
%token TOK_LBRACKET
%token TOK_RBRACKET
%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_COMMA
%token TOK_SEMICOLON
%token TOK_PLUS
%token TOK_MINUS
%token TOK_MULTIPLY
%token TOK_DIVIDE
%token TOK_POW
%token TOK_LESS
%token TOK_GREATER
%token TOK_LESS_EQUAL
%token TOK_GREATER_EQUAL
%token TOK_EQUAL_EQUAL
%token TOK_NOT_EQUAL
%token TOK_ASSIGN
%token TOK_AND
%token TOK_OR
%token TOK_NOT
%token TOK_INT
%token TOK_REAL

%token TOK_COS
%token TOK_SIN
%token TOK_SQRT
%token TOK_INIT
%token TOK_CONSTR

%token <string> TOK_id
%token <float> TOK_const

%token TOK_EOF

/* priorities */
%left TOK_OR
%left TOK_AND
%nonassoc TOK_NOT
%left TOK_PLUS 
%left TOK_MINUS
%left TOK_MULTIPLY 
%left TOK_DIVIDE
%left TOK_POW
%nonassoc unary_minus
%nonassoc TOK_COS TOK_SIN TOK_SQRT

%type <typ> typ
%type <dom> init
%type <assign> decl
%type <bexpr> bexpr
%type <Syntax.prog> file

/* entry point */
%start file

%%

file: 
  TOK_INIT decls
  TOK_CONSTR bexprs
  TOK_EOF
  { {init=$2;constraints=$4} }

decls:
  | decl decls {$1::$2}
  | {[]}

bexprs:
  | bexpr bexprs {$1::$2}
  | {[]}

typ:
  | TOK_INT       {INT}
  | TOK_REAL      {REAL}

decl:
  | typ TOK_id TOK_ASSIGN init TOK_SEMICOLON
    { ($1, $2, $4) }

init:
  | TOK_LBRACKET TOK_const TOK_COMMA TOK_const TOK_RBRACKET 
      {($2,$4)}

bexpr:
  | expr cmp expr                       {Cmp ($2, $1, $3)}
  | bexpr TOK_OR bexpr                  {Or ($1,$3)}
  | bexpr TOK_AND bexpr                 {And ($1,$3)}
  | TOK_NOT bexpr                       {Not ($2)}
  | TOK_LPAREN bexpr TOK_RPAREN         { $2 }


expr:
  | TOK_id                              { Var $1 }
  | binop_expr                          { $1 }
  | TOK_COS expr                        { Unary (COS,$2) }
  | TOK_SIN expr                        { Unary (SIN,$2) }
  | TOK_SQRT expr                       { Unary (SQRT,$2) }
  | TOK_const                           { Cst ($1,$1) }
  | parenthesized_expr                  { $1 }

parenthesized_expr :
  | TOK_LPAREN expr TOK_RPAREN                { $2 }
  | TOK_LPAREN expr_with_prefix TOK_RPAREN    { $2 }


binop_expr:
  | expr TOK_POW expr {Binary (POW,$1,$3)}
  | binop_expr2 {$1}

binop_expr2:
  | expr TOK_DIVIDE expr {Binary(DIV,$1,$3)}
  | expr TOK_MULTIPLY expr {Binary(MUL,$1,$3)}
  | binop_expr3 {$1}

binop_expr3:
  | expr TOK_PLUS expr {Binary(ADD,$1,$3)}
  | expr TOK_MINUS expr {Binary(SUB,$1,$3)}

expr_with_prefix :
  | TOK_MINUS expr { Unary (NEG,$2) }

cmp:
  | TOK_LESS                    { LT }
  | TOK_GREATER                 { GT }
  | TOK_LESS_EQUAL              { LEQ } 
  | TOK_GREATER_EQUAL           { GEQ }
  | TOK_EQUAL_EQUAL             { EQ }
  | TOK_NOT_EQUAL               { NEQ }
