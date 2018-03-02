%{
open Csp
%}


/* tokens */
%token TOK_LBRACE
%token TOK_RBRACE
%token TOK_LBRACKET
%token TOK_RBRACKET
%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_PIPE
%token TOK_COMMA
%token TOK_SEMICOLON
%token TOK_COLON
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
%token TOK_TAN
%token TOK_COT
%token TOK_ASIN
%token TOK_ACOS
%token TOK_ATAN
%token TOK_ACOT
%token TOK_LN
%token TOK_LOG
%token TOK_EXP
%token TOK_NROOT
%token TOK_MIN
%token TOK_MAX
%token TOK_SQRT
%token TOK_CST
%token TOK_INIT
%token TOK_OBJ
%token TOK_CONSTR
%token TOK_ANNOT
%token TOK_DRAW
%token TOK_MINF
%token TOK_INF

%token <string> TOK_id
%token <float> TOK_const

%token TOK_EOF

/* priorities */
%left TOK_OR TOK_AND
%nonassoc TOK_NOT
%left TOK_PLUS TOK_MINUS
%left TOK_MULTIPLY  TOK_DIVIDE
%nonassoc unary_minus
%nonassoc TOK_COS TOK_SIN TOK_TAN TOK_COT
%nonassoc TOK_ASIN TOK_ACOS TOK_ATAN TOK_ACOT
%nonassoc TOK_LN TOK_LOG TOK_EXP TOK_NROOT TOK_SQRT TOK_POW

%type <typ> typ
%type <dom> init
%type <bexpr> bexpr
%type <Csp.prog> file

/* entry point */
%start file

%%

file:
  annot
  constants
  domains
  objective
  constraints
  TOK_EOF
  {
    {
      jacobian=[];
      view=[];
      init=$3;
      objective=$4;
      constraints=$5;
      to_draw=$1;
      constants=$2
    }
  }

annot:
 | TOK_ANNOT TOK_LBRACE annot2 TOK_RBRACE {$3}
 | {[]}

domains:
 | TOK_INIT TOK_LBRACE decls TOK_RBRACE {$3}

objective:
 | TOK_OBJ TOK_LBRACE expr TOK_RBRACE {$3}
 | {Cst(0.)}

constraints:
 | TOK_CONSTR TOK_LBRACE bexprs TOK_RBRACE {$3}

constants:
  | TOK_CST TOK_LBRACE csts TOK_RBRACE {$3}
  | {[]}

csts:
  | TOK_id TOK_ASSIGN value TOK_SEMICOLON csts {($1, $3)::$5}
  | TOK_id TOK_ASSIGN value {[($1, $3)]}
  | {[]}

value:
  | TOK_LBRACKET const TOK_SEMICOLON const TOK_RBRACKET {($2, $4)}
  | const {($1, $1)}

annot2:
 | TOK_DRAW TOK_COLON varlist {$3}

varlist:
  | TOK_id varlist {$1::$2}
  | {[]}

decls:
  | typ TOK_id TOK_ASSIGN init TOK_SEMICOLON decls {($1,$2,$4)::$6}
  | typ TOK_id TOK_ASSIGN init {[($1, $2, $4)]}
  | {[]}

bexprs:
  | bexpr TOK_SEMICOLON bexprs {$1::$3}
  | bexpr {[$1]}
  | {[]}

typ:
  | TOK_INT       {INT}
  | TOK_REAL      {REAL}

init:
  | TOK_LBRACKET TOK_MINF TOK_SEMICOLON TOK_INF TOK_RBRACKET                   {Top}
  | TOK_LBRACKET TOK_MINUS TOK_INF TOK_SEMICOLON TOK_INF TOK_RBRACKET          {Top}
  | TOK_LBRACKET TOK_MINF TOK_SEMICOLON TOK_PLUS TOK_INF TOK_RBRACKET          {Top}
  | TOK_LBRACKET TOK_MINUS TOK_INF TOK_SEMICOLON TOK_PLUS TOK_INF TOK_RBRACKET {Top}
  | TOK_LBRACKET TOK_MINF TOK_SEMICOLON const TOK_RBRACKET                     {Minf ($4)}
  | TOK_LBRACKET TOK_MINUS TOK_INF TOK_SEMICOLON const TOK_RBRACKET            {Minf ($5)}
  | TOK_LBRACKET const TOK_SEMICOLON TOK_INF TOK_RBRACKET                      {Inf ($2)}
  | TOK_LBRACKET const TOK_SEMICOLON TOK_PLUS TOK_INF TOK_RBRACKET             {Inf ($2)}
  | TOK_LBRACKET const TOK_SEMICOLON const TOK_RBRACKET                        {Finite($2,$4)}

const:
  | TOK_const {$1}
  | TOK_MINUS TOK_const {(-.$2)}

bexpr:
  | expr cmp expr                       {Cmp ($2, $1, $3)}
  | bexpr TOK_OR bexpr                  {Or ($1,$3)}
  | bexpr TOK_AND bexpr                 {And ($1,$3)}
  | TOK_NOT bexpr                       {Not ($2)}
  | TOK_LPAREN bexpr TOK_RPAREN         { $2 }


expr:
  | TOK_LPAREN expr TOK_RPAREN          { $2 }
  | binop_expr                          { $1 }
  | TOK_MINUS expr %prec unary_minus    { Unary (NEG, $2) }
  | TOK_COS       expr                  { Unary (COS, $2) }
  | TOK_SIN       expr                  { Unary (SIN, $2) }
  | TOK_TAN       expr                  { Unary (TAN, $2) }
  | TOK_COT       expr                  { Unary (COT, $2) }
  | TOK_ASIN      expr                  { Unary (ASIN, $2) }
  | TOK_ACOS      expr                  { Unary (ACOS, $2) }
  | TOK_ATAN      expr                  { Unary (ATAN, $2) }
  | TOK_ACOT      expr                  { Unary (ACOT, $2) }
  | TOK_LN        expr                  { Unary (LN, $2) }
  | TOK_LOG       expr                  { Unary (LOG, $2) }
  | TOK_EXP       expr                  { Unary (EXP, $2) }
  | TOK_SQRT      expr                  { Unary (SQRT,$2) }
  | TOK_PIPE expr TOK_PIPE              { Unary (ABS, $2) }
  | leaf                                { $1 }

leaf:
  | TOK_const                           { Cst $1 }
  | TOK_id                              { Var $1 }
  /* | TOK_const TOK_id                    {Binary(MUL,(Cst $1),(Var $2))} */
  /* | TOK_id TOK_const                    {Binary(MUL,(Var $1),(Cst $2))} */

binop_expr:
  | expr TOK_POW expr  {Binary (POW,$1,$3)}
  | binop_expr2        {$1}

binop_expr2:
  | expr TOK_DIVIDE   expr  {Binary(DIV,$1,$3)}
  | expr TOK_MULTIPLY expr  {Binary(MUL,$1,$3)}
  | binop_expr3             {$1}

binop_expr3:
  | expr TOK_PLUS  expr {Binary(ADD,$1,$3)}
  | expr TOK_MINUS expr {Binary(SUB,$1,$3)}
  | binop_expr4             {$1}

binop_expr4:
  | TOK_MIN TOK_LPAREN expr TOK_COMMA expr TOK_RPAREN {Binary (MIN,$3,$5)}
  | TOK_MAX TOK_LPAREN expr TOK_COMMA expr TOK_RPAREN {Binary (MAX,$3,$5)}
  | TOK_NROOT TOK_LPAREN expr TOK_COMMA expr TOK_RPAREN {Binary (NROOT,$3,$5)}

cmp:
  | TOK_LESS                    { LT }
  | TOK_GREATER                 { GT }
  | TOK_LESS_EQUAL              { LEQ }
  | TOK_GREATER_EQUAL           { GEQ }
  | TOK_ASSIGN                  { EQ }
  | TOK_NOT_EQUAL               { NEQ }
