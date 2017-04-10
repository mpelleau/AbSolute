%{
  open ModCsp
%}


/* tokens */
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token LPAREN
%token RPAREN
%token PIPE
%token COMMA
%token SEMICOLON
%token COLON
%token PLUS
%token MINUS
%token MULTIPLY
%token DIVIDE
%token POW
%token LESS
%token GREATER
%token LESS_EQUAL
%token GREATER_EQUAL
%token EQUAL_EQUAL
%token NOT_EQUAL
%token ASSIGN
%token COLONEQUAL
%token AND
%token OR
%token NOT
%token INT
%token REAL
%token COS
%token SIN
%token TAN
%token COT
%token ASIN
%token ACOS
%token ATAN
%token ACOT
%token LN
%token LOG
%token EXP
%token NROOT
%token MIN
%token MAX
%token SQRT
%token INIT
%token OBJ
%token CONSTR
%token PARAM
%token SUBJECT_TO
%token VAR
%token MINF
%token INF

%token <string> ID
%token <float> FLOAT

%token EOF

/* priorities */
%left OR AND
%nonassoc NOT
%left PLUS MINUS
%left MULTIPLY  DIVIDE
%nonassoc unary_minus
%nonassoc COS SIN TAN COT
%nonassoc ASIN ACOS ATAN ACOT
%nonassoc LN LOG EXP NROOT SQRT POW

%type <bexpr> bexpr
%type <ModCsp.modstmt> stmt
%type <ModCsp.t> stmts

/* entry point */
%start stmts

%%

stmts:
  | stmt SEMICOLON stmts {$1::$3}
  | {[]}

stmt:
  | VAR ID GREATER_EQUAL expr COMMA LESS_EQUAL expr { ModCsp.Var ($2,$4,$7) }
  | PARAM ID COLONEQUAL expr                        { ModCsp.Param ($2,$4) }
  | SUBJECT_TO ID COLON bexpr                       { ModCsp.SubjectTo ($2,$4) }
  | ID COLON bexpr                                  { ModCsp.SubjectTo ($1,$3) }


const:
  | FLOAT {$1}
  | MINUS FLOAT {(-.$2)}

bexpr:
  | expr cmp expr                   {Cmp ($2, $1, $3)}
  | bexpr OR bexpr                  {Or  ($1,$3)}
  | bexpr AND bexpr                 {And ($1,$3)}
  | NOT bexpr                       {Not ($2)}
  | LPAREN bexpr RPAREN             { $2 }


expr:
  | LPAREN expr RPAREN              { $2 }
  | binop_expr                      { $1 }
  | MINUS expr %prec unary_minus    { Unary (NEG, $2) }
  | COS       expr                  { Unary (COS, $2) }
  | SIN       expr                  { Unary (SIN, $2) }
  | TAN       expr                  { Unary (TAN, $2) }
  | COT       expr                  { Unary (COT, $2) }
  | ASIN      expr                  { Unary (ASIN, $2) }
  | ACOS      expr                  { Unary (ACOS, $2) }
  | ATAN      expr                  { Unary (ATAN, $2) }
  | ACOT      expr                  { Unary (ACOT, $2) }
  | LN        expr                  { Unary (LN, $2) }
  | LOG       expr                  { Unary (LOG, $2) }
  | EXP       expr                  { Unary (EXP, $2) }
  | SQRT      expr                  { Unary (SQRT,$2) }
  | PIPE expr PIPE                  { Unary (ABS, $2) }
  | leaf                            { $1 }


leaf:
  | FLOAT                           { Cst $1 }
  | ID                              { Var $1 }

binop_expr:
  | expr POW expr  {Binary (POW,$1,$3)}
  | binop_expr2        {$1}

binop_expr2:
  | expr DIVIDE   expr  {Binary(DIV,$1,$3)}
  | expr MULTIPLY expr  {Binary(MUL,$1,$3)}
  | binop_expr3             {$1}

binop_expr3:
  | expr PLUS  expr {Binary(ADD,$1,$3)}
  | expr MINUS expr {Binary(SUB,$1,$3)}
  | binop_expr4             {$1}

binop_expr4:
  | MIN LPAREN expr COMMA expr RPAREN {Binary (MIN,$3,$5)}
  | MAX LPAREN expr COMMA expr RPAREN {Binary (MAX,$3,$5)}
  | NROOT LPAREN expr COMMA expr RPAREN {Binary (NROOT,$3,$5)}

cmp:
  | LESS                    { LT }
  | GREATER                 { GT }
  | LESS_EQUAL              { LEQ }
  | GREATER_EQUAL           { GEQ }
  | ASSIGN                  { EQ }
  | NOT_EQUAL               { NEQ }
