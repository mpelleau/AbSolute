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
%token PPOINT
%token PLUS
%token MINUS
%token MULTIPLY
%token DIVIDE
%token POW
%token LT
%token GT
%token LTE
%token GTE
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
  | VAR ID GTE expr COMMA LTE expr                 { Var ($2,$4,$7) }
  | VAR ID set GTE expr COMMA LTE expr             { VarList ($2,$3,$5,$8) }
  | PARAM ID COLONEQUAL expr                       { Param ($2,$4) }
  | ID COLON bexpr                                 { SubjectTo ($1,$3) }


set :
  | LBRACE expr PPOINT expr RBRACE                 { ($2,$4) }


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
  | MINUS expr %prec unary_minus    { Unary (Csp.NEG, $2) }
  | COS       expr                  { Unary (Csp.COS, $2) }
  | SIN       expr                  { Unary (Csp.SIN, $2) }
  | TAN       expr                  { Unary (Csp.TAN, $2) }
  | COT       expr                  { Unary (Csp.COT, $2) }
  | ASIN      expr                  { Unary (Csp.ASIN, $2) }
  | ACOS      expr                  { Unary (Csp.ACOS, $2) }
  | ATAN      expr                  { Unary (Csp.ATAN, $2) }
  | ACOT      expr                  { Unary (Csp.ACOT, $2) }
  | LN        expr                  { Unary (Csp.LN, $2) }
  | LOG       expr                  { Unary (Csp.LOG, $2) }
  | EXP       expr                  { Unary (Csp.EXP, $2) }
  | SQRT      expr                  { Unary (Csp.SQRT,$2) }
  | PIPE expr PIPE                  { Unary (Csp.ABS, $2) }
  | leaf                            { $1 }

leaf:
  | FLOAT                           { Cst $1 }
  | ID                              { Var $1 }
  | ID LBRACKET FLOAT RBRACKET      { Array($1,(int_of_float $3)) }

binop_expr:
  | expr POW expr  {Binary (Csp.POW,$1,$3)}
  | binop_expr2    {$1}

binop_expr2:
  | expr DIVIDE   expr  {Binary(Csp.DIV,$1,$3)}
  | expr MULTIPLY expr  {Binary(Csp.MUL,$1,$3)}
  | binop_expr3         {$1}

binop_expr3:
  | expr PLUS  expr   {Binary(Csp.ADD,$1,$3)}
  | expr MINUS expr   {Binary(Csp.SUB,$1,$3)}
  | binop_expr4       {$1}

binop_expr4:
  | MIN LPAREN expr COMMA expr RPAREN    {Binary (Csp.MIN,$3,$5)}
  | MAX LPAREN expr COMMA expr RPAREN    {Binary (Csp.MAX,$3,$5)}
  | NROOT LPAREN expr COMMA expr RPAREN  {Binary (Csp.NROOT,$3,$5)}

cmp:
  | LT                      { Csp.LT }
  | GT                      { Csp.GT }
  | LTE                     { Csp.LEQ }
  | GTE                     { Csp.GEQ }
  | ASSIGN                  { Csp.EQ }
  | NOT_EQUAL               { Csp.NEQ }
