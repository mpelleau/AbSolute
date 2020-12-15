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
%token NOT_EQUAL
%token ASSIGN
%token COLONEQUAL
%token AND
%token OR
%token NOT
%token PARAM
%token VAR

%token <string> ID
%token <float> FLOAT

%token EOF

/* priorities */
%left OR AND
%nonassoc NOT
%left PLUS MINUS
%left MULTIPLY  DIVIDE
%nonassoc unary_minus
%nonassoc POW

%type <bexpr> bexpr
%type <ModCsp.modstmt> stmt
%type <ModCsp.t> stmts

/* entry point */
%start stmts

%%

// separated_list with optional separator at the end
%public separated_optend(sep,X):
  | sep? {[]}
  | x=X sep xs=separated_optend(sep,X) {x::xs}

stmts:
  | l=separated_optend(SEMICOLON,stmt) EOF { l }

stmt:
  | VAR ID GTE expr COMMA LTE expr                 { Var ($2,$4,$7) }
  | VAR ID set GTE expr COMMA LTE expr             { VarList ($2,$3,$5,$8) }
  | PARAM ID COLONEQUAL expr                       { Param ($2,$4) }
  | ID COLON bexpr                                 { SubjectTo ($1,$3) }


set:
  | LBRACE expr PPOINT expr RBRACE                 { ($2,$4) }

bexpr:
  | expr cmp expr                   {Cmp ($1, $2, $3)}
  | bexpr OR bexpr                  {Or  ($1,$3)}
  | bexpr AND bexpr                 {And ($1,$3)}
  | NOT bexpr                       {Not ($2)}
  | LPAREN bexpr RPAREN             { $2 }

expr:
  | LPAREN expr RPAREN              { $2 }
  | binop_expr                      { $1 }
  | MINUS expr %prec unary_minus    { Neg $2 }
  | leaf                            { $1 }

leaf:
  | FLOAT                           { Cst (Mpqf.of_float $1) }
  | ID                              { Var $1 }
  | ID LBRACKET FLOAT RBRACKET      { Array($1,(int_of_float $3)) }

binop_expr:
  | expr POW expr  {Binary (Csp.POW,$1,$3)}
  | expr DIVIDE   expr  {Binary(Csp.DIV,$1,$3)}
  | expr MULTIPLY expr  {Binary(Csp.MUL,$1,$3)}
  | expr PLUS  expr   {Binary(Csp.ADD,$1,$3)}
  | expr MINUS expr   {Binary(Csp.SUB,$1,$3)}

cmp:
  | LT                      { Csp.LT }
  | GT                      { Csp.GT }
  | LTE                     { Csp.LEQ }
  | GTE                     { Csp.GEQ }
  | ASSIGN                  { Csp.EQ }
  | NOT_EQUAL               { Csp.NEQ }
