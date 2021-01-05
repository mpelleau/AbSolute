%{
open Tools
open Csp
open Csp_helper
%}


/* Keywords */
%token INT           /* int */
%token REAL          /* real */
%token CST           /* constants */
%token INIT          /* init */
%token OBJ           /* objective */
%token CONSTR        /* constraints */
%token SOL           /* solutions */
%token NONE          /* none */
%token INF           /* oo */
%token IN            /* in */
%token NOTIN         /* notin */

/* Delimiters */
%token LBRACE        /* { */
%token RBRACE        /* } */
%token LBRACKET      /* [ */
%token RBRACKET      /* ] */
%token LPAREN        /* ( */
%token RPAREN        /* ) */

/* Operators */
%token COMMA         /* , */
%token SCOLON        /* ; */
%token PLUS          /* + */
%token MINUS         /* - */
%token MULTIPLY      /* * */
%token DIVIDE        /* / */
%token POW           /* ^ */
%token LESS          /* < */
%token GREATER       /* > */
%token LESS_EQUAL    /* <= */
%token GREATER_EQUAL /* >= */
%token NOT_EQUAL     /* != */
%token ASSIGN        /* = */
%token AND           /* && */
%token OR            /* || */
%token NOT           /* ! */

%token <string> TOK_id
%token <Mpqf.t> TOK_const

%token EOF

/* priorities */
%left OR AND
%nonassoc NOT
%left PLUS MINUS
%left MULTIPLY  DIVIDE
%nonassoc unary_minus
%nonassoc POW

%type <annot> typ
%type <dom> init
%type <bexpr> bexpr
%type <Csp.prog> file

/* entry point */
%start file

%%
/* useful parametrized rules */
/*****************************/

// {x}
%inline brace(YOURSELF):
  | LBRACE content=YOURSELF RBRACE { content }

// [x]
%inline bracket(X):
  | LBRACKET content=X RBRACKET { content }

// [x;y]
%inline itv(X,Y):
  | LBRACKET X SCOLON Y RBRACKET { content }

// separated_list with optional separator at the end
%public optend(sep,X):
  | X {[$1]}
  | x=X; sep; xs=optend(sep,X) {x::xs}
  | {[]}

// bloc of the form  NAME{CONTENT}
%public bloc(NAME,CONTENT):
  | NAME content=brace(CONTENT) {content}

// bloc of the form : NAME{CONTENT;CONTENT ...} with optional ';' at the end
%public bloc_list(NAME,CONTENT):
  | NAME content=brace(optend(SCOLON,CONTENT)) {content}

file:
  loption(constants)
  domains=bloc_list(INIT,domains)
  o=option(objective)
  constr=constraints
  s=option(solutions)
  EOF
  {
    {
      init=domains;
      objective=o;
      constraints=constr;
      solutions=s;
    }
  }

domains:
 | typ TOK_id ASSIGN d=bracket(init) {($1, $2, d)}

objective:
 | OBJ o=brace(expr) {o}

constraints:
 | CONSTR b=brace(optend(SCOLON, bexpr)) {b}

solutions:
 | SOL i=brace(optend(SCOLON,sols)) {Known i}
 | SOL brace(NONE) {Unfeasible}

sols:
  | b=boption(NOT) i=brace(optend(SCOLON,separated_pair(TOK_id,ASSIGN,const))) {VarMap.of_list i,b}

constants:
  | CST c=brace(optend(SCOLON,separated_pair(TOK_id,ASSIGN,value))) {c}

value:
  | LBRACKET rational SCOLON rational RBRACKET {($2, $4)}
  | rational {($1, $1)}

rational:
  | const DIVIDE const {Mpqf.div $1 $3}
  | const {$1}

typ: INT {Int} | REAL {Real}

init:
  | MINUS INF SCOLON option(PLUS) INF {Top}
  | MINUS INF SCOLON rational         {Minf ($4)}
  | rational SCOLON option(PLUS) INF  {Inf ($1)}
  | rational SCOLON rational          {Finite($1,$3)}

const:
  | TOK_const {$1}
  | MINUS TOK_const {(Mpqf.neg $2)}

bexpr:
  | expr cmp expr                                 { Cmp ($1, $2, $3) }
  | bexpr OR bexpr                                { Or ($1,$3) }
  | bexpr AND bexpr                               { And ($1,$3) }
  | NOT bexpr                                     { Not $2}
  | expr IN LBRACKET expr SCOLON expr RBRACKET    { inside $1 $4 $6 }
  | expr NOTIN LBRACKET expr SCOLON expr RBRACKET { outside $1 $4 $6 }
  | LPAREN bexpr RPAREN                           { $2 }

expr:
  | i=TOK_id LPAREN a=separated_list(COMMA,expr) RPAREN    { Funcall (i,a) }
  | LPAREN expr RPAREN           { $2 }
  | binop_expr                   { $1 }
  | MINUS expr %prec unary_minus { Neg $2 }
  | TOK_const                    { Cst $1 }
  | TOK_id                       { Var $1 }

binop_expr:
  | expr POW expr      {Binary(POW,$1,$3)}
  | expr DIVIDE expr   {Binary(DIV,$1,$3)}
  | expr MULTIPLY expr {Binary(MUL,$1,$3)}
  | expr PLUS expr     {Binary(ADD,$1,$3)}
  | expr MINUS expr    {Binary(SUB,$1,$3)}

cmp:
  | LESS          { LT }
  | GREATER       { GT }
  | LESS_EQUAL    { LEQ }
  | GREATER_EQUAL { GEQ }
  | ASSIGN        { EQ }
  | NOT_EQUAL     { NEQ }
