%{
open Tools
open Csp
open Csp_helper
%}


/* Keywords */
%token TOK_INT           /* int */
%token TOK_REAL          /* real */
%token TOK_CST           /* constants */
%token TOK_INIT          /* init */
%token TOK_OBJ           /* objective */
%token TOK_CONSTR        /* constraints */
%token TOK_SOL           /* solutions */
%token TOK_NONE          /* none */
%token TOK_MINF          /* -oo */
%token TOK_INF           /* oo */
%token TOK_IN            /* in */
%token TOK_NOTIN         /* notin */

/* Delimiters */
%token TOK_LBRACE        /* { */
%token TOK_RBRACE        /* } */
%token TOK_LBRACKET      /* [ */
%token TOK_RBRACKET      /* ] */
%token TOK_LPAREN        /* ( */
%token TOK_RPAREN        /* ) */

/* Operators */
%token TOK_COMMA         /* , */
%token TOK_SEMICOLON     /* ; */
%token TOK_PLUS          /* + */
%token TOK_MINUS         /* - */
%token TOK_MULTIPLY      /* * */
%token TOK_DIVIDE        /* / */
%token TOK_POW           /* ^ */
%token TOK_LESS          /* < */
%token TOK_GREATER       /* > */
%token TOK_LESS_EQUAL    /* <= */
%token TOK_GREATER_EQUAL /* >= */
%token TOK_NOT_EQUAL     /* != */
%token TOK_ASSIGN        /* = */
%token TOK_AND           /* && */
%token TOK_OR            /* || */
%token TOK_NOT           /* ! */

%token <string> TOK_id
%token <Mpqf.t> TOK_const

%token TOK_EOF

/* priorities */
%left TOK_OR TOK_AND
%nonassoc TOK_NOT
%left TOK_PLUS TOK_MINUS
%left TOK_MULTIPLY  TOK_DIVIDE
%nonassoc unary_minus

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
%public brace(YOURSELF):
  | TOK_LBRACE content=YOURSELF TOK_RBRACE { content }

// [x]
%public bracket(X):
  | TOK_LBRACKET content=X TOK_RBRACKET { content }

// [x;y]
%public itv(X,Y):
  | TOK_LBRACKET X TOK_SEMICOLON Y TOK_RBRACKET { content }

// separated_list with optional separator at the end
%public separated_optend(sep,X):
  | X {[$1]}
  | x=X; sep; xs=separated_optend(sep,X) {x::xs}
  | {[]}

// bloc of the form  NAME{CONTENT}
%public bloc(NAME,CONTENT):
  | NAME content=brace(CONTENT) {content}

// bloc of the form : NAME{CONTENT;CONTENT ...} with optional ';' at the end
%public bloc_list(NAME,CONTENT):
  | NAME content=brace(separated_optend(TOK_SEMICOLON,CONTENT)) {content}

file:
  constants
  domains=bloc_list(TOK_INIT,domains)
  objective
  constraints
  solutions
  TOK_EOF
  {
    {
      init=domains;
      objective=$3;
      constraints=$4;
      solutions=$5;
    }
  }

domains:
  | typ TOK_id TOK_ASSIGN init {($1, $2, $4)}

objective:
 | TOK_OBJ TOK_LBRACE expr TOK_RBRACE {$3}
 | {zero}

constraints:
 | TOK_CONSTR TOK_LBRACE bexprs TOK_RBRACE {$3}

solutions:
 | TOK_SOL TOK_LBRACE instances TOK_RBRACE {Some (Known $3)}
 | TOK_SOL TOK_LBRACE TOK_NONE TOK_RBRACE {Some Unfeasible}
 | {None}

instances:
 | TOK_LBRACE sols TOK_RBRACE TOK_SEMICOLON instances {((VarMap.of_list $2),true)::$5}
 | TOK_LBRACE sols TOK_RBRACE {[(VarMap.of_list $2),true]}
 | TOK_NOT TOK_LBRACE sols TOK_RBRACE TOK_SEMICOLON instances {((VarMap.of_list $3),false)::$6}
 | TOK_NOT TOK_LBRACE sols TOK_RBRACE {[(VarMap.of_list $3),false]}
 | {[]}

sols:
 | TOK_id TOK_ASSIGN const TOK_SEMICOLON sols {($1,$3)::$5}
 | TOK_id TOK_ASSIGN const {[($1,$3)]}
 | {[]}

constants:
  | TOK_CST TOK_LBRACE csts TOK_RBRACE {$3}
  | {[]}

csts:
  | TOK_id TOK_ASSIGN value TOK_SEMICOLON csts {($1, $3)::$5}
  | TOK_id TOK_ASSIGN value {[($1, $3)]}
  | {[]}

value:
  | TOK_LBRACKET rational TOK_SEMICOLON rational TOK_RBRACKET {($2, $4)}
  | rational {($1, $1)}

rational:
  | const TOK_DIVIDE const {Mpqf.div $1 $3}
  | const {$1}

bexprs:
  | bexpr TOK_SEMICOLON bexprs {$1::$3}
  | bexpr {[$1]}
  | {[]}

typ:
  | TOK_INT       {Int}
  | TOK_REAL      {Real}

init:
  | TOK_LBRACKET TOK_MINF TOK_SEMICOLON TOK_INF TOK_RBRACKET                   {Top}
  | TOK_LBRACKET TOK_MINUS TOK_INF TOK_SEMICOLON TOK_INF TOK_RBRACKET          {Top}
  | TOK_LBRACKET TOK_MINF TOK_SEMICOLON TOK_PLUS TOK_INF TOK_RBRACKET          {Top}
  | TOK_LBRACKET TOK_MINUS TOK_INF TOK_SEMICOLON TOK_PLUS TOK_INF TOK_RBRACKET {Top}
  | TOK_LBRACKET TOK_MINF TOK_SEMICOLON rational TOK_RBRACKET                  {Minf ($4)}
  | TOK_LBRACKET TOK_MINUS TOK_INF TOK_SEMICOLON rational TOK_RBRACKET         {Minf ($5)}
  | TOK_LBRACKET rational TOK_SEMICOLON TOK_INF TOK_RBRACKET                   {Inf ($2)}
  | TOK_LBRACKET rational TOK_SEMICOLON TOK_PLUS TOK_INF TOK_RBRACKET          {Inf ($2)}
  | TOK_LBRACKET rational TOK_SEMICOLON rational TOK_RBRACKET                  {Finite($2,$4)}
  | TOK_LBRACE consts TOK_RBRACE                                               {Set($2)}

consts:
  | rational TOK_SEMICOLON consts {$1::$3}
  | rational {[$1]}
  | {[]}

const:
  | TOK_const {$1}
  | TOK_MINUS TOK_const {(Mpqf.neg $2)}

bexpr:
  | expr cmp expr                                    {Cmp ($1, $2, $3)}
  | bexpr TOK_OR bexpr                               {Or ($1,$3)}
  | bexpr TOK_AND bexpr                              {And ($1,$3)}
  | TOK_NOT bexpr                                    {Not ($2)}
  | expr TOK_IN TOK_LBRACKET expr TOK_SEMICOLON expr TOK_RBRACKET    {inside $1 $4 $6 }
  | expr TOK_NOTIN TOK_LBRACKET expr TOK_SEMICOLON expr TOK_RBRACKET {outside $1 $4 $6 }
  | TOK_LPAREN bexpr TOK_RPAREN                      { $2 }

expr:
  | TOK_id TOK_LPAREN args TOK_RPAREN   { Funcall ($1,$3) }
  | TOK_LPAREN expr TOK_RPAREN          { $2 }
  | binop_expr                          { $1 }
  | TOK_MINUS expr %prec unary_minus    { Neg $2 }
  | leaf                                { $1 }

args:
  | expr                {[$1]}
  | expr TOK_COMMA args {$1::$3}

leaf:
  | TOK_const                           { Cst $1 }
  | TOK_id                              { Var $1 }

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

cmp:
  | TOK_LESS                    { LT }
  | TOK_GREATER                 { GT }
  | TOK_LESS_EQUAL              { LEQ }
  | TOK_GREATER_EQUAL           { GEQ }
  | TOK_ASSIGN                  { EQ }
  | TOK_NOT_EQUAL               { NEQ }
