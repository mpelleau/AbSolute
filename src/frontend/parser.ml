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

open Parsing;;
let _ = parse_error;;
# 2 "src/frontend/parser.mly"
open Syntax
# 43 "src/frontend/parser.ml"
let yytransl_const = [|
  257 (* TOK_LBRACE *);
  258 (* TOK_RBRACE *);
  259 (* TOK_LBRACKET *);
  260 (* TOK_RBRACKET *);
  261 (* TOK_LPAREN *);
  262 (* TOK_RPAREN *);
  263 (* TOK_COMMA *);
  264 (* TOK_SEMICOLON *);
  265 (* TOK_PLUS *);
  266 (* TOK_MINUS *);
  267 (* TOK_MULTIPLY *);
  268 (* TOK_DIVIDE *);
  269 (* TOK_POW *);
  270 (* TOK_LESS *);
  271 (* TOK_GREATER *);
  272 (* TOK_LESS_EQUAL *);
  273 (* TOK_GREATER_EQUAL *);
  274 (* TOK_EQUAL_EQUAL *);
  275 (* TOK_NOT_EQUAL *);
  276 (* TOK_ASSIGN *);
  277 (* TOK_AND *);
  278 (* TOK_OR *);
  279 (* TOK_NOT *);
  280 (* TOK_INT *);
  281 (* TOK_REAL *);
  282 (* TOK_COS *);
  283 (* TOK_SIN *);
  284 (* TOK_SQRT *);
  285 (* TOK_INIT *);
  286 (* TOK_CONSTR *);
  287 (* TOK_MINF *);
  288 (* TOK_INF *);
  291 (* TOK_EOF *);
    0|]

let yytransl_block = [|
  289 (* TOK_id *);
  290 (* TOK_const *);
    0|]

let yylhs = "\255\255\
\005\000\006\000\006\000\007\000\007\000\001\000\001\000\003\000\
\002\000\002\000\002\000\002\000\008\000\008\000\004\000\004\000\
\004\000\004\000\004\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\012\000\012\000\011\000\011\000\013\000\013\000\
\013\000\014\000\014\000\010\000\010\000\010\000\010\000\010\000\
\010\000\000\000"

let yylen = "\002\000\
\009\000\002\000\000\000\003\000\000\000\001\000\001\000\005\000\
\005\000\005\000\005\000\005\000\001\000\002\000\003\000\003\000\
\003\000\002\000\003\000\003\000\001\000\002\000\002\000\002\000\
\002\000\001\000\001\000\001\000\003\000\001\000\003\000\003\000\
\001\000\003\000\003\000\001\000\001\000\001\000\001\000\001\000\
\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\042\000\000\000\006\000\007\000\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\013\000\000\000\008\000\000\000\
\000\000\000\000\000\000\000\000\000\000\028\000\027\000\000\000\
\000\000\000\000\021\000\026\000\030\000\033\000\014\000\000\000\
\000\000\000\000\000\000\000\000\000\000\018\000\023\000\024\000\
\025\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\037\000\038\000\039\000\041\000\040\000\
\000\000\000\000\000\000\000\000\000\000\019\000\020\000\000\000\
\004\000\017\000\016\000\001\000\000\000\000\000\000\000\000\000\
\029\000\000\000\009\000\010\000\011\000\012\000"

let yydgoto = "\002\000\
\008\000\017\000\009\000\032\000\004\000\010\000\033\000\022\000\
\034\000\065\000\035\000\036\000\037\000\038\000"

let yysindex = "\011\000\
\241\254\000\000\024\255\000\000\238\254\000\000\000\000\021\255\
\238\254\053\255\045\255\000\000\038\255\067\255\071\255\017\255\
\065\255\012\255\048\255\079\255\000\000\085\255\000\000\012\255\
\016\255\012\255\016\255\016\255\016\255\000\000\000\000\250\254\
\097\255\151\255\000\000\000\000\000\000\000\000\000\000\037\255\
\042\255\007\255\047\255\016\255\091\255\000\000\000\000\000\000\
\000\000\012\255\012\255\012\255\075\255\016\255\016\255\016\255\
\016\255\016\255\000\000\000\000\000\000\000\000\000\000\000\000\
\016\255\112\255\117\255\123\255\127\255\000\000\000\000\163\255\
\000\000\000\000\000\000\000\000\171\255\171\255\091\255\091\255\
\000\000\168\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\130\255\000\000\000\000\000\000\
\130\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\131\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\069\255\000\000\000\000\000\000\
\000\000\131\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\120\255\137\255\086\255\103\255\
\000\000\002\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\241\255\000\000\129\000\094\000\234\255\
\232\255\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 184
let yytable = "\043\000\
\045\000\050\000\047\000\048\000\049\000\006\000\007\000\015\000\
\042\000\015\000\046\000\001\000\070\000\003\000\051\000\052\000\
\024\000\067\000\069\000\072\000\044\000\025\000\015\000\015\000\
\005\000\025\000\019\000\051\000\052\000\077\000\078\000\079\000\
\080\000\081\000\026\000\074\000\075\000\027\000\028\000\029\000\
\082\000\027\000\028\000\029\000\030\000\031\000\019\000\020\000\
\030\000\031\000\021\000\019\000\071\000\011\000\013\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\014\000\063\000\064\000\015\000\066\000\016\000\021\000\018\000\
\023\000\068\000\022\000\021\000\022\000\022\000\022\000\022\000\
\022\000\039\000\022\000\022\000\022\000\022\000\040\000\022\000\
\022\000\022\000\022\000\032\000\041\000\032\000\032\000\032\000\
\032\000\032\000\053\000\032\000\032\000\032\000\032\000\058\000\
\032\000\032\000\032\000\032\000\031\000\076\000\031\000\031\000\
\031\000\031\000\031\000\083\000\031\000\031\000\031\000\031\000\
\084\000\031\000\031\000\031\000\031\000\034\000\085\000\034\000\
\034\000\034\000\086\000\003\000\005\000\034\000\034\000\034\000\
\034\000\012\000\034\000\034\000\034\000\034\000\035\000\073\000\
\035\000\035\000\035\000\000\000\000\000\000\000\035\000\035\000\
\035\000\035\000\000\000\035\000\035\000\035\000\035\000\054\000\
\055\000\056\000\057\000\058\000\059\000\060\000\061\000\062\000\
\071\000\063\000\064\000\054\000\055\000\056\000\057\000\058\000\
\054\000\055\000\056\000\057\000\058\000\056\000\057\000\058\000"

let yycheck = "\024\000\
\025\000\008\001\027\000\028\000\029\000\024\001\025\001\006\001\
\024\000\008\001\026\000\001\000\006\001\029\001\021\001\022\001\
\005\001\040\000\041\000\044\000\005\001\010\001\021\001\022\001\
\001\001\010\001\010\001\021\001\022\001\054\000\055\000\056\000\
\057\000\058\000\023\001\051\000\052\000\026\001\027\001\028\001\
\065\000\026\001\027\001\028\001\033\001\034\001\010\001\031\001\
\033\001\034\001\034\001\010\001\006\001\033\001\002\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\020\001\019\001\020\001\030\001\032\001\003\001\034\001\001\001\
\008\001\032\001\006\001\034\001\008\001\009\001\010\001\011\001\
\012\001\034\001\014\001\015\001\016\001\017\001\008\001\019\001\
\020\001\021\001\022\001\006\001\008\001\008\001\009\001\010\001\
\011\001\012\001\002\001\014\001\015\001\016\001\017\001\013\001\
\019\001\020\001\021\001\022\001\006\001\035\001\008\001\009\001\
\010\001\011\001\012\001\004\001\014\001\015\001\016\001\017\001\
\004\001\019\001\020\001\021\001\022\001\006\001\004\001\008\001\
\009\001\010\001\004\001\002\001\002\001\014\001\015\001\016\001\
\017\001\009\000\019\001\020\001\021\001\022\001\006\001\050\000\
\008\001\009\001\010\001\255\255\255\255\255\255\014\001\015\001\
\016\001\017\001\255\255\019\001\020\001\021\001\022\001\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\006\001\019\001\020\001\009\001\010\001\011\001\012\001\013\001\
\009\001\010\001\011\001\012\001\013\001\011\001\012\001\013\001"

let yynames_const = "\
  TOK_LBRACE\000\
  TOK_RBRACE\000\
  TOK_LBRACKET\000\
  TOK_RBRACKET\000\
  TOK_LPAREN\000\
  TOK_RPAREN\000\
  TOK_COMMA\000\
  TOK_SEMICOLON\000\
  TOK_PLUS\000\
  TOK_MINUS\000\
  TOK_MULTIPLY\000\
  TOK_DIVIDE\000\
  TOK_POW\000\
  TOK_LESS\000\
  TOK_GREATER\000\
  TOK_LESS_EQUAL\000\
  TOK_GREATER_EQUAL\000\
  TOK_EQUAL_EQUAL\000\
  TOK_NOT_EQUAL\000\
  TOK_ASSIGN\000\
  TOK_AND\000\
  TOK_OR\000\
  TOK_NOT\000\
  TOK_INT\000\
  TOK_REAL\000\
  TOK_COS\000\
  TOK_SIN\000\
  TOK_SQRT\000\
  TOK_INIT\000\
  TOK_CONSTR\000\
  TOK_MINF\000\
  TOK_INF\000\
  TOK_EOF\000\
  "

let yynames_block = "\
  TOK_id\000\
  TOK_const\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'decls) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'bexprs) in
    Obj.repr(
# 70 "src/frontend/parser.mly"
  ({init=_3; constraints=_7})
# 248 "src/frontend/parser.ml"
               : Syntax.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : assign) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 73 "src/frontend/parser.mly"
               (_1::_2)
# 256 "src/frontend/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "src/frontend/parser.mly"
    ([])
# 262 "src/frontend/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexprs) in
    Obj.repr(
# 77 "src/frontend/parser.mly"
                               (_1::_3)
# 270 "src/frontend/parser.ml"
               : 'bexprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "src/frontend/parser.mly"
    ([])
# 276 "src/frontend/parser.ml"
               : 'bexprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "src/frontend/parser.mly"
                  (INT)
# 282 "src/frontend/parser.ml"
               : typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "src/frontend/parser.mly"
                  (REAL)
# 288 "src/frontend/parser.ml"
               : typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : dom) in
    Obj.repr(
# 86 "src/frontend/parser.mly"
    ( (_1, _2, _4) )
# 297 "src/frontend/parser.ml"
               : assign))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "src/frontend/parser.mly"
                                                                 (Top)
# 303 "src/frontend/parser.ml"
               : dom))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'const) in
    Obj.repr(
# 90 "src/frontend/parser.mly"
                                                                 (Minf (_4))
# 310 "src/frontend/parser.ml"
               : dom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'const) in
    Obj.repr(
# 91 "src/frontend/parser.mly"
                                                                 (Inf (_2))
# 317 "src/frontend/parser.ml"
               : dom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'const) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'const) in
    Obj.repr(
# 92 "src/frontend/parser.mly"
                                                                 (Finite(_2,_4))
# 325 "src/frontend/parser.ml"
               : dom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 95 "src/frontend/parser.mly"
              (_1)
# 332 "src/frontend/parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 96 "src/frontend/parser.mly"
                        ((-._2))
# 339 "src/frontend/parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "src/frontend/parser.mly"
                                        (Cmp (_2, _1, _3))
# 348 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : bexpr) in
    Obj.repr(
# 100 "src/frontend/parser.mly"
                                        (Or (_1,_3))
# 356 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : bexpr) in
    Obj.repr(
# 101 "src/frontend/parser.mly"
                                        (And (_1,_3))
# 364 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : bexpr) in
    Obj.repr(
# 102 "src/frontend/parser.mly"
                                        (Not (_2))
# 371 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : bexpr) in
    Obj.repr(
# 103 "src/frontend/parser.mly"
                                        ( _2 )
# 378 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 107 "src/frontend/parser.mly"
                                        ( _2 )
# 385 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 108 "src/frontend/parser.mly"
                                        ( _1 )
# 392 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "src/frontend/parser.mly"
                                        ( Unary (NEG, _2) )
# 399 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "src/frontend/parser.mly"
                                        ( Unary (COS, _2) )
# 406 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "src/frontend/parser.mly"
                                        ( Unary (SIN, _2) )
# 413 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "src/frontend/parser.mly"
                                        ( Unary (SQRT,_2) )
# 420 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'leaf) in
    Obj.repr(
# 113 "src/frontend/parser.mly"
                                        ( _1 )
# 427 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 116 "src/frontend/parser.mly"
                                        ( Cst (_1) )
# 434 "src/frontend/parser.ml"
               : 'leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 117 "src/frontend/parser.mly"
                                        ( Var _1 )
# 441 "src/frontend/parser.ml"
               : 'leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "src/frontend/parser.mly"
                       (Binary (POW,_1,_3))
# 449 "src/frontend/parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr2) in
    Obj.repr(
# 121 "src/frontend/parser.mly"
                       (_1)
# 456 "src/frontend/parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "src/frontend/parser.mly"
                            (Binary(DIV,_1,_3))
# 464 "src/frontend/parser.ml"
               : 'binop_expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "src/frontend/parser.mly"
                            (Binary(MUL,_1,_3))
# 472 "src/frontend/parser.ml"
               : 'binop_expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr3) in
    Obj.repr(
# 126 "src/frontend/parser.mly"
                            (_1)
# 479 "src/frontend/parser.ml"
               : 'binop_expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "src/frontend/parser.mly"
                        (Binary(ADD,_1,_3))
# 487 "src/frontend/parser.ml"
               : 'binop_expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "src/frontend/parser.mly"
                        (Binary(SUB,_1,_3))
# 495 "src/frontend/parser.ml"
               : 'binop_expr3))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "src/frontend/parser.mly"
                                ( LT )
# 501 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "src/frontend/parser.mly"
                                ( GT )
# 507 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "src/frontend/parser.mly"
                                ( LEQ )
# 513 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "src/frontend/parser.mly"
                                ( GEQ )
# 519 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "src/frontend/parser.mly"
                                ( EQ )
# 525 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "src/frontend/parser.mly"
                                ( NEQ )
# 531 "src/frontend/parser.ml"
               : 'cmp))
(* Entry file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.prog)
