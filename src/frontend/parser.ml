type token =
  | TOK_LBRACE
  | TOK_RBRACE
  | TOK_LBRACKET
  | TOK_RBRACKET
  | TOK_LPAREN
  | TOK_RPAREN
  | TOK_COMMA
  | TOK_SEMICOLON
  | TOK_COLON
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
  | TOK_ANNOT
  | TOK_DRAW
  | TOK_MINF
  | TOK_INF
  | TOK_id of (string)
  | TOK_const of (float)
  | TOK_EOF

open Parsing;;
let _ = parse_error;;
# 2 "src/frontend/parser.mly"
open Syntax
# 46 "src/frontend/parser.ml"
let yytransl_const = [|
  257 (* TOK_LBRACE *);
  258 (* TOK_RBRACE *);
  259 (* TOK_LBRACKET *);
  260 (* TOK_RBRACKET *);
  261 (* TOK_LPAREN *);
  262 (* TOK_RPAREN *);
  263 (* TOK_COMMA *);
  264 (* TOK_SEMICOLON *);
  265 (* TOK_COLON *);
  266 (* TOK_PLUS *);
  267 (* TOK_MINUS *);
  268 (* TOK_MULTIPLY *);
  269 (* TOK_DIVIDE *);
  270 (* TOK_POW *);
  271 (* TOK_LESS *);
  272 (* TOK_GREATER *);
  273 (* TOK_LESS_EQUAL *);
  274 (* TOK_GREATER_EQUAL *);
  275 (* TOK_EQUAL_EQUAL *);
  276 (* TOK_NOT_EQUAL *);
  277 (* TOK_ASSIGN *);
  278 (* TOK_AND *);
  279 (* TOK_OR *);
  280 (* TOK_NOT *);
  281 (* TOK_INT *);
  282 (* TOK_REAL *);
  283 (* TOK_COS *);
  284 (* TOK_SIN *);
  285 (* TOK_SQRT *);
  286 (* TOK_INIT *);
  287 (* TOK_CONSTR *);
  288 (* TOK_ANNOT *);
  289 (* TOK_DRAW *);
  290 (* TOK_MINF *);
  291 (* TOK_INF *);
  294 (* TOK_EOF *);
    0|]

let yytransl_block = [|
  292 (* TOK_id *);
  293 (* TOK_const *);
    0|]

let yylhs = "\255\255\
\005\000\006\000\007\000\007\000\008\000\008\000\001\000\001\000\
\003\000\002\000\002\000\002\000\002\000\009\000\009\000\004\000\
\004\000\004\000\004\000\004\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\013\000\013\000\012\000\012\000\014\000\
\014\000\014\000\015\000\015\000\011\000\011\000\011\000\011\000\
\011\000\011\000\000\000"

let yylen = "\002\000\
\013\000\005\000\002\000\000\000\003\000\000\000\001\000\001\000\
\005\000\005\000\005\000\005\000\005\000\001\000\002\000\003\000\
\003\000\003\000\002\000\003\000\003\000\001\000\002\000\002\000\
\002\000\002\000\001\000\001\000\001\000\003\000\001\000\003\000\
\003\000\001\000\003\000\003\000\001\000\001\000\001\000\001\000\
\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\043\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\002\000\007\000\008\000\
\000\000\000\000\000\000\000\000\003\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\009\000\
\000\000\000\000\000\000\000\000\000\000\000\000\029\000\028\000\
\000\000\000\000\000\000\022\000\027\000\031\000\034\000\015\000\
\000\000\000\000\000\000\000\000\000\000\000\000\019\000\024\000\
\025\000\026\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\037\000\038\000\039\000\040\000\042\000\
\041\000\000\000\000\000\000\000\000\000\000\000\020\000\021\000\
\000\000\005\000\018\000\017\000\001\000\000\000\000\000\000\000\
\000\000\030\000\000\000\010\000\011\000\012\000\013\000"

let yydgoto = "\002\000\
\017\000\026\000\018\000\041\000\004\000\007\000\019\000\042\000\
\031\000\043\000\074\000\044\000\045\000\046\000\047\000"

let yysindex = "\005\000\
\232\254\000\000\009\255\000\000\238\254\010\255\024\255\249\254\
\019\255\056\255\050\255\041\255\248\254\000\000\000\000\000\000\
\046\255\248\254\065\255\067\255\000\000\059\255\092\255\105\255\
\247\254\100\255\011\255\076\255\110\255\000\000\116\255\000\000\
\011\255\016\255\011\255\016\255\016\255\016\255\000\000\000\000\
\146\255\098\255\160\255\000\000\000\000\000\000\000\000\000\000\
\031\255\035\255\001\255\044\255\016\255\112\255\000\000\000\000\
\000\000\000\000\011\255\011\255\011\255\093\255\016\255\016\255\
\016\255\016\255\016\255\000\000\000\000\000\000\000\000\000\000\
\000\000\016\255\132\255\138\255\140\255\144\255\000\000\000\000\
\173\255\000\000\000\000\000\000\000\000\000\255\000\255\112\255\
\112\255\000\000\178\255\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\157\255\000\000\000\000\000\000\
\000\000\157\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\158\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\063\255\000\000\000\000\
\000\000\000\000\158\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\117\255\135\255\081\255\
\099\255\000\000\141\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\232\255\000\000\000\000\143\000\103\000\
\080\000\223\255\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 192
let yytable = "\052\000\
\054\000\028\000\056\000\057\000\058\000\001\000\079\000\003\000\
\051\000\005\000\055\000\065\000\066\000\067\000\006\000\033\000\
\015\000\016\000\008\000\081\000\053\000\034\000\060\000\061\000\
\029\000\009\000\034\000\030\000\010\000\086\000\087\000\088\000\
\089\000\090\000\035\000\083\000\084\000\036\000\037\000\038\000\
\091\000\028\000\036\000\037\000\038\000\028\000\039\000\040\000\
\011\000\080\000\013\000\039\000\040\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\070\000\071\000\012\000\072\000\
\073\000\075\000\022\000\030\000\023\000\077\000\023\000\030\000\
\023\000\023\000\023\000\023\000\014\000\023\000\023\000\023\000\
\023\000\020\000\023\000\023\000\023\000\023\000\033\000\023\000\
\033\000\024\000\033\000\033\000\033\000\033\000\025\000\033\000\
\033\000\033\000\033\000\062\000\033\000\033\000\033\000\033\000\
\032\000\027\000\032\000\032\000\032\000\032\000\032\000\032\000\
\048\000\032\000\032\000\032\000\032\000\049\000\032\000\032\000\
\032\000\032\000\035\000\050\000\035\000\067\000\035\000\035\000\
\076\000\078\000\085\000\035\000\035\000\035\000\035\000\092\000\
\035\000\035\000\035\000\035\000\036\000\093\000\036\000\094\000\
\036\000\036\000\016\000\095\000\016\000\036\000\036\000\036\000\
\036\000\059\000\036\000\036\000\036\000\036\000\004\000\006\000\
\021\000\082\000\016\000\016\000\000\000\000\000\000\000\060\000\
\061\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\070\000\071\000\080\000\072\000\073\000\000\000\063\000\064\000\
\065\000\066\000\067\000\063\000\064\000\065\000\066\000\067\000"

let yycheck = "\033\000\
\034\000\011\001\036\000\037\000\038\000\001\000\006\001\032\001\
\033\000\001\001\035\000\012\001\013\001\014\001\033\001\005\001\
\025\001\026\001\009\001\053\000\005\001\011\001\022\001\023\001\
\034\001\002\001\011\001\037\001\036\001\063\000\064\000\065\000\
\066\000\067\000\024\001\060\000\061\000\027\001\028\001\029\001\
\074\000\011\001\027\001\028\001\029\001\011\001\036\001\037\001\
\030\001\006\001\001\001\036\001\037\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\007\001\020\001\
\021\001\035\001\002\001\037\001\006\001\035\001\008\001\037\001\
\010\001\011\001\012\001\013\001\036\001\015\001\016\001\017\001\
\018\001\036\001\020\001\021\001\022\001\023\001\006\001\021\001\
\008\001\031\001\010\001\011\001\012\001\013\001\003\001\015\001\
\016\001\017\001\018\001\002\001\020\001\021\001\022\001\023\001\
\006\001\001\001\008\001\008\001\010\001\011\001\012\001\013\001\
\037\001\015\001\016\001\017\001\018\001\008\001\020\001\021\001\
\022\001\023\001\006\001\008\001\008\001\014\001\010\001\011\001\
\049\000\050\000\038\001\015\001\016\001\017\001\018\001\004\001\
\020\001\021\001\022\001\023\001\006\001\004\001\008\001\004\001\
\010\001\011\001\006\001\004\001\008\001\015\001\016\001\017\001\
\018\001\008\001\020\001\021\001\022\001\023\001\002\001\002\001\
\018\000\059\000\022\001\023\001\255\255\255\255\255\255\022\001\
\023\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\006\001\020\001\021\001\255\255\010\001\011\001\
\012\001\013\001\014\001\010\001\011\001\012\001\013\001\014\001"

let yynames_const = "\
  TOK_LBRACE\000\
  TOK_RBRACE\000\
  TOK_LBRACKET\000\
  TOK_RBRACKET\000\
  TOK_LPAREN\000\
  TOK_RPAREN\000\
  TOK_COMMA\000\
  TOK_SEMICOLON\000\
  TOK_COLON\000\
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
  TOK_ANNOT\000\
  TOK_DRAW\000\
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
    let _3 = (Parsing.peek_val __caml_parser_env 10 : 'annot) in
    let _7 = (Parsing.peek_val __caml_parser_env 6 : 'decls) in
    let _11 = (Parsing.peek_val __caml_parser_env 2 : 'bexprs) in
    Obj.repr(
# 72 "src/frontend/parser.mly"
  ({init=_7; constraints=_11;to_draw=_3})
# 263 "src/frontend/parser.ml"
               : Syntax.prog))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 75 "src/frontend/parser.mly"
                                              (Some (_3,_5))
# 271 "src/frontend/parser.ml"
               : 'annot))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : assign) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 78 "src/frontend/parser.mly"
               (_1::_2)
# 279 "src/frontend/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "src/frontend/parser.mly"
    ([])
# 285 "src/frontend/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexprs) in
    Obj.repr(
# 82 "src/frontend/parser.mly"
                               (_1::_3)
# 293 "src/frontend/parser.ml"
               : 'bexprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "src/frontend/parser.mly"
    ([])
# 299 "src/frontend/parser.ml"
               : 'bexprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "src/frontend/parser.mly"
                  (INT)
# 305 "src/frontend/parser.ml"
               : typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "src/frontend/parser.mly"
                  (REAL)
# 311 "src/frontend/parser.ml"
               : typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : dom) in
    Obj.repr(
# 91 "src/frontend/parser.mly"
    ( (_1, _2, _4) )
# 320 "src/frontend/parser.ml"
               : assign))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "src/frontend/parser.mly"
                                                                 (Top)
# 326 "src/frontend/parser.ml"
               : dom))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'const) in
    Obj.repr(
# 95 "src/frontend/parser.mly"
                                                                 (Minf (_4))
# 333 "src/frontend/parser.ml"
               : dom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'const) in
    Obj.repr(
# 96 "src/frontend/parser.mly"
                                                                 (Inf (_2))
# 340 "src/frontend/parser.ml"
               : dom))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'const) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'const) in
    Obj.repr(
# 97 "src/frontend/parser.mly"
                                                                 (Finite(_2,_4))
# 348 "src/frontend/parser.ml"
               : dom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 100 "src/frontend/parser.mly"
              (_1)
# 355 "src/frontend/parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 101 "src/frontend/parser.mly"
                        ((-._2))
# 362 "src/frontend/parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "src/frontend/parser.mly"
                                        (Cmp (_2, _1, _3))
# 371 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : bexpr) in
    Obj.repr(
# 105 "src/frontend/parser.mly"
                                        (Or (_1,_3))
# 379 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : bexpr) in
    Obj.repr(
# 106 "src/frontend/parser.mly"
                                        (And (_1,_3))
# 387 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : bexpr) in
    Obj.repr(
# 107 "src/frontend/parser.mly"
                                        (Not (_2))
# 394 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : bexpr) in
    Obj.repr(
# 108 "src/frontend/parser.mly"
                                        ( _2 )
# 401 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 112 "src/frontend/parser.mly"
                                        ( _2 )
# 408 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 113 "src/frontend/parser.mly"
                                        ( _1 )
# 415 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "src/frontend/parser.mly"
                                        ( Unary (NEG, _2) )
# 422 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "src/frontend/parser.mly"
                                        ( Unary (COS, _2) )
# 429 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "src/frontend/parser.mly"
                                        ( Unary (SIN, _2) )
# 436 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "src/frontend/parser.mly"
                                        ( Unary (SQRT,_2) )
# 443 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'leaf) in
    Obj.repr(
# 118 "src/frontend/parser.mly"
                                        ( _1 )
# 450 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 121 "src/frontend/parser.mly"
                                        ( Cst (_1) )
# 457 "src/frontend/parser.ml"
               : 'leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 122 "src/frontend/parser.mly"
                                        ( Var _1 )
# 464 "src/frontend/parser.ml"
               : 'leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "src/frontend/parser.mly"
                       (Binary (POW,_1,_3))
# 472 "src/frontend/parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr2) in
    Obj.repr(
# 126 "src/frontend/parser.mly"
                       (_1)
# 479 "src/frontend/parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "src/frontend/parser.mly"
                            (Binary(DIV,_1,_3))
# 487 "src/frontend/parser.ml"
               : 'binop_expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 130 "src/frontend/parser.mly"
                            (Binary(MUL,_1,_3))
# 495 "src/frontend/parser.ml"
               : 'binop_expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr3) in
    Obj.repr(
# 131 "src/frontend/parser.mly"
                            (_1)
# 502 "src/frontend/parser.ml"
               : 'binop_expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 134 "src/frontend/parser.mly"
                        (Binary(ADD,_1,_3))
# 510 "src/frontend/parser.ml"
               : 'binop_expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 135 "src/frontend/parser.mly"
                        (Binary(SUB,_1,_3))
# 518 "src/frontend/parser.ml"
               : 'binop_expr3))
; (fun __caml_parser_env ->
    Obj.repr(
# 138 "src/frontend/parser.mly"
                                ( LT )
# 524 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 139 "src/frontend/parser.mly"
                                ( GT )
# 530 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 140 "src/frontend/parser.mly"
                                ( LEQ )
# 536 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 141 "src/frontend/parser.mly"
                                ( GEQ )
# 542 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 142 "src/frontend/parser.mly"
                                ( EQ )
# 548 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 143 "src/frontend/parser.mly"
                                ( NEQ )
# 554 "src/frontend/parser.ml"
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
