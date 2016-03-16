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
  | TOK_id of (string)
  | TOK_const of (float)
  | TOK_EOF

open Parsing;;
let _ = parse_error;;
# 2 "frontend/parser.mly"
open Syntax
# 41 "frontend/parser.ml"
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
  289 (* TOK_EOF *);
    0|]

let yytransl_block = [|
  287 (* TOK_id *);
  288 (* TOK_const *);
    0|]

let yylhs = "\255\255\
\005\000\006\000\006\000\007\000\007\000\001\000\001\000\003\000\
\002\000\004\000\004\000\004\000\004\000\004\000\008\000\008\000\
\008\000\008\000\008\000\008\000\008\000\011\000\011\000\010\000\
\010\000\013\000\013\000\013\000\014\000\014\000\012\000\009\000\
\009\000\009\000\009\000\009\000\009\000\000\000"

let yylen = "\002\000\
\005\000\002\000\000\000\002\000\000\000\001\000\001\000\005\000\
\005\000\003\000\003\000\003\000\002\000\003\000\001\000\001\000\
\002\000\002\000\002\000\001\000\001\000\003\000\003\000\003\000\
\001\000\003\000\003\000\001\000\003\000\003\000\002\000\001\000\
\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\038\000\006\000\007\000\000\000\000\000\
\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\020\000\000\000\000\000\000\000\016\000\
\021\000\025\000\028\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\000\000\017\000\018\000\019\000\000\000\000\000\
\004\000\001\000\000\000\000\000\000\000\000\000\000\000\032\000\
\033\000\034\000\035\000\036\000\037\000\000\000\000\000\008\000\
\000\000\014\000\022\000\023\000\000\000\012\000\000\000\000\000\
\000\000\000\000\000\000\024\000\000\000\000\000\000\000\009\000"

let yydgoto = "\002\000\
\007\000\029\000\008\000\021\000\004\000\009\000\022\000\023\000\
\054\000\024\000\025\000\033\000\026\000\027\000"

let yysindex = "\019\000\
\007\255\000\000\001\255\000\000\000\000\000\000\008\255\001\255\
\015\255\041\255\000\000\162\255\049\255\169\255\162\255\193\255\
\193\255\193\255\000\000\000\000\252\254\029\255\228\255\000\000\
\000\000\000\000\000\000\036\255\065\255\193\255\080\255\217\255\
\063\255\000\000\181\255\000\000\000\000\000\000\162\255\162\255\
\000\000\000\000\193\255\193\255\193\255\193\255\193\255\000\000\
\000\000\000\000\000\000\000\000\000\000\193\255\067\255\000\000\
\205\255\000\000\000\000\000\000\002\255\000\000\060\255\192\255\
\250\254\044\255\072\255\000\000\205\255\058\255\087\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\068\255\000\000\000\000\000\000\000\000\068\255\
\000\000\000\000\000\000\064\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\064\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\097\255\000\000\000\000\000\000\000\000\000\000\150\255\119\255\
\090\255\061\255\032\255\000\000\138\255\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\251\255\000\000\102\000\093\000\242\255\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 247
let yytable = "\032\000\
\014\000\036\000\037\000\038\000\045\000\046\000\047\000\059\000\
\031\000\034\000\043\000\044\000\045\000\046\000\047\000\057\000\
\039\000\040\000\015\000\001\000\061\000\016\000\017\000\018\000\
\005\000\006\000\019\000\020\000\064\000\065\000\066\000\067\000\
\068\000\062\000\063\000\003\000\026\000\026\000\010\000\069\000\
\026\000\026\000\026\000\026\000\012\000\026\000\026\000\026\000\
\026\000\026\000\026\000\028\000\026\000\026\000\026\000\046\000\
\047\000\026\000\026\000\026\000\013\000\042\000\026\000\026\000\
\026\000\027\000\027\000\055\000\060\000\027\000\027\000\027\000\
\056\000\070\000\027\000\027\000\027\000\027\000\027\000\027\000\
\039\000\027\000\027\000\027\000\047\000\058\000\027\000\027\000\
\027\000\071\000\072\000\027\000\027\000\027\000\030\000\030\000\
\005\000\003\000\030\000\030\000\039\000\040\000\031\000\030\000\
\030\000\030\000\030\000\030\000\030\000\011\000\030\000\030\000\
\030\000\041\000\000\000\030\000\030\000\030\000\000\000\000\000\
\030\000\030\000\030\000\029\000\029\000\000\000\000\000\029\000\
\000\000\000\000\000\000\000\000\029\000\029\000\029\000\029\000\
\029\000\029\000\000\000\029\000\029\000\029\000\010\000\010\000\
\029\000\029\000\029\000\000\000\000\000\029\000\029\000\029\000\
\000\000\000\000\011\000\011\000\000\000\000\000\010\000\010\000\
\010\000\000\000\000\000\010\000\010\000\010\000\014\000\000\000\
\010\000\010\000\010\000\011\000\011\000\014\000\000\000\011\000\
\011\000\011\000\030\000\000\000\011\000\011\000\011\000\000\000\
\015\000\035\000\000\000\016\000\017\000\018\000\030\000\015\000\
\019\000\020\000\016\000\017\000\018\000\035\000\000\000\019\000\
\020\000\044\000\045\000\046\000\047\000\000\000\016\000\017\000\
\018\000\000\000\000\000\019\000\020\000\043\000\044\000\045\000\
\046\000\047\000\016\000\017\000\018\000\000\000\059\000\019\000\
\020\000\043\000\044\000\045\000\046\000\047\000\048\000\049\000\
\050\000\051\000\052\000\053\000\043\000\044\000\045\000\046\000\
\047\000\048\000\049\000\050\000\051\000\052\000\053\000"

let yycheck = "\014\000\
\005\001\016\000\017\000\018\000\011\001\012\001\013\001\006\001\
\014\000\015\000\009\001\010\001\011\001\012\001\013\001\030\000\
\021\001\022\001\023\001\001\000\035\000\026\001\027\001\028\001\
\024\001\025\001\031\001\032\001\043\000\044\000\045\000\046\000\
\047\000\039\000\040\000\029\001\005\001\006\001\031\001\054\000\
\009\001\010\001\011\001\012\001\030\001\014\001\015\001\016\001\
\017\001\018\001\019\001\003\001\021\001\022\001\023\001\012\001\
\013\001\026\001\027\001\028\001\020\001\033\001\031\001\032\001\
\033\001\005\001\006\001\032\001\006\001\009\001\010\001\011\001\
\008\001\007\001\014\001\015\001\016\001\017\001\018\001\019\001\
\021\001\021\001\022\001\023\001\013\001\006\001\026\001\027\001\
\028\001\032\001\004\001\031\001\032\001\033\001\005\001\006\001\
\033\001\030\001\009\001\010\001\021\001\022\001\006\001\014\001\
\015\001\016\001\017\001\018\001\019\001\008\000\021\001\022\001\
\023\001\021\000\255\255\026\001\027\001\028\001\255\255\255\255\
\031\001\032\001\033\001\005\001\006\001\255\255\255\255\009\001\
\255\255\255\255\255\255\255\255\014\001\015\001\016\001\017\001\
\018\001\019\001\255\255\021\001\022\001\023\001\005\001\006\001\
\026\001\027\001\028\001\255\255\255\255\031\001\032\001\033\001\
\255\255\255\255\005\001\006\001\255\255\255\255\021\001\022\001\
\023\001\255\255\255\255\026\001\027\001\028\001\005\001\255\255\
\031\001\032\001\033\001\022\001\023\001\005\001\255\255\026\001\
\027\001\028\001\010\001\255\255\031\001\032\001\033\001\255\255\
\023\001\005\001\255\255\026\001\027\001\028\001\010\001\023\001\
\031\001\032\001\026\001\027\001\028\001\005\001\255\255\031\001\
\032\001\010\001\011\001\012\001\013\001\255\255\026\001\027\001\
\028\001\255\255\255\255\031\001\032\001\009\001\010\001\011\001\
\012\001\013\001\026\001\027\001\028\001\255\255\006\001\031\001\
\032\001\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\009\001\010\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\019\001"

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
  TOK_EOF\000\
  "

let yynames_block = "\
  TOK_id\000\
  TOK_const\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'decls) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'bexprs) in
    Obj.repr(
# 71 "frontend/parser.mly"
  ( {init=_2;constraints=_4} )
# 250 "frontend/parser.ml"
               : Syntax.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : assign) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 74 "frontend/parser.mly"
               (_1::_2)
# 258 "frontend/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "frontend/parser.mly"
    ([])
# 264 "frontend/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : bexpr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bexprs) in
    Obj.repr(
# 78 "frontend/parser.mly"
                 (_1::_2)
# 272 "frontend/parser.ml"
               : 'bexprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "frontend/parser.mly"
    ([])
# 278 "frontend/parser.ml"
               : 'bexprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "frontend/parser.mly"
                  (INT)
# 284 "frontend/parser.ml"
               : typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "frontend/parser.mly"
                  (REAL)
# 290 "frontend/parser.ml"
               : typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : dom) in
    Obj.repr(
# 87 "frontend/parser.mly"
    ( (_1, _2, _4) )
# 299 "frontend/parser.ml"
               : assign))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : float) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : float) in
    Obj.repr(
# 91 "frontend/parser.mly"
      ((_2,_4))
# 307 "frontend/parser.ml"
               : dom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "frontend/parser.mly"
                                        (Cmp (_2, _1, _3))
# 316 "frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : bexpr) in
    Obj.repr(
# 95 "frontend/parser.mly"
                                        (Or (_1,_3))
# 324 "frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : bexpr) in
    Obj.repr(
# 96 "frontend/parser.mly"
                                        (And (_1,_3))
# 332 "frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : bexpr) in
    Obj.repr(
# 97 "frontend/parser.mly"
                                        (Not (_2))
# 339 "frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : bexpr) in
    Obj.repr(
# 98 "frontend/parser.mly"
                                        ( _2 )
# 346 "frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 102 "frontend/parser.mly"
                                        ( Var _1 )
# 353 "frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 103 "frontend/parser.mly"
                                        ( _1 )
# 360 "frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "frontend/parser.mly"
                                        ( Unary (COS,_2) )
# 367 "frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "frontend/parser.mly"
                                        ( Unary (SIN,_2) )
# 374 "frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "frontend/parser.mly"
                                        ( Unary (SQRT,_2) )
# 381 "frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 107 "frontend/parser.mly"
                                        ( Cst (_1,_1) )
# 388 "frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parenthesized_expr) in
    Obj.repr(
# 108 "frontend/parser.mly"
                                        ( _1 )
# 395 "frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 111 "frontend/parser.mly"
                                              ( _2 )
# 402 "frontend/parser.ml"
               : 'parenthesized_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_with_prefix) in
    Obj.repr(
# 112 "frontend/parser.mly"
                                              ( _2 )
# 409 "frontend/parser.ml"
               : 'parenthesized_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "frontend/parser.mly"
                      (Binary (POW,_1,_3))
# 417 "frontend/parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr2) in
    Obj.repr(
# 117 "frontend/parser.mly"
                (_1)
# 424 "frontend/parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "frontend/parser.mly"
                         (Binary(DIV,_1,_3))
# 432 "frontend/parser.ml"
               : 'binop_expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "frontend/parser.mly"
                           (Binary(MUL,_1,_3))
# 440 "frontend/parser.ml"
               : 'binop_expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr3) in
    Obj.repr(
# 122 "frontend/parser.mly"
                (_1)
# 447 "frontend/parser.ml"
               : 'binop_expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "frontend/parser.mly"
                       (Binary(ADD,_1,_3))
# 455 "frontend/parser.ml"
               : 'binop_expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 126 "frontend/parser.mly"
                        (Binary(SUB,_1,_3))
# 463 "frontend/parser.ml"
               : 'binop_expr3))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 129 "frontend/parser.mly"
                   ( Unary (NEG,_2) )
# 470 "frontend/parser.ml"
               : 'expr_with_prefix))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "frontend/parser.mly"
                                ( LT )
# 476 "frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "frontend/parser.mly"
                                ( GT )
# 482 "frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "frontend/parser.mly"
                                ( LEQ )
# 488 "frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "frontend/parser.mly"
                                ( GEQ )
# 494 "frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 136 "frontend/parser.mly"
                                ( EQ )
# 500 "frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 137 "frontend/parser.mly"
                                ( NEQ )
# 506 "frontend/parser.ml"
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
