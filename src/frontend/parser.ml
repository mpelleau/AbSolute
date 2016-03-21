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
# 2 "src/frontend/parser.mly"
open Syntax
# 41 "src/frontend/parser.ml"
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
\008\000\008\000\008\000\008\000\012\000\012\000\010\000\010\000\
\011\000\011\000\013\000\013\000\013\000\014\000\014\000\009\000\
\009\000\009\000\009\000\009\000\009\000\000\000"

let yylen = "\002\000\
\009\000\002\000\000\000\003\000\000\000\001\000\001\000\005\000\
\005\000\003\000\003\000\003\000\002\000\003\000\001\000\001\000\
\002\000\002\000\002\000\001\000\001\000\001\000\003\000\004\000\
\003\000\001\000\003\000\003\000\001\000\003\000\003\000\001\000\
\001\000\001\000\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\038\000\000\000\006\000\007\000\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\000\
\000\000\022\000\021\000\000\000\000\000\000\000\015\000\016\000\
\020\000\026\000\029\000\000\000\000\000\000\000\000\000\000\000\
\000\000\017\000\018\000\019\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\032\000\033\000\034\000\
\035\000\036\000\037\000\000\000\000\000\000\000\014\000\023\000\
\000\000\004\000\012\000\011\000\001\000\000\000\031\000\000\000\
\000\000\000\000\000\000\009\000\024\000"

let yydgoto = "\002\000\
\008\000\017\000\009\000\028\000\004\000\010\000\029\000\030\000\
\060\000\031\000\032\000\033\000\034\000\035\000"

let yysindex = "\012\000\
\250\254\000\000\024\255\000\000\243\254\000\000\000\000\005\255\
\243\254\025\255\030\255\000\000\032\255\060\255\050\255\035\255\
\077\255\021\255\081\255\000\000\014\255\021\255\038\255\038\255\
\038\255\000\000\000\000\249\254\066\255\144\255\000\000\000\000\
\000\000\000\000\000\000\063\255\038\255\119\255\133\255\252\254\
\028\255\000\000\000\000\000\000\021\255\021\255\021\255\059\255\
\038\255\038\255\038\255\038\255\038\255\000\000\000\000\000\000\
\000\000\000\000\000\000\038\255\098\255\158\255\000\000\000\000\
\166\255\000\000\000\000\000\000\000\000\062\255\000\000\048\255\
\048\255\096\255\171\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\106\255\000\000\000\000\000\000\
\106\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\107\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\254\
\000\000\000\000\000\000\000\000\107\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\082\255\000\000\099\255\
\116\255\065\255\000\255\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\244\255\000\000\103\000\074\000\235\255\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yytablesize = 184
let yytable = "\039\000\
\045\000\042\000\043\000\044\000\013\000\010\000\013\000\010\000\
\038\000\040\000\006\000\007\000\001\000\046\000\047\000\062\000\
\046\000\047\000\021\000\065\000\010\000\010\000\003\000\037\000\
\005\000\021\000\013\000\070\000\071\000\072\000\073\000\074\000\
\041\000\067\000\068\000\011\000\022\000\037\000\075\000\023\000\
\024\000\025\000\041\000\022\000\026\000\027\000\023\000\024\000\
\025\000\014\000\018\000\026\000\027\000\023\000\024\000\025\000\
\049\000\050\000\026\000\027\000\053\000\015\000\016\000\023\000\
\024\000\025\000\019\000\048\000\026\000\027\000\025\000\050\000\
\025\000\025\000\053\000\025\000\025\000\025\000\025\000\025\000\
\025\000\025\000\025\000\025\000\020\000\025\000\025\000\030\000\
\036\000\030\000\030\000\069\000\030\000\030\000\061\000\030\000\
\030\000\030\000\030\000\030\000\030\000\076\000\030\000\030\000\
\028\000\050\000\028\000\003\000\005\000\028\000\028\000\012\000\
\028\000\028\000\028\000\028\000\028\000\028\000\066\000\028\000\
\028\000\027\000\000\000\027\000\063\000\000\000\027\000\027\000\
\000\000\027\000\027\000\027\000\027\000\027\000\027\000\000\000\
\027\000\027\000\064\000\046\000\047\000\049\000\050\000\051\000\
\052\000\053\000\054\000\055\000\056\000\057\000\058\000\059\000\
\049\000\050\000\051\000\052\000\053\000\054\000\055\000\056\000\
\057\000\058\000\059\000\077\000\000\000\000\000\049\000\050\000\
\051\000\052\000\053\000\064\000\000\000\000\000\049\000\050\000\
\051\000\052\000\053\000\049\000\050\000\051\000\052\000\053\000"

let yycheck = "\021\000\
\008\001\023\000\024\000\025\000\006\001\006\001\008\001\008\001\
\021\000\022\000\024\001\025\001\001\000\021\001\022\001\037\000\
\021\001\022\001\005\001\041\000\021\001\022\001\029\001\010\001\
\001\001\005\001\002\001\049\000\050\000\051\000\052\000\053\000\
\005\001\046\000\047\000\031\001\023\001\010\001\060\000\026\001\
\027\001\028\001\005\001\023\001\031\001\032\001\026\001\027\001\
\028\001\020\001\001\001\031\001\032\001\026\001\027\001\028\001\
\009\001\010\001\031\001\032\001\013\001\030\001\003\001\026\001\
\027\001\028\001\032\001\002\001\031\001\032\001\006\001\010\001\
\008\001\009\001\013\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\019\001\008\001\021\001\022\001\006\001\
\008\001\008\001\009\001\033\001\011\001\012\001\032\001\014\001\
\015\001\016\001\017\001\018\001\019\001\004\001\021\001\022\001\
\006\001\010\001\008\001\002\001\002\001\011\001\012\001\009\000\
\014\001\015\001\016\001\017\001\018\001\019\001\045\000\021\001\
\022\001\006\001\255\255\008\001\006\001\255\255\011\001\012\001\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\255\255\
\021\001\022\001\006\001\021\001\022\001\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\019\001\
\009\001\010\001\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\019\001\006\001\255\255\255\255\009\001\010\001\
\011\001\012\001\013\001\006\001\255\255\255\255\009\001\010\001\
\011\001\012\001\013\001\009\001\010\001\011\001\012\001\013\001"

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
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'decls) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'bexprs) in
    Obj.repr(
# 67 "src/frontend/parser.mly"
  ({init=_3; constraints=_7})
# 237 "src/frontend/parser.ml"
               : Syntax.prog))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : assign) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 70 "src/frontend/parser.mly"
               (_1::_2)
# 245 "src/frontend/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "src/frontend/parser.mly"
    ([])
# 251 "src/frontend/parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bexprs) in
    Obj.repr(
# 74 "src/frontend/parser.mly"
                               (_1::_3)
# 259 "src/frontend/parser.ml"
               : 'bexprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "src/frontend/parser.mly"
    ([])
# 265 "src/frontend/parser.ml"
               : 'bexprs))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "src/frontend/parser.mly"
                  (INT)
# 271 "src/frontend/parser.ml"
               : typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "src/frontend/parser.mly"
                  (REAL)
# 277 "src/frontend/parser.ml"
               : typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : dom) in
    Obj.repr(
# 83 "src/frontend/parser.mly"
    ( (_1, _2, _4) )
# 286 "src/frontend/parser.ml"
               : assign))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : float) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : float) in
    Obj.repr(
# 87 "src/frontend/parser.mly"
      ((_2,_4))
# 294 "src/frontend/parser.ml"
               : dom))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'cmp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "src/frontend/parser.mly"
                                        (Cmp (_2, _1, _3))
# 303 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : bexpr) in
    Obj.repr(
# 91 "src/frontend/parser.mly"
                                        (Or (_1,_3))
# 311 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : bexpr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : bexpr) in
    Obj.repr(
# 92 "src/frontend/parser.mly"
                                        (And (_1,_3))
# 319 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : bexpr) in
    Obj.repr(
# 93 "src/frontend/parser.mly"
                                        (Not (_2))
# 326 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : bexpr) in
    Obj.repr(
# 94 "src/frontend/parser.mly"
                                        ( _2 )
# 333 "src/frontend/parser.ml"
               : bexpr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'parenthesized_expr) in
    Obj.repr(
# 98 "src/frontend/parser.mly"
                                        ( _1 )
# 340 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr) in
    Obj.repr(
# 99 "src/frontend/parser.mly"
                                        ( _1 )
# 347 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 100 "src/frontend/parser.mly"
                                        ( Unary (COS,_2) )
# 354 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 101 "src/frontend/parser.mly"
                                        ( Unary (SIN,_2) )
# 361 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 102 "src/frontend/parser.mly"
                                        ( Unary (SQRT,_2) )
# 368 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'leaf) in
    Obj.repr(
# 103 "src/frontend/parser.mly"
                                        ( _1 )
# 375 "src/frontend/parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : float) in
    Obj.repr(
# 106 "src/frontend/parser.mly"
                                        ( Cst (_1) )
# 382 "src/frontend/parser.ml"
               : 'leaf))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 107 "src/frontend/parser.mly"
                                        ( Var _1 )
# 389 "src/frontend/parser.ml"
               : 'leaf))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 110 "src/frontend/parser.mly"
                                 ( _2 )
# 396 "src/frontend/parser.ml"
               : 'parenthesized_expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 111 "src/frontend/parser.mly"
                                           ( Unary (NEG, _3) )
# 403 "src/frontend/parser.ml"
               : 'parenthesized_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "src/frontend/parser.mly"
                       (Binary (POW,_1,_3))
# 411 "src/frontend/parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr2) in
    Obj.repr(
# 115 "src/frontend/parser.mly"
                       (_1)
# 418 "src/frontend/parser.ml"
               : 'binop_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "src/frontend/parser.mly"
                            (Binary(DIV,_1,_3))
# 426 "src/frontend/parser.ml"
               : 'binop_expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "src/frontend/parser.mly"
                            (Binary(MUL,_1,_3))
# 434 "src/frontend/parser.ml"
               : 'binop_expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'binop_expr3) in
    Obj.repr(
# 120 "src/frontend/parser.mly"
                            (_1)
# 441 "src/frontend/parser.ml"
               : 'binop_expr2))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "src/frontend/parser.mly"
                        (Binary(ADD,_1,_3))
# 449 "src/frontend/parser.ml"
               : 'binop_expr3))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "src/frontend/parser.mly"
                        (Binary(SUB,_1,_3))
# 457 "src/frontend/parser.ml"
               : 'binop_expr3))
; (fun __caml_parser_env ->
    Obj.repr(
# 127 "src/frontend/parser.mly"
                                ( LT )
# 463 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 128 "src/frontend/parser.mly"
                                ( GT )
# 469 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "src/frontend/parser.mly"
                                ( LEQ )
# 475 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 130 "src/frontend/parser.mly"
                                ( GEQ )
# 481 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "src/frontend/parser.mly"
                                ( EQ )
# 487 "src/frontend/parser.ml"
               : 'cmp))
; (fun __caml_parser_env ->
    Obj.repr(
# 132 "src/frontend/parser.mly"
                                ( NEQ )
# 493 "src/frontend/parser.ml"
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
