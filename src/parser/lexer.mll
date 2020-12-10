
{
 open Lexing
 open Parser


(* keyword table *)
let kwd_table = Hashtbl.create 10
let _ =
  List.iter (fun (a,b) -> Hashtbl.add kwd_table a b)
    [
      "init",           TOK_INIT;
      "objective", 	    TOK_OBJ;
      "constraints",    TOK_CONSTR;
      "constants",      TOK_CST;
      "solutions",      TOK_SOL;
      "none",           TOK_NONE;
      "int",            TOK_INT;
      "real",           TOK_REAL;
      "oo",             TOK_INF;
      "in",             TOK_IN;
      "notin",          TOK_NOTIN
    ]

(* (exact) parsing of decimal constants *)
let parse_const s =
  (* splits a (non-empty) string into a sign and the rest of the string *)
  let split_sign s =
    match s.[0] with
    | ('-' | '+') as c -> c,(String.sub s 1 (String.length s-1))
    | _ -> '+',s
  in
  let of_decimal_point s shift =
    try
      let i =  String.index s '.' in
      let size_dec = String.length s-i-1 in
      let rmv_dot = (String.sub s 0 i)^(String.sub s (i+1) size_dec) in
      if size_dec > shift then
        Mpqf.div
          (Mpqf.of_string rmv_dot)
          (Mpqf.of_string ("1"^(String.make (size_dec-shift)) '0'))
      else
        Mpqf.of_string (rmv_dot^(String.make (shift-size_dec) '0'))
    with Not_found -> Mpqf.of_string (s^String.make shift '0')
  in
  let of_scientific_notation s =
    let sign,s = split_sign s in
    let i = ref 0 in
    try
      String.iter (fun c -> incr i; match c with 'e' | 'E' -> raise Exit | _ -> ()) s;
      (* not a scientific notation *)
      if sign='+' then of_decimal_point s 0 else of_decimal_point ("-"^s) 0
    with
    | Exit ->
       let m = String.sub s 0 (!i-1) in
       let e = int_of_string (String.sub s !i (String.length s- !i)) in
       if sign='+' then of_decimal_point m e
       else of_decimal_point ("-"^m) e
  in
  of_scientific_notation s

}



(* character classes *)
let space = [' ' '\t' '\r']+
let newline = "\n" | "\r" | "\r\n"
let digit = ['0'-'9']
let cst = ( digit+ | "." digit+ | digit+ "." digit*)
let const = (cst | cst "e" digit+ | cst "e+" digit+ | cst "e-" digit+)

rule token = parse

(* identifier or reserved keyword *)
| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* '%'? as id
{ try Hashtbl.find kwd_table id with Not_found -> TOK_id id }


(* symbols *)
| "("    { TOK_LPAREN }
| ")"    { TOK_RPAREN }
| "{"    { TOK_LBRACE }
| "}"    { TOK_RBRACE }
| "["    { TOK_LBRACKET }
| "]"    { TOK_RBRACKET }
| ","    { TOK_COMMA }
| ";"    { TOK_SEMICOLON }
| "+"    { TOK_PLUS }
| "-"    { TOK_MINUS }
| "*"    { TOK_MULTIPLY }
| "/"    { TOK_DIVIDE }
| "^"    { TOK_POW }
| "<"    { TOK_LESS }
| ">"    { TOK_GREATER }
| "<="   { TOK_LESS_EQUAL }
| ">="   { TOK_GREATER_EQUAL }
| "!="   { TOK_NOT_EQUAL }
| "="    { TOK_ASSIGN }
| "&&"   { TOK_AND }
| "||"   { TOK_OR }
| "!"    { TOK_NOT }
(* literals *)
| const as c { TOK_const (parse_const c) }

(* spaces, comments *)
| "/*" { comment lexbuf; token lexbuf }
| "//" [^ '\n' '\r']* { token lexbuf }
| newline { new_line lexbuf; token lexbuf }
| space { token lexbuf }

(* end of file *)
| eof { TOK_EOF }

(* nested comments (handled recursively)  *)
and comment = parse
| "*/" { () }
| [^ '\n' '\r'] { comment lexbuf }
| newline { new_line lexbuf; comment lexbuf }
