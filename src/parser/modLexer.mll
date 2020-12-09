
{
 open Lexing
 open ModParser


(* keyword table *)
let kwd_table = Hashtbl.create 10
let _ =
  List.iter (fun (a,b) -> Hashtbl.add kwd_table a b)
    [
      "param",          PARAM;
      "var",            VAR;
      "objective", 	OBJ;
      "constraints",    CONSTR;
      "oo",             INF;
      "-oo",            MINF
   ]

(* (exact) parsing of decimal constants constants *)
(*let parse_const c =
  let rec div10 x n =
    if n <= 0 then x else div10 (x /. (float_of_int 10)) (n-1)
  in
  try
    let p = String.index c '.' in
    let p' = String.length c - p - 1 in
    let x = (String.sub c 0 p)^(String.sub c (p+1) p') in
    div10 (float_of_string x) p'
  with Not_found ->
    float_of_string c
*)

let parse_const = float_of_string
}



(* character classes *)
let space = [' ' '\t' '\r']+
let newline = "\n" | "\r" | "\r\n" | "subject to"
let digit = ['0'-'9']
let const = digit+ ('.' digit* )? (['e' 'E'] ['+' '-']? digit+ )?


rule token = parse

(* identifier or reserved keyword *)
| ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* '%'? as id
{ try Hashtbl.find kwd_table id with Not_found -> ID id }


(* symbols *)
| "("    { LPAREN }
| ")"    { RPAREN }
| "{"    { LBRACE }
| "}"    { RBRACE }
| "["    { LBRACKET }
| "]"    { RBRACKET }
| ","    { COMMA }
| ";"    { SEMICOLON }
| "+"    { PLUS }
| "-"    { MINUS }
| "*"    { MULTIPLY }
| "/"    { DIVIDE }
| "^"    { POW }
| "<"    { LT }
| ">"    { GT }
| "<="   { LTE }
| ">="   { GTE }
| "!="   { NOT_EQUAL }
| "="    { ASSIGN }
| "&&"   { AND }
| "||"   { OR }
| "!"    { NOT }
| ":"    { COLON }
| ":="   { COLONEQUAL }
| ".."   { PPOINT }

(* literals *)
| const as c { FLOAT (float_of_string c) }

(* (\* spaces, comments *\) *)
(* | "/*" { comment lexbuf; token lexbuf } *)
| "#" [^ '\n' '\r']* { token lexbuf }
| newline { new_line lexbuf; token lexbuf }
| space { token lexbuf }

(* end of file *)
| eof { EOF }

(* (\* nested comments (handled recursively)  *\) *)
(* and comment = parse *)
(* | "*/" { () } *)
(* | [^ '\n' '\r'] { comment lexbuf } *)
(* | newline { new_line lexbuf; comment lexbuf } *)
