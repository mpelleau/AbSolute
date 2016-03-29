open Syntax
open Lexing

(*****************************************)
(*              AST CHECKING             *)
(*****************************************)

exception IllFormedAST of string

let illegal_var_draw v = 
  Format.sprintf
    "Illegal variable to draw:%s don't belong to the variables of the problem" 
    v

let illegal_var_draw2 v1 v2 = 
  Format.sprintf
    "Illegal variable to draw:%s and %s don't belong to the variables of the problem" 
    v1 v2

let check_ast p =
  let h = Hashtbl.create 10 in
  let check_vars () =
    List.iter (fun (_,v,_) -> if Hashtbl.mem h v then 
	raise (IllFormedAST (Format.sprintf "two variables share the same name: %s" v))
      else Hashtbl.add h v true
    ) p.init 
  and check_draw () = 
    match p.to_draw with
    | None -> ()
    | Some (v1,v2) -> 
      let rec aux acc1 acc2 = function
	| [] when acc2 -> raise (IllFormedAST (illegal_var_draw v1))
	| [] when acc1 -> raise (IllFormedAST (illegal_var_draw v2))
	| [] -> raise(IllFormedAST (illegal_var_draw2 v1 v2))
	| (_,v,_)::tl -> 
	  let a1 = v=v1 and a2 = v=v2 in
	  if a1 && a2 then ()
	  else  aux (acc1 || a1) (acc2 || a2) tl
      in
      aux false false p.init;
  and check_dom () =
    let aux (_, var, d) =
      match d with 
      | Finite (f1,f2) -> if f1 >= f2 then 
	  raise (IllFormedAST (Format.sprintf "Illegal domain for var %s:[%f;%f]" var f1 f2))
      | _ -> ()
    in List.iter aux p.init
  and check_constrs () = () in
  check_vars ();
  check_dom ();
  check_draw ();
  check_constrs ()


(****************************************)
(*               Parsing                *)
(****************************************)

let string_of_position p =
  Printf.sprintf "%s:%i:%i" p.pos_fname p.pos_lnum (p.pos_cnum - p.pos_bol)
   
(* open a file and parse it *)
let parse (filename:string) : prog =
  let f = open_in filename in
  let lex = from_channel f in
  try
    lex.lex_curr_p <- { lex.lex_curr_p with pos_fname = filename; };
    Parser.file Lexer.token lex
  with
  | Failure "lexing: empty token" ->
      Printf.eprintf "Parse error (invalid token) near %s\n" 
        (string_of_position lex.lex_start_p);
      failwith "Parse error"
  | _ ->
      Printf.eprintf "Parse error (invalid syntax) near %s\n" 
        (string_of_position lex.lex_start_p);
      failwith "Parse error"

let parse fn =
  let p = parse fn in
  {p with constraints = List.map power_unrolling_bexpr p.constraints}
