open Csp
open Lexing

(*****************************************)
(*              AST CHECKING             *)
(*****************************************)


(*errors*)
exception IllFormedAST of string

let illegal_var_draw v =
  Format.sprintf
    "Illegal variable to draw:%s don't belong to the variables of the problem"
    v

let illegal_var_draw2 v1 v2 =
  Format.sprintf
    "Illegal variable to draw:%s and %s don't belong to the variables of the problem"
    v1 v2

let illegal_constraint spec =
  Format.sprintf "Illegal constraint: %s" spec

let check_ast p =
  let h = Hashtbl.create 10 in
  let check_vars () =
    List.iter (fun (_,v,_) -> if Hashtbl.mem h v then
	raise (IllFormedAST (Format.sprintf "two variables share the same name: %s" v))
      else Hashtbl.add h v true
    ) p.init
  and check_draw () =
    match p.to_draw with
    | [] -> ()
    | l -> List.iter (fun v ->
      if not (Hashtbl.mem h v) then raise (IllFormedAST (illegal_var_draw v))) l

  and check_dom () =
    let aux (_, var, d) =
      match d with
      | Finite (f1,f2) -> if f1 > f2 then
	  raise (IllFormedAST (Format.sprintf "Illegal domain for var %s:[%f;%f]" var f1 f2))
      | _ -> ()
    in List.iter aux p.init
  and check_constrs () =
    let check_v = function
      | Var v ->
         if not (Hashtbl.mem h v) then
           let msg = illegal_constraint ("non-declared variable "^v) in
           Hashtbl.iter (fun a _ -> Format.printf "%s\n" a) h;
           raise (IllFormedAST msg)
      | _ -> ()
    in
    List.iter (iter_constr check_v (fun _ -> ())) p.constraints
  in
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
let parse (filename:string option) : prog =
  let filename =
    match filename with
    | None -> failwith "you must specify a filename"
    | Some s -> s
  in
  let f = open_in filename in
  let lex = from_channel f in
  let fileparser =
    let ext = Filename.extension filename in
    if ext = ".mod" then begin
        (* Format.printf "mod file detected. parsing with mod parser\n%!"; *)
        (fun lex -> ModParser.stmts ModLexer.token lex |> ModCsp.toCsp)
      end else Parser.file Lexer.token
  in
  try
    lex.lex_curr_p <- { lex.lex_curr_p with pos_fname = filename; };
    fileparser lex
  with
  (* | Failure s -> *)
  (*     Printf.eprintf "Error near %s\n%s\n" *)
  (*       (string_of_position lex.lex_start_p) *)
	(*       s; *)
  (*     failwith "Parse error" *)
  | Parsing.Parse_error -> failwith "Parse error"


let parse fn =
  let p = parse fn in
  check_ast p;
  {p with constraints = p.constraints}
