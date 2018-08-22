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

(* allowed functions and their arity *)
let runtime = [
    ("sqrt",1);
    ("exp",1);
    ("ln",1);
    ("pow",1);
    ("cos",1);
    ("sin",1);
    ("tan",1);
    ("acos",1);
    ("asin",1);
    ("atan",1);
    ("max",2);
    ("min",2);
  ]

let illegal_funcall func arity =
  if List.exists (fun (name,_) -> name = func) runtime then
    Format.sprintf "Illegal funcall: %s expects %d arguments but was given %d"
                   func
                   (List.assoc func runtime)
                   arity
  else Format.sprintf "Illegal funcall: unknown function %s" func


let check_ast p =
  let h = Hashtbl.create 10 in
  let check_vars () =
    List.iter (fun (_,v,_) -> if Hashtbl.mem h v then
	raise (IllFormedAST (Format.sprintf "two variables share the same name: %s" v))
      else Hashtbl.add h v true
    ) p.init
  and check_csts () =
    List.iter (fun (v,_) -> if Hashtbl.mem h v then
      raise (IllFormedAST (Format.sprintf "two variables share the same name: %s" v))
      else Hashtbl.add h v true
    ) p.constants
  and check_draw () =
    match p.to_draw with
    | [] -> ()
    | l -> List.iter (fun v ->
      if not (Hashtbl.mem h v) then raise (IllFormedAST (illegal_var_draw v))) l

  and check_dom () =
    let aux (_, var, d) =
      match d with
      | Finite (f1,f2) -> if f1 > f2 then
	  raise (IllFormedAST (Format.sprintf "Illegal domain for var %s:[%s;%s]" var (Mpqf.to_string f1) (Mpqf.to_string f2)))
      | _ -> ()
    in List.iter aux p.init
  and check_constrs () =
    let check_v = function
      | Var v ->
         if not (Hashtbl.mem h v) then
           let msg = illegal_constraint ("non-declared variable "^v) in
           Hashtbl.iter (fun a _ -> Format.printf "%s\n" a) h;
           raise (IllFormedAST msg)
      | Funcall(name,args) ->
         let nb_args = List.length args in
         if not (List.exists (fun (funname,arrity) ->
             name = funname && nb_args = arrity
                   ) runtime) then
           let msg = illegal_funcall name nb_args in
           raise (IllFormedAST msg)
      | _ -> ()
    in
    List.iter (iter_constr check_v (fun _ -> ())) p.constraints
  in
  check_vars ();
  check_csts ();
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
  (* Parsing.set_trace true; *)
  let filename =
    match filename with
    | None -> failwith "you must specify a filename"
    | Some s -> s
  in
  let f = open_in filename in
  Format.printf "file opened\n";
  let lex = from_channel f in
  Format.printf "lex channel\n";
  let fileparser =
    let ext = Filename.extension filename in
    if ext = ".mod" then begin
        (* Format.printf "mod file detected. parsing with mod parser\n%!"; *)
        (fun lex -> ModParser.stmts ModLexer.token lex |> ModCsp.toCsp)
      end else (Format.printf "parsing...\n"; Parser.file Lexer.token)
  in
  try
    lex.lex_curr_p <- { lex.lex_curr_p with pos_fname = filename; };
    fileparser lex
  with
  | IllFormedAST s -> failwith s
  (* | Failure s -> *)
  (*     Printf.eprintf "Error near %s\n%s\n" *)
  (*       (string_of_position lex.lex_start_p) *)
	(*       s; *)
  (*     failwith "Parse error" *)
  | Parsing.Parse_error -> failwith "Parse error"


let parse fn =
  let p = parse fn in
  check_ast p;
  (*List.iter (fun c -> Format.printf "  -- %a\n" Csp.print_bexpr c) p.Csp.constraints;*)
  let prob = if !Constant.rewrite then Preprocessing.preprocess p else Preprocessing.no_views p in
  (*List.iter (fun c -> Format.printf "  ++ %a\n" Csp.print_bexpr c) prob.Csp.constraints;*)
  (*List.iter (fun (v, (l, h)) -> Format.printf "  ** %s = %f (%f)\n" v l h) prob.Csp.constants;
  List.iter (fun (v, e) -> Format.printf "  // %s = %a\n" v Csp.print_expr e) prob.Csp.view;
  Format.printf "\n";*)
  let j = Csp.compute_jacobian prob in
  {prob with jacobian = j}
