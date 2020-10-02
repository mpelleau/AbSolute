open Csp
open Lexing

exception IllFormedAST of string

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
  and check_dom () =
    let aux (_, v, d) =
      match d with
      | Finite (f1,f2) ->
         if f1 > f2 then
	         raise (IllFormedAST (Format.sprintf "Illegal domain : %s in [%s;%s]"
                                  v (Mpqf.to_string f1) (Mpqf.to_string f2)))
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
    List.iter (Csp_helper.iter_constr check_v (fun _ -> ())) p.constraints
  in
  check_vars ();
  check_dom ();
  check_constrs ()

(* open a file and parse it *)
let parse (filename:string) : prog =
  if !Constant.debug > 0 then Format.printf "parsing\n%!";
  let f = open_in filename in
  let lex = from_channel f in
  let fileparser =
    let len = String.length filename in
    if len >= 4 && (String.sub filename (len-4) 4) = ".mod" then begin
        (fun lex -> ModParser.stmts ModLexer.token lex |> ModCsp.toCsp)
      end
    else Parser.file Lexer.token
  in
  try
    lex.lex_curr_p <- { lex.lex_curr_p with pos_fname = filename; };
    fileparser lex
  with
  | IllFormedAST s -> failwith s
  | Parsing.Parse_error -> failwith "Parse error"

let parse (fn:string) =
  Format.printf "parsing ... %!";
  let p = parse fn in
  Format.printf "done.\nast check ... %!";
  check_ast p;
  Format.printf "done.\n%!";
  if !Constant.trace then Format.printf "\n@[<2>%a@]%!" Csp_printer.print p;
  p
