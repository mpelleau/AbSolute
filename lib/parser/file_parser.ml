open Csp
open Expr
open Lexing

exception Semantic_error of string

exception Syntax_error of string

let illegal_constraint spec = Format.sprintf "Illegal constraint: %s" spec

(* allowed functions and their arity *)
let runtime =
  [ ("abs", 1)
  ; ("sqrt", 1)
  ; ("exp", 1)
  ; ("ln", 1)
  ; ("pow", 1)
  ; ("cos", 1)
  ; ("sin", 1)
  ; ("tan", 1)
  ; ("acos", 1)
  ; ("asin", 1)
  ; ("atan", 1)
  ; ("max", 2)
  ; ("min", 2) ]

let illegal_funcall func arity =
  if List.exists (fun (name, _) -> name = func) runtime then
    Format.sprintf "Illegal funcall: %s expects %d arguments but was given %d"
      func (List.assoc func runtime) arity
  else Format.sprintf "Illegal funcall: unknown function %s" func

let check_ast p =
  let h = Hashtbl.create 10 in
  let check_vars () =
    List.iter
      (fun (_, v, _) ->
        if Hashtbl.mem h v then
          raise
            (Semantic_error
               (Format.sprintf "two variables share the same name: %s" v))
        else Hashtbl.add h v true)
      p.init
  and check_dom () =
    let open Dom in
    let aux (_, v, d) =
      match d with
      | Finite (f1, f2) ->
          if f1 > f2 then
            raise
              (Semantic_error
                 (Format.sprintf "Illegal domain : %s in [%s;%s]" v
                    (Mpqf.to_string f1) (Mpqf.to_string f2)))
      | _ -> ()
    in
    List.iter aux p.init
  and check_constrs () =
    let check_v = function
      | Var v ->
          if not (Hashtbl.mem h v) then (
            let msg = illegal_constraint ("non-declared variable " ^ v) in
            Hashtbl.iter (fun a _ -> Format.printf "%s\n" a) h ;
            raise (Semantic_error msg) )
      | Funcall (name, args) ->
          let nb_args = List.length args in
          if
            not
              (List.exists
                 (fun (funname, arrity) -> name = funname && nb_args = arrity)
                 runtime)
          then
            let msg = illegal_funcall name nb_args in
            raise (Semantic_error msg)
      | _ -> ()
    in
    List.iter (Csp_helper.iter_constr check_v (fun _ -> ())) p.constraints
  in
  check_vars () ; check_dom () ; check_constrs ()

let get_line fname number =
  let ic = open_in fname in
  let rec loop cur =
    let cur_line = input_line ic in
    if cur = number then (close_in ic ; cur_line) else loop (cur + 1)
  in
  loop 1

let show_error fname line col =
  let line = get_line fname line in
  let col = String.make (col - 1) ' ' in
  Format.asprintf "%s\n%s^" line col

let print_pos ppf lex =
  let p = lex.lex_curr_p in
  let col = p.pos_cnum - p.pos_bol in
  Format.fprintf ppf "file %s, Line %d, Column %d\n%s" p.pos_fname p.pos_lnum
    col
    (show_error p.pos_fname p.pos_lnum col)

let constr (str : string) =
  let lexb = Lexing.from_string str in
  (Parser.bexpreof Lexer.token) lexb

let expr (str : string) =
  let lexb = Lexing.from_string str in
  (Parser.expreof Lexer.token) lexb

(* open a file and parse it *)
let parse (filename : string) : problem =
  if !Constant.debug > 0 then Format.printf "parsing\n%!" ;
  let f = open_in filename in
  let lex = from_channel f in
  let fileparser = Parser.file Lexer.token in
  try
    lex.lex_curr_p <- {lex.lex_curr_p with pos_fname= filename} ;
    fileparser lex
  with _ ->
    let msg = Format.asprintf "Syntax Error in %a\n" print_pos lex in
    close_in f ; raise (Syntax_error msg)
