open Csp
open Csp_printer
open Tools

module Make(Abs : Signature.AbstractCP) = struct

  include Solver.Solve(Abs)

  (****************)
  (*   PRINTING   *)
  (****************)

  let print_instance fmt (instance:Csp.instance) =
    let bindings = VarMap.bindings instance in
    let print_bind fmt (var,value) =
      Format.fprintf fmt "%s:%a"
                     var
                     Mpqf.print value
    in
    Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ; ")
                         (fun fmt -> Format.fprintf fmt "%a" print_bind)
                         fmt bindings

  let err_constr (instance:Csp.instance) cstr e1 cmp e2 =
    Format.eprintf "the instance %a do not satisfy the constraint %a\n"
                   print_instance instance
                   print_bexpr cstr;
    Format.eprintf "it evaluates to %a %a %a\n"
                   Mpqf.print e1
                   print_cmpop cmp
                   Mpqf.print e2

  (* evaluate an expression according to an instance *)
  let eval (instance:Csp.instance) expr =
    let rec aux = function
      | Var v -> VarMap.find_fail v instance
      | Cst i -> i
      | Binary(op,e1,e2) ->
         let e1' = aux e1 and e2' = aux e2 in
         (match op with
          | ADD -> Mpqf.add e1' e2'
          | SUB -> Mpqf.sub e1' e2'
          | MUL -> Mpqf.mul e1' e2'
          | DIV -> Mpqf.div e1' e2'
        | POW -> Mpqf.of_float ((Mpqf.to_float e1') ** (Mpqf.to_float e2')))
      | Neg e -> Mpqf.neg (aux e)
      | Funcall(name, [e]) ->
         let e = Mpqf.to_float (aux e) in
         let func =
           match name with
           | "sqrt" -> sqrt
           | "cos"  -> cos
           | "sin"  -> sin
           | "acos" -> acos
           | "asin" -> asin
           | "tan"  -> tan
           | "atan" -> atan
           | "exp"  -> exp
           | "ln"   -> log
           | x -> Tools.fail_fmt "unrecognized function name %s" x
         in Mpqf.of_float (func e)
      | Funcall(name,[e1;e2]) ->
          let e1 = Mpqf.to_float (aux e1) in
          let e2 = Mpqf.to_float (aux e2) in
          let func =
            match name with
            | "max" -> max
            | "min" -> min
            | x -> Tools.fail_fmt "unrecognized function name %s" x
          in Mpqf.of_float (func e1 e2)
      | Funcall(name, _) -> Tools.fail_fmt "cant evaluate function call %s" name
    in aux expr

  (* check if an instance is valid wrt to a constraint *)
  let check_cstr print (instance:Csp.instance) cstr =
    let rec aux = function
      | Cmp(op,e1,e2) ->
         let e1' = eval instance e1 and e2' = eval instance e2 in
         let res =
           (match op with
            | EQ  -> e1' = e2'
            | NEQ -> e1' <> e2'
            | GT  -> e1' >  e2'
            | GEQ -> e1' >= e2'
            | LT  -> e1' <  e2'
            | LEQ -> e1' <= e2'
           )
         in
         if not res then
           if print then err_constr instance cstr e1' op e2';
         res
      | Or(c1,c2)  -> aux c1 || aux c2
      | And(c1,c2) -> aux c1 && aux c2
      | Not c      -> not (aux c)
    in aux cstr

  (*checks if the value of variable of an instance belong to the corresponding domain *)
  let belong_to (instance:Csp.instance) (typ,var,dom) =
    let check_type typ value =
      match typ with
      | Int  -> ceil value = value
      | Real -> true
    in
    let check_dom dom value =
      match dom with
      | Finite (l,u) -> l < value && value < u
    | _ -> failwith "cant handle infinite domains for now"
    in
    let value = VarMap.find_fail var instance |> Mpqf.to_float in
    check_type typ value && check_dom dom (Mpqf.of_float value)

  (* checks if an instance satisfies a csp *)
  let check_instance fn print (instance:Csp.instance) csp =
    List.for_all (belong_to instance) csp.init
    &&
    List.for_all (check_cstr print instance) csp.constraints

  (* checks that the sure value DO satisfy the constraints *)
  let check_sure fn csp result =
    let total_sure = ref 0 in
    iter_sure (fun (e,_) ->
        let cpt = ref 0 in
        while !cpt < 10 do
          incr total_sure;
          incr cpt;
          (* we try to spawn at least 10 points *)
          let i = Abs.spawn e in
          ignore (check_instance fn false i csp)
        done
      ) result;
    !total_sure

  (* compute the ratio of unsure value that DO satisfy the constraints *)
  let check_unsure fn csp result =
    let total = ref 0 and unsure = ref 0 in
    (* we try to spawn at least 10 points *)
    iter_unsure (fun (e,_) ->
        let cpt = ref 0 in
        while !cpt < 10 do
          incr total;
          incr cpt;
          let i = Abs.spawn e in
          if check_instance fn false i csp then incr unsure
        done
      ) result;
    if !total > 0 then Some ((float !unsure) /. (float !total)) else None

  (* checks if an instance is covered by at least one abstract element of a list *)
  let covered_by (i:Csp.instance) abs_list =
    List.exists (fun (e,_) -> Abs.is_abstraction e i) abs_list

  (* checks that the problem's known solutions belong to an astract element *)
  let check_known_solutions fn result goods =
    let open Result in
    try
      List.iter (fun instance ->
          let covered_sure = covered_by instance result.sure in
          if not covered_sure then
            let covered_unsure = covered_by instance result.unsure in
            if not covered_unsure then
              begin
                (*the instance sould be covered by an element *)
                Format.eprintf "%s: the solution %a, should be covered by an abstract element, but is not\n%!"
                  fn
                  print_instance instance;
                raise Exit
              end
        ) goods;
      true
    with Exit -> false

  (* check that some inconsistent instance does not belong to a sure solutions *)
  let check_known_bad fn result nogoods =
    let open Result in
    try
      List.iter (fun instance ->
          let covered_sure = covered_by instance result.sure in
          if covered_sure then
            (* the instance sould NOT be covered by a sure element *)
            begin
              Format.eprintf "%s the instance %a is covered by an abstract element but it shouldnt be\n%!"
                fn
                print_instance instance;
              raise Exit
            end
        ) nogoods;
      true
    with Exit -> false

  (* check that the solving does not find any sure solution *)
  let check_infeasible result =
    let open Result in
    result.sure = []

  let result csp = solving csp

end
