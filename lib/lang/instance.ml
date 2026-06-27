open Tools

type t = Q.t VarMap.t

type typed = (Q.t * typ) VarMap.t

let of_list = function
  | [] -> invalid_arg "Instance.of_list: no coordinate given"
  | x -> VarMap.of_list x

let of_list_typed coords : typed =
  match coords with
  | [] -> invalid_arg "Instance.of_list: no coordinate given"
  | x -> VarMap.of_list x

let translate i1 i2 =
  VarMap.fold (fun v q acc -> VarMap.update v (Option.map (Q.add q)) acc) i1 i2

let to_apron_gen i =
  let open Apronext in
  let env, coords = VarMap.bindings i |> List.split in
  let e = Environmentext.make_s (Array.of_list env) [||] in
  Generatorext.of_rational_point e coords

let print fmt instance =
  let bind fmt (var, value) =
    Format.fprintf fmt "%s : %a" var Q.pp_print value
  in
  VarMap.bindings instance |> Format.fprintf fmt "{%a}" (pp_list_sep "; " bind)

let print_typed fmt typed_instance =
  let bind fmt (var, (value, typ)) =
    match typ with
    | Int -> Format.fprintf fmt "%s : %i" var (Q.ceil value)
    | Real -> Format.fprintf fmt "%s : %a" var Q.pp_print value
  in
  VarMap.bindings typed_instance
  |> Format.fprintf fmt "{%a}" (pp_list_sep "; " bind)

let to_string : t -> string = Format.asprintf "%a" print

let to_string_typed : typed -> string = Format.asprintf "%a" print_typed

let iter = VarMap.iter

let to_seq = VarMap.to_seq

let to_seq_typed = VarMap.to_seq

let iter_typed = VarMap.iter

let untype typed : t = VarMap.map fst typed
