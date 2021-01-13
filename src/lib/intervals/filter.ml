(* This modules defines sound backward operators intervals given a forward
   evaluation semantics *)

module Make (I : Itv_sig.ITV_EVAL) = struct
  include I

  (* r = -i => i = -r *)
  let filter_neg (i : t) (r : t) : t option = meet i (neg r)

  let filter_abs (i : t) (r : t) : t option =
    if is_positive i then meet i r
    else if is_negative i then meet i (neg r)
    else meet i (join r (neg r))

  (* r = i1+i2 => i1 = r-i2 /\ i2 = r-i1 *)
  let filter_add (i1 : t) (i2 : t) (r : t) : (t * t) option =
    Tools.merge_bot (meet i1 (sub r i2)) (meet i2 (sub r i1))

  (* r = i1-i2 => i1 = i2+r /\ i2 = i1-r *)
  let filter_sub (i1 : t) (i2 : t) (r : t) : (t * t) option =
    Tools.merge_bot (meet i1 (add i2 r)) (meet i2 (sub i1 r))

  (* r = i1*i2 => (i1 = r/i2 \/ i2=r=0) /\ (i2 = r/i1 \/ i1=r=0) *)
  let filter_mul (i1 : t) (i2 : t) (r : t) : (t * t) option =
    Tools.merge_bot
      ( if contains_float r 0. && contains_float i2 0. then Some i1
      else Option.bind (div r i2) (meet i1) )
      ( if contains_float r 0. && contains_float i1 0. then Some i2
      else Option.bind (div r i1) (meet i2) )

  (* r = i1/i2 => i1 = i2*r /\ (i2 = i1/r \/ i1=r=0) *)
  let filter_div (i1 : t) (i2 : t) (r : t) : (t * t) option =
    Tools.merge_bot
      (meet i1 (mul i2 r))
      ( if contains_float r 0. && contains_float i1 0. then Some i2
      else Option.bind (div i1 r) (meet i2) )

  (* r = sqrt i => i = r*r or i < 0 *)
  let filter_sqrt (i : t) (r : t) : t option =
    let rr = mul r r in
    if is_positive i then meet i rr else meet i (join i rr)

  (* r = exp i => i = ln r *)
  let filter_exp (i : t) (r : t) : t option = Option.bind (ln r) (meet i)

  (* r = ln i => i = exp r *)
  let filter_ln (i : t) (r : t) : t option = meet i (exp r)

  (* r = log i => i = *)
  let filter_log _ = failwith "todo filter_log"

  (* r = i ** n => i = nroot r *)
  let filter_pow (i : t) n (r : t) =
    Tools.(merge_bot (Option.bind (n_root r n) (meet i)) (Some n))

  (* r = nroot i => i = r ** n *)
  let filter_root i r n = Tools.(merge_bot (meet i (pow r n)) (Some n))

  (* r = min (i1, i2) => i1 = *)
  let filter_min i1 i2 _r = Some (i1, i2)

  (* r = max (i1, i2) *)
  let filter_max i1 i2 _r = Some (i1, i2)

  let filter_fun name args r : t list option =
    let arity_1 (f : t -> t -> t option) : t list option =
      match args with
      | [i] -> f i r |> Option.map (fun i -> [i])
      | _ -> failwith (Format.sprintf "%s expect one argument" name)
    in
    let arity_2 (f : t -> t -> t -> (t * t) option) : t list option =
      match args with
      | [i1; i2] -> f i1 i2 r |> Option.map (fun (i1, i2) -> [i1; i2])
      | _ -> failwith (Format.sprintf "%s expect two arguments" name)
    in
    match name with
    | "sqrt" -> arity_1 filter_sqrt
    | "exp" -> arity_1 filter_exp
    | "ln" -> arity_1 filter_ln
    | "max" -> arity_2 filter_max
    | "min" -> arity_2 filter_min
    | s -> failwith (Format.sprintf "unknown filter function : %s" s)
end
