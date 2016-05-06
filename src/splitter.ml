open ADCP

(* Consistency computation and splitting strategy handling *)
module Make (Abs : AbstractCP) = struct

  type consistency = Full of Abs.t 
		     | Maybe of Abs.t * Syntax.bexpr list
		     | Empty

  let consistency abs constrs : consistency =
    try
      let abs' = List.fold_left Abs.meet abs constrs in
      let unsat = List.filter (fun c -> not (Abs.sat_cons abs' c)) constrs in
      match unsat with
      | [] -> Full abs'
      | _ -> if Abs.is_bottom abs' then Empty else Maybe(abs', unsat)
    with Bot.Bot_found -> Empty
	
  let split abs expr = Abs.split abs expr

end
