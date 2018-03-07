(* only one instanciation forall variable maps modules *)
module VarMap = struct
  include Mapext.Make(struct type t = string let compare = compare end)

  (* we add few utilities inside it *)

  (* builds a map from an association list*)
  let of_list (assoc: (string*'a) list) =
    List.fold_left (fun acc (k,m) -> add k m acc) empty assoc

end
