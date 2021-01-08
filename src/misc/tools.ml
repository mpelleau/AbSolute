(** A small set of useful utilities *)

(** {1} Printing stuff *)

(** same as failwith but uses a format instead *)
let fail_fmt fmt = Format.kasprintf failwith fmt

(** terminal output with a color given in parameter restoring default color
    after use *)
let color_printf fmt col x =
  Format.kasprintf (fun s -> Format.fprintf fmt "%s%s%s" col s "\027[0m") x

(** red terminal output *)
let red_fprintf fmt x = color_printf fmt "\027[31m" x

(** blue terminal output *)
let cyan_fprintf fmt x = color_printf fmt "\027[36m" x

(** green terminal output *)
let green_fprintf fmt x = color_printf fmt "\027[32m" x

(** yellow terminal output *)
let yellow_fprintf fmt x = color_printf fmt "\027[33m" x

(** printing that erases previous output. should not be intertwined with orher
    prints *)
let inplace_print () =
  let size_last = ref 0 in
  fun fmt s ->
    let erase = String.make !size_last '\b' in
    size_last := String.length s ;
    Format.fprintf fmt "%s%s" erase s

(** 2D table print indentation *)
let matrix_print_indent fmt mat =
  let sizes = Array.make (Array.length mat.(0)) 0 in
  for i = 0 to Array.length mat.(0) - 1 do
    let maxsize = ref 0 in
    for j = 0 to Array.length mat - 1 do
      maxsize := max !maxsize (String.length mat.(j).(i))
    done ;
    sizes.(i) <- !maxsize
  done ;
  for i = 0 to Array.length mat - 1 do
    for j = 0 to Array.length mat.(0) - 1 do
      let dif = sizes.(j) - String.length mat.(i).(j) in
      let fill = String.make dif ' ' in
      Format.fprintf fmt "%s%s " mat.(i).(j) fill
    done ;
    Format.fprintf fmt "\n"
  done

(** semi_colon separator *)
let semi_colon_sep fmt () = Format.fprintf fmt ";"

(** comma separator *)
let comma_sep fmt () = Format.fprintf fmt ","

(** newline separator *)
let newline_sep fmt () = Format.fprintf fmt "\n"

(** misc *)
let swap_pair (a, b) = (b, a)

(** folder on all possible combinations of size 2 of an array *)
let fold_on_combination_2 ?duplicate:(d = false) f acc arr =
  let l = Array.length arr in
  let rec aux res i1 i2 =
    if i1 >= l then res
    else if i2 = l then
      let n = i1 + 1 in
      aux res n n
    else if i2 = i1 && not d then aux res i1 (i2 + 1)
    else aux (f res arr.(i1) arr.(i2)) i1 (i2 + 1)
  in
  aux acc 0 0

(** meet on options *)
let meet_bot f x y = Option.bind x (fun x -> Option.map (fun y -> f x y) y)

(** merge on options *)
let merge_bot x y = Option.bind x (fun x -> Option.map (fun y -> (x, y)) y)

(** join on options *)
let join_bot2 f x y =
  match (x, y) with None, a | a, None -> a | Some a, Some b -> Some (f a b)

(** {1} Map instances *)

(** only one instanciation forall variable maps modules *)
module VarMap = struct
  include Map.Make (String)

  (** fails directly with an error msg instead of raising Not_found *)
  let find_fail key map =
    try find key map with Not_found -> fail_fmt "key not found: %s" key

  (** builds a map from an association list*)
  let of_list (assoc : (key * 'a) list) =
    List.fold_left (fun acc (k, m) -> add k m acc) empty assoc
end

module VarSet = Set.Make (String)
