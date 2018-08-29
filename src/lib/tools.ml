(******************)
(* Printing stuff *)
(******************)

(* same as failwith but uses a format instead *)
let fail_fmt fmt = Format.kasprintf (fun s -> failwith s) fmt

(* terminal output with a color given in parameter *)
(* restoring default color after use *)
let color_printf fmt col x =
  Format.kasprintf (fun s -> Format.fprintf fmt "%s%s%s" col s "\027[0m") x

(* red terminal output *)
let red_fprintf fmt x = color_printf fmt "\027[31m" x

(* green terminal output *)
let green_fprintf fmt x = color_printf fmt "\027[32m" x

(* yellow terminal output *)
let yellow_fprintf fmt x = color_printf fmt "\027[33m" x

(* 2D table print indentation *)
let matrix_print_indent fmt mat =
  let sizes = Array.make (Array.length mat.(0)) 0 in
  for i = 0 to  Array.length mat.(0) -1 do
    let maxsize = ref 0 in
    for j = 0 to  Array.length mat -1 do
      maxsize := max (!maxsize) (String.length mat.(j).(i));
    done;
    sizes.(i) <- !maxsize;
  done;
  for i = 0 to  Array.length mat -1 do
    for j = 0 to  Array.length mat.(0) -1 do
      let dif = sizes.(j) - (String.length mat.(i).(j)) in
      let fill = String.make dif ' ' in
      Format.fprintf fmt "%s%s " mat.(i).(j) fill
    done;
    Format.fprintf fmt "\n"
  done

(******************************)
(* printing for debug purpose *)
(******************************)

(* debug utility that indents according to the debug level *)
let debug i fmt =
  let spacing = String.make i ' ' in
  Format.ksprintf (Format.printf "%s%s%s%!" spacing spacing) fmt

(*****************)
(* Map instances *)
(*****************)

(* only one instanciation forall variable maps modules *)
module VarMap = struct
  include Mapext.Make(struct type t = string let compare = compare end)

  (* we add few utilities inside it *)

  (* we define a find_fail that fails directly with an error msg
     when a variable is not found *)
  let find_fail key map =
    try find key map
    with Not_found -> fail_fmt "variable not found: %s" key

  (* we define a find_fail that fails directly with an error msg
     when a variable is not found *)
  let find_opt key map =
    try Some (find key map)
    with Not_found -> None

  (* builds a map from an association list*)
  let of_list (assoc: (string*'a) list) =
    List.fold_left (fun acc (k,m) -> add k m acc) empty assoc

end
