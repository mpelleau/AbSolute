(** A small set of useful utilities *)

(***********************)
(** {1} Printing stuff *)
(***********************)

(** same as failwith but uses a format instead *)
let fail_fmt fmt = Format.kasprintf failwith fmt

(** terminal output with a color given in parameter
  restoring default color after use *)
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

(** printing that erases previous output. should not be
   intertwined with orher prints *)
let inplace_print () =
  let size_last = ref 0 in
  (fun fmt s ->
    let erase = String.make !size_last '\b' in
    size_last := String.length s;
    (Format.fprintf fmt "%s%s" erase s))

(** 2D table print indentation *)
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

 (** Mpqf human understandable printing when denominator is a power of ten *)
let pp_print_size_of_0_suffix =
  let bump_0 str n =
    let ls = String.length str in
    if ls > n then
      let integer_part = String.sub str 0 (ls-n) in
      let decimal_part = String.sub str (ls-n) n in
      integer_part^"."^decimal_part
    else
      let zeros = String.make (n-ls) '0' in
      "0."^zeros^str
  in
  let rec retry (mpqf: Mpqf.t) : string =
    if Mpqf.sgn mpqf < 0 then "-"^(retry (Mpqf.neg mpqf))
    else
      let num,den = Mpqf.to_mpzf2 mpqf in
      let den_str = Mpzf.to_string den in
      let num_str = Mpzf.to_string num in
      if den = Mpzf.of_int 1 then num_str
      else if den_str.[0] = '1' then
        let cpt = ref 0 in
        for i = String.length den_str - 1 downto 1 do
          if den_str.[i] = '0' then incr cpt
        done;
        if !cpt = String.length den_str - 1 then bump_0 num_str !cpt
        else Mpqf.to_string mpqf
      else Mpqf.to_string mpqf
  in
  retry

(** float light printing when decimal part is nul *)
let pp_print_float fmt (f:float) =
  let i = int_of_float f in
  if float i = f then
    Format.fprintf fmt "%i" i
  else Format.pp_print_float fmt f

(** Mpqf light printing when convertible to a float *)
let pp_print_mpqf fmt (m:Mpqf.t) =
  let f = Mpqf.to_float m in
  if Mpqf.of_float f = m then pp_print_float fmt f
  else Format.fprintf fmt "%s" (pp_print_size_of_0_suffix m)

(** debug utility that indents according to the debug level *)
let debug level fmt =
  if !Constant.debug < level then
    Format.ikfprintf ignore Format.std_formatter fmt
  else
    let spacing = String.make level ' ' in
    Format.kasprintf (fun msg -> Format.printf "%s%s%s" spacing spacing msg) fmt

(** semi_colon separator *)
let semi_colon_sep fmt () = Format.fprintf fmt ";"

(** comma separator *)
let comma_sep fmt () = Format.fprintf fmt ","

(** newline separator *)
let newline_sep fmt () = Format.fprintf fmt "\n"

(** misc *)
let swap_pair (a,b) = (b,a)

(** folder on all possible combinations of size 2 of an array *)
let fold_on_combination_2 ?duplicate:(d=false) f acc arr =
  let l = Array.length arr in
  let rec aux res i1 i2 =
    if i1 >= l then res
    else if i2 = l then let n = i1+1 in aux res n n
    else if i2 = i1 && (not d) then aux res i1 (i2+1)
    else aux (f res arr.(i1) arr.(i2)) i1 (i2+1)
  in
  aux acc 0 0

(** meet on options *)
let meet_bot f x y =
  Option.map (fun x -> (Option.map (fun y -> f x y) y)) x

(** merge on options *)
let merge_bot x y =
  Option.bind x (fun x -> (Option.map (fun y -> (x,y)) y))

(** join on options *)
let join_bot2 f x y =
  match x,y with
  | None,a | a,None -> a
  | Some a,Some b -> Some (f a b)

(** {1} Map instances *)

(** only one instanciation forall variable maps modules *)
module VarMap = struct
  include Map.Make(String)

  (** fails directly with an error msg instead of raising Not_found *)
  let find_fail key map =
    try find key map
    with Not_found -> fail_fmt "key not found: %s" key

  (** builds a map from an association list*)
  let of_list (assoc : (key * 'a) list) =
    List.fold_left (fun acc (k,m) -> add k m acc) empty assoc
end

module VarSet = Set.Make(String)
