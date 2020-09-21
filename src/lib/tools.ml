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

(**********************)
(** {1} Map instances *)
(**********************)

(** only one instanciation forall variable maps modules *)
module VarMap = struct
  include Map.Make(String)

  (** fails directly with an error msg instead of raising Not_found *)
  let find_fail key map =
    try find key map
    with Not_found -> fail_fmt "variable not found: %s" key

  (** builds a map from an association list*)
  let of_list (assoc: (string*'a) list) =
    List.fold_left (fun acc (k,m) -> add k m acc) empty assoc
end

module VarSet = Set.Make(String)
