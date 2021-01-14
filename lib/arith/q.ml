type t = Mpqf.t

(** {1 Constructors} *)

let zero = Mpqf.of_int 0

let one = Mpqf.of_int 1

let two = Mpqf.of_int 2

let minus_one = Mpqf.of_int (-1)

(** {1 Operators} *)

let add = Mpqf.add

let mul = Mpqf.mul

let div x y = if not (Mpqf.equal zero y) then Some (Mpqf.div x y) else None

let neg x = Mpqf.neg x

let ceil x = x |> Mpqf.to_float |> ceil |> int_of_float

let floor x = x |> Mpqf.to_float |> floor |> int_of_float

(** {1 Conversions} *)
let to_int x =
  let xi = int_of_float (Mpqf.to_float x) in
  if Mpqf.equal x (Mpqf.of_int xi) then Some xi else None

let to_float = Mpqf.to_float

let to_rational : t -> t = Fun.id

let of_int = Mpqf.of_int

let of_float = Mpqf.of_float

let of_rational = Fun.id

(** {1 Comparisons} *)

let equal = Mpqf.equal

let compare = Mpqf.cmp

(** {1 Printing} *)

(** Mpqf human understandable printing when denominator is a power of ten *)
let pp_print_size_of_0_suffix =
  let bump_0 str n =
    let ls = String.length str in
    if ls > n then
      let integer_part = String.sub str 0 (ls - n) in
      let decimal_part = String.sub str (ls - n) n in
      integer_part ^ "." ^ decimal_part
    else
      let zeros = String.make (n - ls) '0' in
      "0." ^ zeros ^ str
  in
  let rec retry (mpqf : Mpqf.t) : string =
    if Mpqf.sgn mpqf < 0 then "-" ^ retry (Mpqf.neg mpqf)
    else
      let num, den = Mpqf.to_mpzf2 mpqf in
      let den_str = Mpzf.to_string den in
      let num_str = Mpzf.to_string num in
      if den = Mpzf.of_int 1 then num_str
      else if den_str.[0] = '1' then (
        let cpt = ref 0 in
        for i = String.length den_str - 1 downto 1 do
          if den_str.[i] = '0' then incr cpt
        done ;
        if !cpt = String.length den_str - 1 then bump_0 num_str !cpt
        else Mpqf.to_string mpqf )
      else Mpqf.to_string mpqf
  in
  retry

(** Mpqf light printing when convertible to a float *)
let pp_print fmt (m : Mpqf.t) =
  let f = Mpqf.to_float m in
  if Mpqf.of_float f = m then F.pp_print fmt f
  else Format.fprintf fmt "%s" (pp_print_size_of_0_suffix m)

let print = pp_print

let to_string = Mpqf.to_string
