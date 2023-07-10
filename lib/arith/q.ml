(** Rational arithmetic module *)

type t = Mpqf.t

(** {1 Constructors} *)

let zero : t = Mpqf.of_int 0

let one : t = Mpqf.of_int 1

let two : t = Mpqf.of_int 2

let minus_one : t = Mpqf.of_int (-1)

(** {1 Operators} *)

let add : t -> t -> t = Mpqf.add

let sub : t -> t -> t = Mpqf.sub

let mul : t -> t -> t = Mpqf.mul

let div (x : t) (y : t) : t option =
  if not (Mpqf.equal zero y) then Some (Mpqf.div x y) else None

let pow : t -> int -> t =
  let rec pow a = function
    | 0 -> one
    | 1 -> a
    | n ->
        let b = pow a (n / 2) in
        mul (mul b b) (if n mod 2 = 0 then one else a)
  in
  fun q n -> if n < 0 then Mpqf.div one (pow q (-n)) else pow q n

let neg (x : t) : t = Mpqf.neg x

let abs (x : t) : t = Mpqf.abs x

let ceil (x : t) = x |> Mpqf.to_float |> ceil |> int_of_float

let floor (x : t) = x |> Mpqf.to_float |> floor |> int_of_float

(** {1 Conversions} *)
let to_int (x : t) =
  let xi = int_of_float (Mpqf.to_float x) in
  if Mpqf.equal x (Mpqf.of_int xi) then Some xi else None

let to_float : t -> float = Mpqf.to_float

let to_rational : t -> t = Fun.id

let of_int : int -> t = Mpqf.of_int

let of_float : float -> t = Mpqf.of_float

let of_rational = Fun.id

(** {1 Comparisons} *)

let equal : t -> t -> bool = Mpqf.equal

let compare : t -> t -> int = Mpqf.cmp

(** {1 Printing} *)

(** Mpqf human understandable printing when denominator is a power of ten *)
let pp_print_size_of_0_suffix : t -> string =
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
let pp_print fmt (m : t) =
  let f = Mpqf.to_float m in
  if Mpqf.of_float f = m then F.pp_print fmt f
  else Format.fprintf fmt "%s" (pp_print_size_of_0_suffix m)

let print = pp_print

let to_string : t -> string = Format.asprintf "%a" pp_print
