(* Some utilities to print names with argument of functions.  *)

let tname2 (x,y) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"
let tfname2 (x,y) = "(" ^ string_of_float x ^ "," ^ string_of_float y ^ ")"
let fname name (arg1: int) = name ^ " " ^ string_of_int arg1
let fname2 name (arg1: int) (arg2: int) = fname name arg1 ^ " " ^ string_of_int arg2
let string_of_key (v, plane) = string_of_int v ^ " " ^ tname2 plane
let fname_key name k = name ^ " " ^ string_of_key k
