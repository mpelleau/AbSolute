open Bench_desc_t
open Bound_sig

let make_bound : bound -> (module BOUND) = function
| `Rational -> (module Bound_rat)
| `Integer -> (module Bound_int)
| `Float -> (module Bound_float)
