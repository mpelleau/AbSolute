(*
  An abstract fixpoint solver based on Constraint Programming
  
  Author: Antoine Mine
  Copyright 2014
*)

(*
  Adds a bottom element to a data-type.
*)


type 'a bot = Bot | Nb of 'a

let strict_bot f x = 
  match x with Bot -> Bot | Nb x -> f x

let lift_bot f x = 
  match x with Bot -> Bot | Nb x -> Nb (f x)
      
let merge_bot2 x y = 
  match x,y with Bot,_ | _,Bot -> Bot | Nb a, Nb b -> Nb (a,b)

let join_bot2 f x y = 
  match x,y with Bot,a | a,Bot -> a | Nb a,Nb b -> Nb (f a b)

let meet_bot2 f x y =
  match x,y with Bot, _ | _, Bot -> Bot | Nb a, Nb b -> Nb (f a b)

let meet_bot f x y =
  match y with Bot -> Bot | Nb a -> f x a


let nobot = 
  function Nb x -> x | Bot -> failwith "unexpected bottom encountered"


exception Bot_found

let debot = 
  function Nb x -> x | Bot -> raise Bot_found

let rebot f x = 
  try Nb (f x) with Bot_found -> Bot

let bot_to_string f = function Bot -> "_|_" | Nb x -> f x

let is_Bot = function
  | Bot -> true
  | _ -> false
