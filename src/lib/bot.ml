type 'a bot = Bot | Nb of 'a

let strict_bot f x =
  match x with Bot -> Bot | Nb x -> f x

let lift_bot f x =
  match x with Bot -> Bot | Nb x -> Nb (f x)

let merge_bot2 x y =
  match x,y with
  | Bot,_  | _,Bot -> Bot
  | Nb a, Nb b -> Nb (a,b)

let join_bot2 f x y =
  match x,y with
  | Bot,a | a,Bot -> a
  | Nb a,Nb b -> Nb (f a b)

let meet_bot f x y =
  match y with Bot -> Bot | Nb a -> f x a

exception Bot_found

let debot =
  function Nb x -> x | Bot -> raise Bot_found

let rebot f x =
  try Nb (f x) with Bot_found -> Bot
