open Apron

type solving = 
  Environment.t * Lincons1.earray * Tcons1.t list list * Tcons1.earray * Tcons1.earray * Tcons1.earray

type minimization = 
  Environment.t * Lincons1.earray * Tcons1.t list list * Tcons1.earray * Texpr1.t

module type H = sig
  type t
  val handle : t -> unit
end
module Handler (A:H)(Abs:ADCP.AbstractCP) = struct 
  let man = Abs.get_manager

  let consistency abs tab max_iter =
    let rec cons_loop abs n =
      if n >= max_iter || Abstract1.is_bottom man abs then abs
      else(
        let abs_tmp = Abstract1.copy man abs in
        Abstract1.meet_tcons_array_with man abs tab;		
        if Abstract1.is_eq man abs_tmp abs then abs
        else cons_loop abs (n+1)
      )
    in cons_loop abs 0
end



                (*****************************)
                (*    PROBLEMS INSTANCES     *)
                (*****************************)

                     (* SOLVING PROBLEMS *)

(* Gear4: best known solution=1.643428; eps=0.5e-6; prec=1.0e-1 *)
let gear4 : solving =
  let integers = Array.map Var.of_string [|"x1";"x2";"x3";"x4"|]
  and reals = Array.map Var.of_string [|"x5";"x6"|] in
  let env = Environment.make integers reals in
  (*Domains from N. Berger*)
  let domains = Parser.lincons1_of_lstring env ["x5>=0"; "x5<=100"; "x6>=0"; "x6<=100"; "x1>=10"; "x1<=25"; "x2>=0"; "x2<=20"; "x3>=37"; "x3<=50"; "x4>=45"; "x4<=55"] in
  (*Domains from MinLP
    let domains = Parser.lincons1_of_lstring env ["x5>=0"; "x6>=0"; "x1>=12"; "x1<=60"; "x2>=12"; "x2<=60"; "x3>=12"; "x3<=60"; "x4>=12"; "x4<=60"] in*)
  let list = [["-1000000*x1*x2/(x3*x4) - x5 + x6 > -14279.32477276"; "-1000000*x1*x2/(x3*x4) - x5 + x6 < -14279.32477276"; "-x5 - x6 < -1.643433"; "-x5 - x6 > -1.643423"]] in
  let tab = ["-1000000*x1*x2/(x3*x4) - x5 + x6 = -14279.32477276"; "-x5 - x6 >= -1.643433"; "-x5 - x6 <= -1.643423"] in
  let tab_lin = ["-x5 - x6 >= -1.643433"; "-x5 - x6 <= -1.643423"] in
  let tab_non_lin = ["-1000000*x1*x2/(x3*x4) - x5 + x6 = -14279.32477276"] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) list
  and cons' = Parser.tcons1_of_lstring env tab
  and cons_lin = Parser.tcons1_of_lstring env tab_lin
  and cons_non_lin = Parser.tcons1_of_lstring env tab_non_lin in
  (env, domains, cons, cons', cons_non_lin, cons_lin)


(* st_miqp5: best known solution=-333.888889; eps=1; prec=1.0e-1 *)
let st_miqp5:solving =
  let integers = Array.map Var.of_string [|"x1";"x2"|]
  and reals = Array.map Var.of_string [|"x3";"x4";"x5";"x6";"x7"|] in
  let env = Environment.make integers reals in
  (*Domains from N. Berger
    let domains = Parser.lincons1_of_lstring env ["x1>=-10000"; "x1<=1"; "x2>=-10000"; "x2<=1"; "x3>=-7.24380468458"; "x3<=22.6826188429"; "x4>=-6.0023781122"; "x4<=3.80464419615"; "x5>=-100.797166188733"; "x5<=1200.5189336042"; "x6>=-800.75189948987"; "x6<=1400.5864991498"; "x7>=800.98296319621e-17"; "x7<=1900.4187214575"] in*)
  (*Domains from MinLP*)
  let domains = Parser.lincons1_of_lstring env ["x1<=1"; "x2<=1"; "x3>=-7.24380468458"; "x3<=22.6826188429"; "x4>=-6.0023781122"; "x4<=3.80464419615"; "x5>=-1.797166188733"; "x5<=11.5189336042"; "x6>=-8.75189948987"; "x6<=14.5864991498"; "x7>=8.98296319621e-17"; "x7<=19.4187214575"] in
  let list = [["-1.93414531698*x3 + 1.80314509442*x4 + 2.89695789508*x5 + 0.729324957489*x6 + 3.8837442915*x7 > 60"; "- 1.13150591228*x3 + 1.10500971967*x4 - 1.01838569726*x5 + 2.62556984696*x6 + 4.85468036438*x7 > 60"; "-0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 > 0"; "0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 > 1"; "0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 > 0"; "-0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 > 1"; "-0.328188665272*x3 + 0.199986646277*x4 + 0.506106406938*x5 - 0.583459965992*x6 + 0.505695871289*x7 < 0"; "-0.345682002598*x3 - 0.101625962101*x4 + 0.57594668021*x5 + 0.729324957489*x6 + 0.0809113394063*x7 < 0"; "0.756087294764*x3 - 0.200079270407*x4 + 0.151379235251*x5 + 0.145864991498*x6 + 0.586607210695*x7 < 0"; "-x1 + 0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 > 0"; "x1 - 0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 > 0"; "-x2 - 0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 > 0"; "x2 + 0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 > 0"; "-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 < 332.888889"; "-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 > 334.888889"]] in
  let tab = ["-1.93414531698*x3 + 1.80314509442*x4 + 2.89695789508*x5 + 0.729324957489*x6 + 3.8837442915*x7 <= 60"; "- 1.13150591228*x3 + 1.10500971967*x4 - 1.01838569726*x5 + 2.62556984696*x6 + 4.85468036438*x7 <= 60"; "-0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 <= 0"; "0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 <= 1"; "0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 <= 0"; "-0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 <= 1"; "-0.328188665272*x3 + 0.199986646277*x4 + 0.506106406938*x5 - 0.583459965992*x6 + 0.505695871289*x7 >= 0"; "-0.345682002598*x3 - 0.101625962101*x4 + 0.57594668021*x5 + 0.729324957489*x6 + 0.0809113394063*x7 >= 0"; "0.756087294764*x3 - 0.200079270407*x4 + 0.151379235251*x5 + 0.145864991498*x6 + 0.586607210695*x7 >= 0"; "-x1 + 0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 <= 0"; "x1 - 0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 <= 0"; "-x2 - 0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 <= 0"; "x2 + 0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 <= 0"; "-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 >= 332.888889"; "-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 <= 334.888889"] in
  let tab_lin = ["-1.93414531698*x3 + 1.80314509442*x4 + 2.89695789508*x5 + 0.729324957489*x6 + 3.8837442915*x7 <= 60"; "- 1.13150591228*x3 + 1.10500971967*x4 - 1.01838569726*x5 + 2.62556984696*x6 + 4.85468036438*x7 <= 60"; "-0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 <= 0"; "0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 <= 1"; "0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 <= 0"; "-0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 <= 1"; "-0.328188665272*x3 + 0.199986646277*x4 + 0.506106406938*x5 - 0.583459965992*x6 + 0.505695871289*x7 >= 0"; "-0.345682002598*x3 - 0.101625962101*x4 + 0.57594668021*x5 + 0.729324957489*x6 + 0.0809113394063*x7 >= 0"; "0.756087294764*x3 - 0.200079270407*x4 + 0.151379235251*x5 + 0.145864991498*x6 + 0.586607210695*x7 >= 0"; "-x1 + 0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 <= 0"; "x1 - 0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 <= 0"; "-x2 - 0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 <= 0"; "x2 + 0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 <= 0"] in
  let tab_non_lin = ["-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 >= 332.888889"; "-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 <= 334.888889"] in
  (*let tab = ["-(5*x6^2 - 0.875189948987*x6 + 52*x7^2 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 >= 332.888889"; "-(5*x6^2 - 0.875189948987*x6 + 52*x7^2 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 <= 334.888889"; "-1.93414531698*x3 + 1.80314509442*x4 + 2.89695789508*x5 + 0.729324957489*x6 + 3.8837442915*x7 <= 60"; "- 1.13150591228*x3 + 1.10500971967*x4 - 1.01838569726*x5 + 2.62556984696*x6 + 4.85468036438*x7 <= 60"; "-0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 <= 0"; "0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 <= 1"; "0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 <= 0"; "-0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 <= 1"; "-0.328188665272*x3 + 0.199986646277*x4 + 0.506106406938*x5 - 0.583459965992*x6 + 0.505695871289*x7 >= 0"; "-0.345682002598*x3 - 0.101625962101*x4 + 0.57594668021*x5 + 0.729324957489*x6 + 0.0809113394063*x7 >= 0"; "0.756087294764*x3 - 0.200079270407*x4 + 0.151379235251*x5 + 0.145864991498*x6 + 0.586607210695*x7 >= 0"; "-x1 + 0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 <= 0"; "x1 - 0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 <= 0"; "-x2 - 0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 <= 0"; "x2 + 0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 <= 0"; "-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 >= 332.888889"; "-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 <= 334.888889"] in*)
  let cons = List.map (List.map (Parser.tcons1_of_string env)) list 
  and cons' = Parser.tcons1_of_lstring env tab
  and cons_lin = Parser.tcons1_of_lstring env tab_lin
  and cons_non_lin = Parser.tcons1_of_lstring env tab_non_lin in
  (env, domains, cons, cons', cons_non_lin, cons_lin)

let lin1:solving =
  let x = Var.of_string "x" and y = Var.of_string "y" in
  let env = Environment.make [||] [|x;y|]  in
  let domains = Parser.lincons1_of_lstring env ["x>=0"; "x<=50";"y>=0"; "y<=50"] 
  and tab = [
    "x>=0"; "x<=50";"y>=0"; "y<=50";
    "y-x < 40";
    "y-x > -40";
    "y+x < 90";
    "y+x > 10" 
  ] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) [tab] 
  and cons' = Parser.tcons1_of_lstring env tab 
  and cons_lin = Parser.tcons1_of_lstring env tab
  and cons_non_lin = Parser.tcons1_of_lstring env [] in
  (env, domains, cons, cons', cons_non_lin, cons_lin)

let lin2 = lin1

let nonlin1:solving =
  let x = Var.of_string "x" and y = Var.of_string "y" in
  let env = Environment.make [||] [|x;y|] in
  let domains = Parser.lincons1_of_lstring env ["x>=0"; "x<=5";"y>=0"; "y<=5"] 
  and tab = ["(y-103.5)^2+(x-2.5)^2>10000";
	     "(x+8)^2+(y+8)^2>202";
	     "(x-23.5)^2+(y-2.5)^2>400"] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) [tab] 
  and cons' = Parser.tcons1_of_lstring env tab
  and cons_lin = Parser.tcons1_of_lstring env []
  and cons_non_lin = Parser.tcons1_of_lstring env tab in
  (env, domains, cons, cons', cons_non_lin, cons_lin)

let nonlin2:solving = 
  (**Problème Difficile de racines*)
  let x1 = Var.of_string "x1" and x2 = Var.of_string "x2" in
  let env = Environment.make [||] [|x1;x2|] in
  let domains = Parser.lincons1_of_lstring env ["x1>=0"; "x1<=5";"x2>=0"; "x2<=5"]
  and tab = ["0.5*(x1-2.5)*(x1-2.5)+(x2-2.5)*(x2-2.5)>1";
	     "sqrt(x1)-x2>-2.5";"-sqrt(x1)-x2<-2.5";
	     "sqrt(5-x1)-x2>-2.5";
	     "-sqrt(5-x1)-x2<-2.5"] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) [tab] 
  and cons' = Parser.tcons1_of_lstring env tab
  and cons_lin = Parser.tcons1_of_lstring env []
  and cons_non_lin = Parser.tcons1_of_lstring env tab in
  (env, domains, cons, cons', cons_non_lin, cons_lin)

let one_circle:solving = 
  let x = Var.of_string "x" and y = Var.of_string "y" in
  let env = Environment.make [||] [|x;y|] in
  let domains = Parser.lincons1_of_lstring env ["x>=0"; "x<=40";"y>=0"; "y<=40"]
  and tab = [
    "(x-20)^2 + (y-20)^2 <= 400"
  ] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) [tab] 
  and cons' = Parser.tcons1_of_lstring env tab
  and cons_lin = Parser.tcons1_of_lstring env []
  and cons_non_lin = Parser.tcons1_of_lstring env tab in
  (env, domains, cons, cons', cons_non_lin, cons_lin)

let two_circles:solving = 
  (*Deux Cercles *)
  let x = Var.of_string "x" and y = Var.of_string "y" in
  let env = Environment.make [||] [|x;y|] in
  let domains = Parser.lincons1_of_lstring env ["x>=0"; "x<=40";"y>=0"; "y<=40"]
  and tab = [
    "(x-20)^2 + (y-20)^2 <= 400";
    "(x)^2 + (y)^2 <= 1000"
  ] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) [tab] 
  and cons' = Parser.tcons1_of_lstring env tab
  and cons_lin = Parser.tcons1_of_lstring env []
  and cons_non_lin = Parser.tcons1_of_lstring env tab in
  (env, domains, cons, cons', cons_non_lin, cons_lin) 

let cpr2:solving =
(*CPR2-ANI-10-10*)
  let b = Array.make 10 "" in 
  Array.iteri (fun i x-> b.(i)<-"x"^(string_of_int (1+i))) b;
  let t = Array.map Var.of_string b in
  let env = Environment.make [||] t in
  let domains = 
    (Array.map (fun s -> [s^">=-1"; s^"<=1"]) b) |>
	Array.to_list |> List.concat |>
	    Parser.lincons1_of_lstring env 
  in
  let tab = [
    "x2 + 2*x6 + x9 + 2*x10 - 1.0E-5 = 0";
    "x3 + x8 - 3.0E-5 = 0";
    "x1 + x3 + 2*x5 + 2*x8 + x9 + x10 - 5.0E-5 = 0";
    "x4 + 2*x7 - 1.0E-5 = 0";
    "0.5140437E-7 * x5 - x1^2 = 0";
    "0.1006932E-6 * x6 - x2^2 = 0";
    "0.7816278E-15 * x7 - x4^2 = 0";
    "0.1496236E-6 * x8 - x1*x3 = 0";
    "0.6194411E-7 * x9 - x1*x2 = 0";
    "0.2089296E-14 * x10 - x1*x2^2 = 0"] in
  let tab_lin = [
    "x2 + 2*x6 + x9 + 2*x10 - 1.0E-5 = 0";
    "x3 + x8 - 3.0E-5 = 0";
    "x1 + x3 + 2*x5 + 2*x8 + x9 + x10 - 5.0E-5 = 0";
    "x4 + 2*x7 - 1.0E-5 = 0"] in
  let tab_non_lin = [
    "0.5140437E-7 * x5 - x1^2 = 0";
    "0.1006932E-6 * x6 - x2^2 = 0";
    "0.7816278E-15 * x7 - x4^2 = 0";
    "0.1496236E-6 * x8 - x1*x3 = 0";
    "0.6194411E-7 * x9 - x1*x2 = 0";
    "0.2089296E-14 * x10 - x1*x2^2 = 0"] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) [tab] 
  and cons' = Parser.tcons1_of_lstring env tab
  and cons_lin = Parser.tcons1_of_lstring env tab_lin
  and cons_non_lin = Parser.tcons1_of_lstring env tab_non_lin in
  (env, domains, cons, cons', cons_non_lin, cons_lin)


let octo_hole:solving =
  let x1 = Var.of_string "x1" and x2 = Var.of_string "x2" in
  let env = Environment.make [||] [|x1;x2|] in
  let domains = Parser.lincons1_of_lstring env ["x1>=0"; "x1<=5";"x2>=0"; "x2<=5"] in
  let tab = ["x2>=-x1+3";"x1<=4" ;"x1>=1"; "x2>=1";"x2<=4"; "x2<=x1+2";"x1<=x2+2"; "(x1-2.5)*(x1-2.5)+(x2-2.5)*(x2-2.5)>0.1"]
  and tab_lin = ["x2>=-x1+3";"x1<=4" ;"x1>=1"; "x2>=1";"x2<=4"; "x2<=x1+2";"x1<=x2+2"]
  and tab_non_lin = ["(x1-2.5)*(x1-2.5)+(x2-2.5)*(x2-2.5)>0.1"] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) [tab] 
  and cons' = Parser.tcons1_of_lstring env tab
  and cons_lin = Parser.tcons1_of_lstring env tab_lin
  and cons_non_lin = Parser.tcons1_of_lstring env tab_non_lin in
  (env, domains, cons, cons', cons_non_lin, cons_lin)


                      (* MINIMIZATION PROBLEMS *)

let test:minimization =    
  let x1 = Var.of_string "x1" and x2 = Var.of_string "x2" in
  let env = Environment.make [|x1|] [|x2|] in
  let domains = Parser.lincons1_of_lstring env ["x1>=1"; "x1<=5"; "x2>=1"; "x2<=5"] in
  let list = [["x2 - x1 > 2"; "x1 + x2 < 3"; "x1 - x2 > 2.5"]] in
  let tab = ["x2 - x1 <= 2"; "x1 + x2 >= 3"; "x1 - x2 <= 2.5"] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) list in
  let cons' = Parser.tcons1_of_lstring env tab in
  let obj = Parser.texpr1_of_string env "x1 + 2*x2" in
  (env, domains, cons, cons', obj)
