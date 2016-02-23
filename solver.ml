open Apron;;
open Mpqf;;
open Format;;
open ADCP;;

let scalar_to_float = function
  | Scalar.Mpqf x -> Mpqf.to_float x
  | Scalar.Float x -> x
  | Scalar.Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Near x

let print_sol box =
  let itv = box.Abstract1.interval_array in
  printf "solution = [|[%f; %f]" (scalar_to_float (itv.(0)).Interval.inf) (scalar_to_float (itv.(0)).Interval.sup);
  for i=1 to ((Array.length itv)-1) do
    printf "; [%f; %f]" (scalar_to_float (itv.(i)).Interval.inf) (scalar_to_float (itv.(i)).Interval.sup);
  done;
  printf "|]@.";

module Solve(Abs : AbstractCP) =
  struct
    let man = Abs.get_manager
    let consistency abs tab max_iter =
      let rec cons_loop abs n =
        if n >= max_iter then
          (abs)
        else
          if Abstract1.is_bottom man abs then
            abs
          else
            (
            let abs_tmp = Abstract1.copy man abs in
            Abstract1.meet_tcons_array_with man abs tab;		
            if Abstract1.is_eq man abs_tmp abs then
              abs
            else
              cons_loop abs (n+1)
            )
      in
      let abs' = cons_loop abs 0 in
      abs'
    let rec explore abs env tab max_iter prec nb_steps nb_sol =
      let abs' = consistency abs tab max_iter in
      if Abstract1.is_bottom man abs' then
        (
        (* No solutions in this sub-tree. *)
        (nb_steps, nb_sol)
        )
      else
        (
        (* Keep on searching in this sub-tree. *)
        let (small, exprs) = Abs.is_small abs' prec in
        if small then
          (* Solution found! *)
          (
          let box = Abstract1.to_box man abs' in
          print_sol box;
          (nb_steps, nb_sol+1)
          )
        else
          (
          (* Split the next variable, heuristic *)
          let list_abs = Abs.split abs' exprs in
          List.fold_left (fun (nbe, nbsol) absi -> explore absi env tab max_iter prec (nbe+1) nbsol) (nb_steps, nb_sol) list_abs
          )
        )
    let solving env domains cons max_iter prec =
      let abs = Abs.of_lincons_array env domains in
      printf "abs = %a@." Abstract1.print abs;
      let box = Abstract1.to_box man abs in
      let tab = box.Abstract1.interval_array in
      printf "box = %a@." (print_array Interval.print) tab;
      let s = Manager.get_funopt man Manager.Funid_meet_tcons_array in
      let s' = {s with Manager.algorithm = 100;} in
      Manager.set_funopt man Manager.Funid_meet_tcons_array s';(**)
      if not (Abstract1.is_bottom man abs) then
        let (nb_steps, nb_sol) = explore abs env cons max_iter prec 1 0 in
      
        if nb_sol == 0 then
          printf "No solutions - #created nodes: %d@." nb_steps
        else
          if nb_sol == 1 then
            printf "Unique solution - #created nodes: %d@." nb_steps
          else
            printf "#solutions: %d - #created nodes: %d@." nb_sol nb_steps
      else
        printf "No Solutions - #created nodes: 0@."
  end


let x1 = Var.of_string "x1";;
let x2 = Var.of_string "x2";;
let x3 = Var.of_string "x3";;
let x4 = Var.of_string "x4";;
let x5 = Var.of_string "x5";;
let x6 = Var.of_string "x6";;
let x7 = Var.of_string "x7";;
(* Gear4: best known solution=1.643428; eps=0.5e-6; prec=1.0e-1 *)
let gear4 =    
  let env = Environment.make [|x1; x2; x3; x4|] [|x6; x7|] in
  (*Domains from N. Berger*)
  let domains = Parser.lincons1_of_lstring env ["x6>=0"; "x6<=100"; "x7>=0"; "x7<=100"; "x1>=10"; "x1<=25"; "x2>=0"; "x2<=20"; "x3>=37"; "x3<=50"; "x4>=45"; "x4<=55"] in
  (*Domains from MinLP
  let domains = Parser.lincons1_of_lstring env ["x6>=0"; "x7>=0"; "x1>=12"; "x1<=60"; "x2>=12"; "x2<=60"; "x3>=12"; "x3<=60"; "x4>=12"; "x4<=60"] in*)
  let list = [["-1000000*x1*x2/(x3*x4) - x6 + x7 > -14279.32477276"; "-1000000*x1*x2/(x3*x4) - x6 + x7 < -14279.32477276"; "-x6 - x7 < -1.643433"; "-x6 - x7 > -1.643423"]] in
  let tab = ["-1000000*x1*x2/(x3*x4) - x6 + x7 = -14279.32477276"; "-x6 - x7 >= -1.643433"; "-x6 - x7 <= -1.643423"] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) list in
  let cons' = Parser.tcons1_of_lstring env tab in
  (env, domains, cons, cons')
;; 

(* st_miqp5: best known solution=-333.888889; eps=1; prec=1.0e-1 *)
let st_miqp5 =    
  let env = Environment.make [|x1; x2|] [|x3; x4; x5; x6; x7|] in
  (*Domains from N. Berger
  let domains = Parser.lincons1_of_lstring env ["x1>=-10000"; "x1<=1"; "x2>=-10000"; "x2<=1"; "x3>=-7.24380468458"; "x3<=22.6826188429"; "x4>=-6.0023781122"; "x4<=3.80464419615"; "x5>=-100.797166188733"; "x5<=1200.5189336042"; "x6>=-800.75189948987"; "x6<=1400.5864991498"; "x7>=800.98296319621e-17"; "x7<=1900.4187214575"] in*)
  (*Domains from MinLP*)
  let domains = Parser.lincons1_of_lstring env ["x1<=1"; "x2<=1"; "x3>=-7.24380468458"; "x3<=22.6826188429"; "x4>=-6.0023781122"; "x4<=3.80464419615"; "x5>=-1.797166188733"; "x5<=11.5189336042"; "x6>=-8.75189948987"; "x6<=14.5864991498"; "x7>=8.98296319621e-17"; "x7<=19.4187214575"] in
  let list = [["-1.93414531698*x3 + 1.80314509442*x4 + 2.89695789508*x5 + 0.729324957489*x6 + 3.8837442915*x7 > 60"; "- 1.13150591228*x3 + 1.10500971967*x4 - 1.01838569726*x5 + 2.62556984696*x6 + 4.85468036438*x7 > 60"; "-0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 > 0"; "0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 > 1"; "0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 > 0"; "-0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 > 1"; "-0.328188665272*x3 + 0.199986646277*x4 + 0.506106406938*x5 - 0.583459965992*x6 + 0.505695871289*x7 < 0"; "-0.345682002598*x3 - 0.101625962101*x4 + 0.57594668021*x5 + 0.729324957489*x6 + 0.0809113394063*x7 < 0"; "0.756087294764*x3 - 0.200079270407*x4 + 0.151379235251*x5 + 0.145864991498*x6 + 0.586607210695*x7 < 0"; "-x1 + 0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 > 0"; "x1 - 0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 > 0"; "-x2 - 0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 > 0"; "x2 + 0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 > 0"; "-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 < 332.888889"; "-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 > 334.888889"]] in
  let tab = ["-1.93414531698*x3 + 1.80314509442*x4 + 2.89695789508*x5 + 0.729324957489*x6 + 3.8837442915*x7 <= 60"; "- 1.13150591228*x3 + 1.10500971967*x4 - 1.01838569726*x5 + 2.62556984696*x6 + 4.85468036438*x7 <= 60"; "-0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 <= 0"; "0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 <= 1"; "0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 <= 0"; "-0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 <= 1"; "-0.328188665272*x3 + 0.199986646277*x4 + 0.506106406938*x5 - 0.583459965992*x6 + 0.505695871289*x7 >= 0"; "-0.345682002598*x3 - 0.101625962101*x4 + 0.57594668021*x5 + 0.729324957489*x6 + 0.0809113394063*x7 >= 0"; "0.756087294764*x3 - 0.200079270407*x4 + 0.151379235251*x5 + 0.145864991498*x6 + 0.586607210695*x7 >= 0"; "-x1 + 0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 <= 0"; "x1 - 0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 <= 0"; "-x2 - 0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 <= 0"; "x2 + 0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 <= 0"; "-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 >= 332.888889"; "-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 <= 334.888889"] in
  (*let tab = ["-(5*x6^2 - 0.875189948987*x6 + 52*x7^2 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 >= 332.888889"; "-(5*x6^2 - 0.875189948987*x6 + 52*x7^2 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 <= 334.888889"; "-1.93414531698*x3 + 1.80314509442*x4 + 2.89695789508*x5 + 0.729324957489*x6 + 3.8837442915*x7 <= 60"; "- 1.13150591228*x3 + 1.10500971967*x4 - 1.01838569726*x5 + 2.62556984696*x6 + 4.85468036438*x7 <= 60"; "-0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 <= 0"; "0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 <= 1"; "0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 <= 0"; "-0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 <= 1"; "-0.328188665272*x3 + 0.199986646277*x4 + 0.506106406938*x5 - 0.583459965992*x6 + 0.505695871289*x7 >= 0"; "-0.345682002598*x3 - 0.101625962101*x4 + 0.57594668021*x5 + 0.729324957489*x6 + 0.0809113394063*x7 >= 0"; "0.756087294764*x3 - 0.200079270407*x4 + 0.151379235251*x5 + 0.145864991498*x6 + 0.586607210695*x7 >= 0"; "-x1 + 0.0524800119769*x3 + 0.904837825133*x4 - 0.209520819817*x5 + 0.291729982996*x6 + 0.222506183367*x7 <= 0"; "x1 - 0.0524800119769*x3 - 0.904837825133*x4 + 0.209520819817*x5 - 0.291729982996*x6 - 0.222506183367*x7 <= 0"; "-x2 - 0.445391966818*x3 - 0.301519984248*x4 - 0.587645368916*x5 + 0.145864991498*x6 + 0.586607210695*x7 <= 0"; "x2 + 0.445391966818*x3 + 0.301519984248*x4 + 0.587645368916*x5 - 0.145864991498*x6 - 0.586607210695*x7 <= 0"; "-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 >= 332.888889"; "-(5*x6*x6 - 0.875189948987*x6 + 52*x7*x7 - 192.710582631*x7) + 54.0615511462*x3 + 45.2691026456*x4 + 33.0896119339*x5 <= 334.888889"] in*)
  let cons = List.map (List.map (Parser.tcons1_of_string env)) list in
  let cons' = Parser.tcons1_of_lstring env tab in
  (env, domains, cons, cons')
;;

let main =
  (*let x = Var.of_string "x" in
  let y = Var.of_string "y" in
  let env = Environment.make [|x|] [|y|] in
  let domains = Parser.lincons1_of_lstring env ["x>=-5"; "x<=5"; "y>=-5"; "y<=5"] in
  (*let tab = ["x^2+y^2=4"; "(x-2)^2+(y+1)^2=1"] in*)
  let tab = ["y-x^2=0"; "y+x^2=2"] in
  let constraints = Parser.tcons1_of_lstring env tab in*)
  let (env, domains, andor, constraints) = gear4 in
  let module SolverBox = Solve(BoxCP) in
  SolverBox.solving env domains constraints 3 0.001;
  let module SolverOct = Solve(OctCP) in
  SolverOct.solving env domains constraints 3 0.001;
  let module SolverPoly = Solve(PolyCP) in
  SolverPoly.solving env domains constraints 3 0.001;

