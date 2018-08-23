(******************************************************************)
(*                   entry point of the solver                    *)
(******************************************************************)

(******************************************************************)
(* An instance of the solver is parmetrized by an abstract domain *)
(* which will be used in the abstract solving process and a       *)
(* rendering module witch fits the domain we use                  *)
(******************************************************************)

open Drawer_sig

module GoS (Abs:Adcp_sig.AbstractCP)(Dr:Drawer with type t = Abs.t) = struct
  module Sol = Solver.Solve(Abs)
  module Print = Out.Make(Dr)
  let go prob =
    let res = Sol.solving prob in
    Print.out prob res
end

module GoM (Abs:Adcp_sig.AbstractCP)(Dr:Drawer with type t = Abs.t) = struct
  module Min = Minimizer.Minimize(Abs)
  module Print = Out.Make(Dr)
  let go prob =
    let res = Min.minimizing prob in
    Print.out_min prob res
end

(************************)
(* THE SOLVER INSTANCES *)
(************************)


(***************)
(*   domains   *)
(***************)

module type FullDomain = sig
    module Abstract : Adcp_sig.AbstractCP
    module Drawer : Drawer_sig.Drawer with type t = Abstract.t
end

module MakeFullDomain
    (Abstract : Adcp_sig.AbstractCP)
    (Drawer : Drawer_sig.Drawer with type t = Abstract.t)
    = struct

    module Abstract = Abstract
    module Drawer = Drawer
end

module MakeProduct (D1 : FullDomain) (D2 : FullDomain)
    = struct

    module Abstract = Product.MakeProduct(D1.Abstract)(D2.Abstract)
    module Drawer = Product_drawer.Product_Drawer(Abstract)(D2.Drawer)
end

let get_domain : string -> (module FullDomain)
    = function
    | "box" -> let module M = MakeFullDomain (Abstract_box.BoxF) (Box_drawer.Make(Abstract_box.BoxF))
        in (module M)
    | "boxS" -> let module M = MakeFullDomain (Abstract_box.BoxStrict) (Realbox_drawer)
        in (module M)
    | "boxQ" -> let module M = MakeFullDomain (Abstract_box.BoxQ) (Box_drawer.Make(Abstract_box.BoxQ))
        in (module M)
    | "boxQS" -> let module M = MakeFullDomain (Abstract_box.BoxQStrict) (Box_drawer.Make(Abstract_box.BoxQStrict))
        in (module M)
    | "boxCP" -> let module M = MakeFullDomain (ADCP.BoxCP) (Apron_drawer.BoxDrawer)
        in (module M)
    | "oct" -> let module M = MakeFullDomain (ADCP.OctBoxCP) (Apron_drawer.OctDrawer)
        in (module M)
    | "poly" -> let module M = MakeFullDomain (ADCP.PolyCP) (Apron_drawer.PolyDrawer)
        in (module M)
    | "vpl" -> let module M = MakeFullDomain (Vpl_domain.VplCP) (Vpl_drawer)
        in (module M)
    | s -> Printf.sprintf "Domain %s does not exist" s |> invalid_arg

let set_domain_from_names : string list -> (module FullDomain)
    = fun names ->
    List.fold_left
        (fun (module D : FullDomain) name ->
            let (module F : FullDomain) = get_domain name in
            let module P = MakeProduct (D)(F) in
            (module P)
        )
        (List.hd names |> get_domain)
        (List.tl names)

let set_domain : unit -> (module FullDomain)
    = fun () ->
    print_endline "omg";
    print_endline !Constant.domain;
    Str.split (Str.regexp ",") !Constant.domain
    |> set_domain_from_names

(**
 * Lifts the given abstract domain and its associated drawer into a runnable domain.
 * The results depends on the value of flags {!val:Constant.minimizing} and {!val:Constant.step_by_step}.
 *)
(*
let lift (type s) (module Domain : Adcp_sig.AbstractCP with type t = s) (module Drawer : Drawer_sig.Drawer with type t = s) (prob : Csp.prog) : unit =
    if !Constant.minimizing
    then let module Minimizer = GoM (Domain)(Drawer) in
        Minimizer.go prob
    else
        if !Constant.step_by_step
        then let module SBS = Step_by_step.Make (Domain)(Drawer) in
            SBS.solving prob
        else let module Solver = GoS (Domain)(Drawer) in
             Solver.go prob
*)

(**
 * Lifts the given abstract domain and its associated drawer into a runnable domain.
 * The results depends on the value of flags {!val:Constant.minimizing} and {!val:Constant.step_by_step}.
 *)
let lift (module D : FullDomain) (prob : Csp.prog) : unit =
    if !Constant.minimizing
    then let module Minimizer = GoM (D.Abstract)(D.Drawer) in
        Minimizer.go prob
    else
        if !Constant.step_by_step
        then let module SBS = Step_by_step.Make (D.Abstract)(D.Drawer) in
            SBS.solving prob
        else let module Solver = GoS (D.Abstract)(D.Drawer) in
             Solver.go prob

(********************)
(* OPTIONS HANDLING *)
(********************)

let speclist =
  let open Constant in
  let open Argext in
  [
  ("-visualization", Set visualization    , "Enables visualization mode");
  ("-precision"    , Float set_prec       , default_float "Sets the precision" precision);
  ("-max_sol"      , Int set_max_sol      , default_int "Sets the maximum number of solutions" max_sol);
  ("-max_iter"     , Int set_max_iter     , default_int "Sets the maximum number of iterations" max_iter);
  ("-domain"       , Set_string domain    , options (default_string "Changes the domain used for the solving" domain) "box, boxS, boxQ, boxQS, boxCP, oct, poly, boxNoct, boxNpoly, octNpoly, BandP, vpl");
  ("-obj"          , Set obj              , "Generates an .obj file (for 3D visualization)");
  ("-tex"          , Set tex              , "Prints the solutions in latex format on stadard output");
  ("-pruning"      , Set pruning          , "Enables the \"pruning\" during the solving process");
  ("-trace"        , Set trace            , "Prints the solutions on standard output");
  ("-sure"         , Set sure             , "Keeps only the sure solutions");
  ("-minimize"     , Set minimizing       , "Specify that the problem is a minimization problem");
  ("-iter"         , Set iter             , "Enables the loop for the propagation");
  ("-pruning_iter" , Int set_pruning_iter , "Changes the number of times the pruning process is applied");
  ("-debug"        , Unit set_debug       , "Prints the execution for debug purpose");
  ("-debug_lv"     , Int set_debug_lv     , "Set the debug level. The higher, most print you get");
  ("-split"        , String set_split     , options "Changes the splitting strategy used for the solving" "default, maxSmear, smear");
  ("-no-rewrite"   , Clear rewrite        , default_bool "Disables the constraint rewriting" rewrite);
  ("-sbs"          , Set step_by_step     , "Enabling step by step visualization");
  ("-lin"          , Arg.String Vpl_domain.set_lin      , "Set the linearization algorithm of the VPL");
  ("-vpl_split"    , Arg.String Vpl_domain.set_split    , "Set the split strategy of the VPL");
]

(*************** ALIASES ************)
let aliases =
  [
  ("-m", "-minimize");
  ("-t", "-trace");
  ("-s", "-sure");
  ("-v", "-visualization");
  ("-p", "-precision");
  ("-d", "-domain");
  ("-i", "-iter");
  ("-pi","-pruning_iter");
  ("-sp","-split");
  ]

let globaldescr =
  "AbSolute is a constraint solver based on abstract domains. For more info, check out https://github.com/mpelleau/AbSolute\n"

let parse_args () = Argext.parse_args_aliases speclist aliases Constant.set_prob globaldescr

(***************)
(* entry point *)
(***************)

let go() =
  let open Constant in
  parse_args ();
  Format.printf "domain : %s\n" !domain;
  let prob = File_parser.parse !problem in
  Format.printf "file parsed\n";
  if !debug > 0 then Vpl_domain.enable_debug();
  if !trace then Format.printf "%a" Csp.print prob;
  lift (set_domain ()) prob
let _ = go()
