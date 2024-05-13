(** Libabsolute is a constraint solving library based on abstract domains *)

(** {1 Core constraint language}*)

module Csp = Csp
module Constraint = Constraint
module Expr = Expr
module Dom = Dom
module Instance = Instance

(** This module defines several parsing utilities *)
module Parser = struct
  (** Exception raised by parse functions when the input is syntactically
      invalid.*)
  exception Syntax_error = File_parser.Syntax_error

  (** Exception raised by check functions when the input is semantically invalid *)
  exception Semantic_error = File_parser.Semantic_error

  (** Check that no unbound variables appear, that domains are valid and
      function calls respect the arity.

      @raise [Semantic_error] if one of the above conditions is violated *)
  let semantic_check p = File_parser.check_ast p

  (** Parses a string representation of a numeric expression.

      @raise [Syntax_error] when the input is syntactically invalid *)
  let expr = File_parser.expr

  (** Parses a string representation of a constraint.

      @raise [Syntax_error] when the input is syntactically invalid *)
  let constr = File_parser.constr

  (** parses a [.abs] file and builds the corresponding problem. Raises
      [Syntax_error] when the input is syntactically invalid and
      [Semantic_error] when the input makes no sense (e.g unbound variable).
      When check is set to false, no semantic check is performed *)
  let file ?(check = true) filename =
    let p = File_parser.parse filename in
    if check then semantic_check p ;
    p
end

(** {1 Solver parametrization} *)

module Signature = Signature
module Domains = Domains
module Solver = Solver
module Iterator = Iterator
module Consistency = Consistency
module Result = Result
module Constant = Constant

(** {1 Arithmetic modules} *)

module Ring = Ring
module Q = Q
module F = F
module I = I

(** Module for polynomials over an abstract arithmetic rings *)
module Polynom = Polynom

(** {1 Miscellaneous}*)

module Tools = Tools
module Kleene = Kleene
module Cgraph = Cgraph
