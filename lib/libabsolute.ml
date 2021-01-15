module Csp = Csp
module Constraint = Constraint
module Print = Print

module Parser = struct
  exception Syntax_error = File_parser.Syntax_error

  exception Semantic_error = File_parser.Semantic_error

  (** Parses a string representation of a constraint. Raises [Syntax_error] when
      the input is syntactically invalid *)
  let constr = File_parser.constr

  (** Check that no unbound variables appear, that domains are valid and
      function calls respect the arity. Raises [Semantic_error] if one of the
      above conditions is violated *)
  let semantic_check p = File_parser.check_ast p

  (** parses a [.abs] file and builds the corresponding problem. Raises
      [Syntax_error] when the input is syntactically invalid and
      [Semantic_error] when the input maakes no sense (e.g unbound variable).
      When check is set to false, no semantic check is performed *)
  let file ?(check = true) filename =
    let p = File_parser.parse filename in
    if check then semantic_check p ;
    p
end

module Signature = Signature
module Iterator = Iterator
module Solver = Solver
module Result = Result
module Constant = Constant
module Domains = Domains
module Kleene = Kleene
module Consistency = Consistency
