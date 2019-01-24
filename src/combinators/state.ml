type ('a, 'b) state = {
  combinator: 'a;
  abs: 'b;
  constraints: Csp.ctrs;
  constants: Csp.csts;
  view: Csp.jacob;
}

let create combinator abs constraints constants view =
  {combinator=combinator;
   abs=abs;
   constraints=constraints;
   constants=constants;
   view=view}
