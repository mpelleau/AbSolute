# Welcome to the source files of AbSolute !

Sources are split into several repositories:
- `lib` defines all the auxiliary modules that will be used by the solver. They provide general functions that can be re-used and are not specific to AbSolute's implementation.
- `frontend` is the lexer/parser part of the solver
- `domains` defines the abstract domains used by AbSolute
- `solver` is the actual solver: it defines the abstract domain parametrized solving loop
- `print` regroups all the output utilities of the solver
- `main.ml` is the entry point of the solver
- `check.ml` is a sanity checker of the implementation

## Lexicon

A number of abbreviations are used in AbSolute, we give a non-exhaustive list here:

- CSP: constraint satisfaction problem.
- ADCP: abstract domain (for) constraint programming.
- MPQF: GMP multi-precision rationals, functional version (from the external library GMP); 'Q' probably stands for "quotient".
- ITV: interval.
- VPL: (Verimag) Verified Polyhedra Library (external library).
- csts: constants
- ctrs: constraints
