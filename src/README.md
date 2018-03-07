# Welcome to the source files of AbSolute !

Sources are split into several repositories:
- **lib** defines all the auxiliary modules that will be used by the solver. They provide general functions that can be re-used and are not specific to AbSolute's implementation.
- **frontend** is the lexer/parser part of the solver
- **domains** defines the abstract domains used by absolute
- **solver** is the actual solver: it defines the abstract domain parametrized solving loop
- **print** regroups all the output utilities of the solver
- **main.ml** is the entry point of the solver