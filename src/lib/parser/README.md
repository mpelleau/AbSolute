# frontend
This repository defines all the modules that will be used to build the problem from a textual representation

- **csp** defines the problems and the constraint language used by absolute
- **lexer.mll** and **parser.mly** are used to generate to syntaxic analysis
- **rewrite** defines the rewriting rules that will be applied to the problem when the option *-rewrite* is enabled
- **file_parser** is the entry point of the csp buiding: it parses a file and return the corresponding csp