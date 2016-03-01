# tools
OCAMLC =    ocamlc.opt
OCAMLOPT =  ocamlopt.opt
OCAMLDEP =  ocamldep

# libraries
OPAMDIR = $(HOME)/.opam/4.02.3/lib
APRONDIR = $(OPAMDIR)/apron
GMPDIR = $(OPAMDIR)/gmp
OCAMLINC = -I $(APRONDIR) -I $(GMPDIR)
OCAMLLIBS = bigarray.cma gmp.cma apron.cma polkaMPQ.cma octD.cma boxMPQ.cma str.cma unix.cma -cclib "-L$(APRONDIR) -L$(GMPDIR)"
OCAMLOPTLIBS = bigarray.cmxa gmp.cmxa apron.cmxa polkaMPQ.cmxa octD.cmxa boxMPQ.cmxa str.cmxa unix.cmxa -cclib "-L$(APRONDIR) -L$(GMPDIR)"
CLIBS = -lgmp

# targets
TARGETS = minimizer.opt solver.opt

# source files
MLFILES = constant.ml utils.ml ADCP.ml problems.ml minimizer.ml solver.ml main.ml
# MLIFILES = ADCP.mli

# object files
CMOFILES = $(MLFILES:%.ml=%.cmo)
CMXFILES = $(MLFILES:%.ml=%.cmx)
CMIFILES = $(MLIFILES:%.ml=%.cmi)

# rules
all: $(TARGETS)

solver.opt: $(CMXFILES)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) $(OCAMLINC) -cclib "$(CLIBS)" $(OCAMLOPTLIBS) $+

solver: $(CMOFILES)
	$(OCAMLC) -custom -o $@ $(OCAMLFLAGS) $(OCAMLINC) -cclib "$(CLIBS)" $(OCAMLLIBS) $+

minimizer.opt: $(CMXFILES)
	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) $(OCAMLINC) -cclib "$(CLIBS)" $(OCAMLOPTLIBS) $+

minimizer: $(CMOFILES)
	$(OCAMLC) -custom -o $@ $(OCAMLFLAGS) $(OCAMLINC) -cclib "$(CLIBS)" $(OCAMLLIBS) $+

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $*.ml

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $*.ml

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $*.mli

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC)  -c $*.ml

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC)  -c $*.ml

clean:
	rm -f depend $(TARGETS) *.o *.a *.cm* *~ \#*

MLSOURCES = $(MLFILES) $(MLIFILES)

depend: $(MLSOURCES) Makefile
	-$(OCAMLDEP) -native $(OCAMLINC) $(MLSOURCES) > depend

.phony:	all clean

include depend
