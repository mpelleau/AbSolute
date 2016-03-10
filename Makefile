# tools
OCAMLC =    ocamlc.opt
OCAMLOPT =  ocamlopt.opt
OCAMLDEP =  ocamldep

# libraries
OPAMDIR = $(HOME)/.opam/4.02.3/lib
APRONDIR = $(OPAMDIR)/apron
ZARITHDIR = $(OPAMDIR)/zarith
GMPDIR = $(OPAMDIR)/gmp
CAMLIDLDIR = $(OPAMDIR)/camlidl
OCAMLINC = -I $(ZARITHDIR) -I $(APRONDIR) -I $(GMPDIR) -I $(CAMLIDLDIR)
OCAMLLIBS = bigarray.cma gmp.cma apron.cma polkaMPQ.cma zarith.cma octD.cma boxMPQ.cma str.cma unix.cma graphics.cma -cclib "-L $(ZARITHDIR) -L $(APRONDIR) -L $(GMPDIR) -L $(CAMLIDLDIR)"
OCAMLOPTLIBS = bigarray.cmxa gmp.cmxa apron.cmxa polkaMPQ.cmxa zarith.cmxa octD.cmxa boxMPQ.cmxa str.cmxa unix.cmxa graphics.cmxa -cclib "-L $(ZARITHDIR) -L $(APRONDIR) -L $(GMPDIR) -L $(CAMLIDLDIR)"
CLIBS = -lgmp -lxcb

# targets
TARGETS = minimizer.opt solver.opt

# source files
MLFILES = constant.ml utils.ml ADCP.ml problems.ml vue.ml minimizer.ml solver.ml main.ml
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
