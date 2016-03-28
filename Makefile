# tools
OCAMLC    = ocamlc.opt
OCAMLOPT  = ocamlopt.opt
OCAMLDEP  = ocamldep
OCAMLLEX  = ocamllex
OCAMLYACC = ocamlyacc

# libraries
OPAMDIR = $(HOME)/.opam/4.02.3/lib
APRONDIR = $(OPAMDIR)/apron
ZARITHDIR = $(OPAMDIR)/zarith
GMPDIR = $(OPAMDIR)/gmp
CAMLIDLDIR = $(OPAMDIR)/camlidl
OCAMLINC = -I $(ZARITHDIR) -I $(APRONDIR) -I $(GMPDIR) -I $(CAMLIDLDIR) -I src -I src/domains -I src/frontend
LIBS = bigarray gmp apron polkaMPQ zarith octD boxMPQ str unix graphics
CCLIB = -cclib "-L $(ZARITHDIR) -L $(APRONDIR) -L $(GMPDIR) -L $(CAMLIDLDIR)"
OCAMLLIBS = $(LIBS:%=%.cma) $(CCLIB)
OCAMLOPTLIBS = $(LIBS:%=%.cmxa) $(CCLIB) 
CLIBS = -lgmp -lxcb

# targets
TARGETS = minimizer.opt solver.opt

AUTOGEN =\
  src/frontend/parser.ml \
  src/frontend/lexer.ml \
  src/frontend/parser.mli

# source files
MLFILES = \
  src/frontend/syntax.ml \
  src/frontend/parser.ml \
  src/frontend/lexer.ml \
  src/frontend/file_parser.ml \
  src/constant.ml \
  src/utils.ml \
  src/domains/apron_domain.ml \
  src/domains/bot.ml \
  src/domains/mapext.ml \
  src/domains/bound_sig.ml \
  src/domains/bound_mpqf.ml \
  src/domains/bound_float.ml \
  src/domains/itv_sig.ml \
  src/domains/itv.ml \
  src/domains/abstract_box.ml \
  src/ADCP.ml \
  src/variousDA.ml \
  src/problems.ml \
  src/vue.ml \
  src/solver.ml \
  src/main.ml

# MLIFILES = ADCP.mli

# object files
CMIFILES = $(MLIFILES:%.ml=%.cmi)
CMOFILES = $(MLFILES:%.ml=%.cmo)
CMXFILES = $(MLFILES:%.ml=%.cmx)

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

%.cmi: %.mli %.ml
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $*.mli

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC)  -c $*.ml

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC)  -c $*.ml

%.ml: %.mll
	$(OCAMLLEX) $*.mll

%.ml %.mli: %.mly
	$(OCAMLYACC) $*.mly

clean:
	rm -f depend $(TARGETS) $(AUTOGEN)
	rm -f `find . -name "*.o"`
	rm -f `find . -name "*.a"`
	rm -f `find . -name "*.cm*"`
	rm -f `find . -name "*~"`

MLSOURCES = $(MLFILES) $(MLIFILES)

depend: $(MLSOURCES) Makefile
	-$(OCAMLDEP) -native $(OCAMLINC) $(MLSOURCES) > depend

.phony:	all clean

include depend
