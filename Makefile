OPAMBIN   := $(shell opam config var bin)
OCAMLC    := $(OPAMBIN)/ocamlc.opt
OCAMLOPT  := $(OPAMBIN)/ocamlopt.opt
OCAMLDEP  := $(OPAMBIN)/ocamldep
OCAMLLEX  := $(OPAMBIN)/ocamllex
OCAMLYACC := $(OPAMBIN)/ocamlyacc
CC        := gcc

# libraries
OPAMDIR   := $(shell opam config var lib)
APRONDIR  := $(OPAMDIR)/apron
ZARITHDIR := $(OPAMDIR)/zarith
GMPDIR    := $(OPAMDIR)/gmp
OCAMLDIR  := $(OPAMDIR)/ocaml

#ocaml libraries
LIBS         := bigarray gmp apron polkaMPQ zarith octD boxMPQ str unix graphics
OCAMLLIBS    := $(LIBS:%=%.cma) $(CCLIB)
OCAMLOPTLIBS := $(LIBS:%=%.cmxa) $(CCLIB)

# directories to include
OCAMLINC  := -I $(ZARITHDIR) -I $(APRONDIR) -I $(GMPDIR) \
             -I src -I src/lib -I src/domains -I src/frontend -I src/print \
             -I src/solver

# targets
TARGETS = solver.opt

AUTOGEN =\
  src/frontend/parser.ml \
  src/frontend/lexer.ml \
  src/frontend/parser.mli \
  src/frontend/modParser.ml \
  src/frontend/modLexer.ml \
  src/frontend/modParser.mli\

# source files
MLFILES = \
  src/lib/mapext.ml \
	src/lib/tools.ml \
	src/lib/polynom.ml \
	src/lib/array_maker.ml \
	src/lib/linconsext.ml \
	src/lib/tconsext.ml \
	src/lib/abstractext.ml \
  src/lib/constant.ml \
  src/lib/apron_utils.ml \
  src/lib/bot.ml \
  src/lib/bound_sig.ml \
  src/lib/bound_mpqf.ml \
  src/lib/bound_float.ml \
  src/lib/itv_sig.ml \
  src/lib/itv.ml \
	src/lib/newitv.ml \
  src/frontend/csp.ml \
  src/frontend/parser.ml \
  src/frontend/lexer.ml \
  src/frontend/modCsp.ml \
  src/frontend/modParser.ml \
  src/frontend/modLexer.ml \
  src/frontend/rewrite.ml \
  src/frontend/preprocessing.ml \
  src/frontend/file_parser.ml \
  src/domains/apron_domain.ml \
  src/domains/abstract_box.ml \
  src/domains/adcp_sig.ml \
  src/domains/ADCP.ml \
  src/domains/variousDA.ml \
  src/solver/result.ml \
  src/solver/splitter.ml \
  src/solver/solver.ml \
  src/solver/minimizer.ml \
  src/print/view.ml \
  src/print/objgen.ml \
	src/print/latex.ml \
  src/print/drawer_sig.ml \
	src/print/box_drawer.ml \
  src/print/realbox_drawer.ml \
	src/print/apron_drawer.ml \
  src/print/out.ml \
	src/main.ml

CFILES = \
  src/lib/ml_float.c

# MLIFILES = ADCP.mli

# object files
CMIFILES = $(MLIFILES:%.ml=%.cmi)
CMOFILES = $(MLFILES:%.ml=%.cmo)
CMXFILES = $(MLFILES:%.ml=%.cmx)
OFILES   = $(CFILES:%.c=%.o)

# rules
all: $(TARGETS)
	@mkdir -p out

solver.opt: $(OFILES) $(CMXFILES)
	$(OCAMLOPT) -o $@ $(OCAMLINC) $(OCAMLOPTLIBS) $+

#minimizer.opt: $(CMXFILES)
#	$(OCAMLOPT) -o $@ $(OCAMLOPTFLAGS) $(OCAMLINC) -cclib "$(CLIBS)" $(OCAMLOPTLIBS) $+

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $*.ml

%.cmi: %.mli %.ml
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $*.mli

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC)  -c $*.ml

%.ml: %.mll
	$(OCAMLLEX) $*.mll

%.ml %.mli: %.mly
	$(OCAMLYACC) $*.mly

%.o: %.c
	$(CC) -I $(OCAMLDIR) -o $@ -c $+

clean:
	rm -f .depend $(TARGETS) $(AUTOGEN)
	rm -f `find . -name "*.o"`
	rm -f `find . -name "*.a"`
	rm -f `find . -name "*.cm*"`
	rm -f `find . -name "*~"`
	rm -f out/*
	rm -f -R out

MLSOURCES = $(MLFILES) $(MLIFILES)

.depend: $(MLSOURCES) Makefile
	-$(OCAMLDEP) -native $(OCAMLINC) $(MLSOURCES) > .depend

.phony:	all clean

include .depend
