# this configuration file is generated from .configure.make
-include Makefile.config

OPAMBIN   := $(shell opam config var bin)
OCAMLOPTIONS := -w "+a-4-32-27-42" -warn-error "+a-4-32-27-42"
OCAMLC    := $(OPAMBIN)/ocamlc.opt $(OCAMLOPTIONS)
OCAMLOPT  := $(OPAMBIN)/ocamlopt.opt $(OCAMLOPTIONS)
OCAMLDEP  := $(OPAMBIN)/ocamldep
OCAMLLEX  := $(OPAMBIN)/ocamllex
OCAMLYACC := $(OPAMBIN)/ocamlyacc
CC        := gcc

#ocaml libraries
OCAMLLIBS    := $(LIBS:%=%.cma)
OCAMLOPTLIBS := $(LIBS:%=%.cmxa)

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
	src/lib/argext.ml \
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
	src/domains/vpl_domain.ml \
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
	src/print/vpl_drawer.ml \
	src/print/variousDA_drawer.ml \
	src/print/out.ml \
	src/solver/step_by_step.ml \
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
	$(OCAMLOPT) $(PROF) -o $@ $(OCAMLINC) $(OCAMLOPTLIBS) $+

opam_config:
	opam-admin make

setup_vpl:
	cp -f ./src/domains/vpl_domain.ok.ml ./src/domains/vpl_domain.ml
	cp -f ./src/print/vpl_drawer.ok.ml ./src/print/vpl_drawer.ml

setup_no_vpl:
	cp -f ./src/domains/vpl_domain.ko.ml ./src/domains/vpl_domain.ml
	cp -f ./src/print/vpl_drawer.ko.ml ./src/print/vpl_drawer.ml

# proxy rule for rebuilding configuration files directly from the main Makefile
Makefile.config:
	$(MAKE) -f .configure.make all

#minimizer.opt: $(CMXFILES)
#	$(OCAMLOPT) -o $@ (OCAMLINC) -cclib "$(CLIBS)" $(OCAMLOPTLIBS) $+

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(PROF) $(OCAMLINC) -c $*.ml

%.cmi: %.mli %.ml
	$(OCAMLOPT) $(PROF) $(OCAMLINC) -c $*.mli

%.cmx: %.ml
	$(OCAMLOPT) $(PROF) $(OCAMLINC)  -c $*.ml

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
	rm -f Makefile.config

MLSOURCES = $(MLFILES) $(MLIFILES)

.depend: $(MLSOURCES) Makefile
	-$(OCAMLDEP) -native $(OCAMLINC) $(MLSOURCES) > .depend

.phony:	all clean opam_config setup_vpl setup_no_vpl

-include .depend
