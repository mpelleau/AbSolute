SHELL := /bin/bash

include $(shell ocamlc -where)/Makefile.config

all: check_vpl Makefile.config

check_vpl:
	@{ \
		echo "Looking for VPL"; \
		if test -z "$$VPL_PATH"; then VPL_PATH=$$(ocamlfind query vpl); fi; \
		if [ -n "$$VPL_PATH" ]; then \
			echo "VPL found"; \
			cp -f ./src/domains/vpl_domain.ok.ml ./src/domains/vpl_domain.ml; \
			cp -f ./src/print/vpl_drawer.ok.ml ./src/print/vpl_drawer.ml; \
		else \
			echo "VPL not found"; \
			cp -f ./src/domains/vpl_domain.ko.ml ./src/domains/vpl_domain.ml; \
			cp -f ./src/print/vpl_drawer.ko.ml ./src/print/vpl_drawer.ml; \
		fi \
	}

Makefile.config:
	@{ \
		echo "# This file was generated from configure.make"; \
		echo ; \
		echo 'OPAMDIR    := $$(shell opam config var lib)'; \
		echo 'APRONDIR   := $$(OPAMDIR)/apron'; \
		echo 'GMPDIR     := $$(OPAMDIR)/gmp'; \
		echo 'OCAMLDIR   := $$(OPAMDIR)/ocaml'; \
		if test -z "$$VPL_PATH"; then VPL_PATH=$$(ocamlfind query vpl); fi; \
		if [ -n "$$VPL_PATH" ]; then \
			echo 'VPLDIR = $$(OPAMDIR)/vpl'; \
			echo ;\
			echo "LIBS := bigarray gmp apron polkaMPQ octD boxMPQ str unix graphics vpl"; \
			echo ;\
			echo 'OCAMLINC  := -I $$(APRONDIR) -I $$(GMPDIR) -I src -I src/lib -I src/domains -I src/frontend -I src/print -I src/solver -I $$(VPLDIR)'; \
		else \
			echo "LIBS := bigarray gmp apron polkaMPQ octD boxMPQ str unix graphics"; \
			echo ;\
			echo 'OCAMLINC  := -I $$(APRONDIR) -I $$(GMPDIR) -I src -I src/lib -I src/domains -I src/frontend -I src/print -I src/solver'; \
		fi; \
	} > $@
