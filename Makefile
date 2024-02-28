# Frontend to dune.

.PHONY: default build install uninstall test clean memabsolute

absolute: build
	@cp ./_build/default/absolute-solver/absolute.exe absolute

# The following target is for AbSolute's developpers.
# You should have memtrace, memtrace-viewer and landmarks installed
memabsolute:
	@mv lib/dune lib/dune.tmp
	@echo "(include_subdirs unqualified)\n(library (public_name libabsolute) (name libabsolute)\
 (libraries bigarray gmp apron apron.polkaMPQ apron.octD apron.boxMPQ apronext picasso landmarks)\
 (preprocess (pps landmarks-ppx --auto)) (foreign_stubs (language c) (names ml_float)))" > lib/dune;
	dune build absolute-solver/memabsolute.exe
	@mv lib/dune.tmp lib/dune
	@echo "OCAML_LANDMARKS=on,output=timeperf.json,format=json MEMTRACE=memperf.ctf ./_build/default/absolute-solver/memabsolute.exe \$$@" > memabsolute;
	@echo "[ \$$? -ne 1 ] && (" >> memabsolute;
	@echo "printf \"\\\n\\\n\"" >> memabsolute;
	@echo "echo \"A memory usage trace has been generated in file memperf.ctf\"" >> memabsolute;
	@echo "echo \"A time usage trace has been generated in file timeperf.json\"" >> memabsolute;
	@echo "echo \"Do you wish to launch a viewer on these files?\"" >> memabsolute;
	@echo "read -p \"'t' for time (default) or 'm' for memory [T/m] \" yn" >> memabsolute;
	@echo "     case \$$yn in">> memabsolute;
	@echo "         [m]* ) xdg-open http://\$$HOSTNAME:8080; memtrace-viewer memperf.ctf; break;;">> memabsolute;
	@echo "         * ) xdg-open http://lexifi.github.io/landmarks/viewer.html; exit;;">> memabsolute;
	@echo "     esac" >> memabsolute;
	@echo ")" >> memabsolute;
	@chmod +x memabsolute

default: build doc

doc:
	dune build @doc
	mkdir -p "docs"
	cp -r _build/default/_doc/_html/* docs/

build:
	dune build .

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
	rm -f memperf.ctf timeperf.json memabsolute
# remove all files/folders ignored by git as defined in .gitignore (-X).
	git clean -dfX
