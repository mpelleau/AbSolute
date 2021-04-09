# Frontend to dune.

.PHONY: default build install uninstall test clean

absolute: build
	@cp ./_build/default/absolute-solver/absolute.exe absolute

memabsolute: absolute
	@echo "MEMTRACE=log.ctf ./_build/default/absolute-solver/memabsolute.exe \$$@" > memabsolute;
	@echo "[ \$$? -ne 1 ] && (xdg-open http://\$$HOSTNAME:8080; memtrace-viewer log.ctf)" >> memabsolute;
	@chmod +x memabsolute

default: build doc

doc:
	dune build @doc
	mkdir -p "docs"
	cp -r _build/default/_doc/_html/* docs/

build:
	dune build

test:
	dune runtest -f

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
	rm -f log.ctf
	rm -f memabsolute
# remove all files/folders ignored by git as defined in .gitignore (-X).
	git clean -dfX
