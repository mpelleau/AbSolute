# Frontend to dune.

.PHONY: default build install uninstall test clean

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
# remove all files/folders ignored by git as defined in .gitignore (-X).
	git clean -dfX
