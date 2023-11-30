.PHONY: test check

PROGRAM_MODULES=lib/game lib/deck lib/board lib/player
TEST_MODULES=main
MODULES=$(PROGRAM_MODULES) $(TEST_MODULES)
OBJECTS=$(MODULES:=.cmo)
MLS=$(PROGRAM_MODULES:=.ml)
MLIS=$(PROGRAM_MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix,oUnit,str,yojson

default: build
	utop

build:
	dune build

doc:
	dune build @doc

game:
	OCAMLRUNPARAM=b dune exec bin/main.exe
test:
	OCAMLRUNPARAM=b dune exec test/main.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/main.exe
	bisect-ppx-report html
zip:
	rm -f sequence.zip
	zip -r sequence.zip . -x@exclude.lst

clean:
	dune clean
	rm -f sequence.zip

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)
	bisect*.out