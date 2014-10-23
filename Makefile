INTERFACE_SRC = src/common,src/interface
ENGINE_SRC = src/engine,src/common
PACKAGES = ocsfml.graphics

find_files = $(wildcard $(dir)/*.ml*)
dirs := src/common src/interface src/engine src
files := $(foreach dir,$(dirs),$(find_files))

OUTPUT = main.native

interface:
	ocamlbuild -use-ocamlfind -Is $(INTERFACE_SRC) -package $(PACKAGES) $(OUTPUT)

engine :
	ocamlbuild -use-ocamlfind -Is $(ENGINE_SRC) -package $(PACKAGES) $(OUTPUT)

run: interface
	./$(OUTPUT)

doc :
	ocamlfind ocamldoc -package $(PACKAGES) -d documentation \
	-t "Notre super Documentation" -I _build/src/common -I _build/src/interface \
	-I _build/src/engine -html -colorize-code $(files)
	rm documentation.html
	ln -s documentation/index.html documentation.html

clean:
	ocamlbuild -clean

test:
	echo "To be completed, this is a command that returns 0 for Travis."
