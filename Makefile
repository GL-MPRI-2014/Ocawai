INTERFACE_SRC = src/common,src/interface,src/engine,src
PACKAGES = ocsfml.graphics

find_files = $(wildcard $(dir)/*.ml*)
dirs := src/common src/interface src/engine src
files := $(foreach dir,$(dirs),$(find_files))

OUTPUT = main.native

interface:
	ocamlbuild -use-ocamlfind -Is $(INTERFACE_SRC) -package $(PACKAGES) $(OUTPUT)

run: interface
	./$(OUTPUT)

doc :
	ocamlfind ocamldoc -package $(PACKAGES) -d documentation \
	-t "Notre super Documentation" -I _build/src/common -I _build/src/interface \
	-I _build/src/engine -html -colorize-code $(files)
	ln -s documentation/index.html documentation.html

clean:
	ocamlbuild -clean
