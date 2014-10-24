INTERFACE_SRC = src/common,src/interface,src/interface/gui
ENGINE_SRC = src/engine,src/common
PACKAGES = ocsfml.graphics

find_files = $(wildcard $(dir)/*.ml*)
# We will later need to add engine, but while it is not compiled we cannot make
# the corresponding documentation
dirs := src/common src/interface src/interface/gui
files := $(foreach dir,$(dirs),$(find_files))

OUTPUT = main.native

interface:
	ocamlbuild -use-ocamlfind -Is $(INTERFACE_SRC) -package $(PACKAGES) $(OUTPUT)

engine :
	ocamlbuild -use-ocamlfind -Is $(ENGINE_SRC) -package $(PACKAGES) $(OUTPUT)

run: interface
	./$(OUTPUT)

# For now, we cannot handle engine
doc : interface
	mkdir -p documentation
	ocamlfind ocamldoc -stars -package $(PACKAGES) -d documentation \
	-t "Projet Genie Logiciel MPRI 2014" \
	-I _build/src/common -I _build/src/interface \
	-I _build/src/engine -html -colorize-code $(files)
	rm -f documentation.html
	ln -s documentation/index.html documentation.html

clean:
	ocamlbuild -clean
	rm -f documentation/*.html documentation/*.css
	printf "#!/bin/bash\n\n[ -d ./documentation ] && rmdir -- ./documentation" >> cleanDoc.sh
	bash cleanDoc.sh || true
	rm -f -- cleanDoc.sh
	rm -f documentation.html

test:
	echo "To be completed, this is a command that returns 0 for Travis."
