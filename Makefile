INTERFACE_SRC = src/common,src/interface
ENGINE_SRC = src/engine,src/common
PACKAGES = ocsfml.graphics

OUTPUT = main.native

interface:
	ocamlbuild -use-ocamlfind -Is $(INTERFACE_SRC) -package $(PACKAGES) $(OUTPUT)

engine :
	ocamlbuild -use-ocamlfind -Is $(ENGINE_SRC) -package $(PACKAGES) $(OUTPUT)

run: interface
	./$(OUTPUT)

clean:
	ocamlbuild -clean

test:
	echo "To be completed, this is a command that returns 0 for Travis."
