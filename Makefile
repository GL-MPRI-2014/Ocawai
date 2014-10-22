INTERFACE_SRC = src/common,src/interface,src/engine,src
PACKAGES = ocsfml.graphics

OUTPUT = main.native

interface:
	ocamlbuild -use-ocamlfind -Is $(INTERFACE_SRC) -package $(PACKAGES) $(OUTPUT)

run: interface
	./$(OUTPUT)

clean:
	ocamlbuild -clean
