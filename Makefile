INTERFACE_SRC = src/common,src/interface,src
PACKAGES = ocsfml.graphics

OUTPUT = main.native

run: interface
	./$(OUTPUT)

interface:
	ocamlbuild -use-ocamlfind -Is $(INTERFACE_SRC) -package $(PACKAGES) $(OUTPUT)

clean:
	ocamlbuild -clean
