INTERFACE_SRC = src/common,src/interface,src/engine,src
PACKAGES = ocsfml.graphics

interface:
	ocamlbuild -use-ocamlfind -Is $(INTERFACE_SRC) -package $(PACKAGES) main.native

clean:
	ocamlbuild -clean
