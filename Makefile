INTERFACE_SRC = src/common,src/interface,src/engine
ENGINE_SRC = src/engine,src/common
PACKAGES = ocsfml.graphics,atdgen

OUTPUT = main.native

find_files_atd = $(wildcard $(dir)/*.atd)

dirs := src/common src/interface
files_atd := $(foreach dir,$(dirs),$(find_files_atd))
files_atd_ml := $(files_atd:.atd=_t.ml) $(files_atd:.atd=_j.ml)
files_atd_mli := $(files_atd_ml:.ml=.mli)

default: interface

# atd files
%_t.ml: %.atd
	atdgen -t $<
%_j.ml: %.atd
	atdgen -j $<

interface: $(files_atd_ml)
	ocamlbuild -use-ocamlfind -Is $(INTERFACE_SRC) -package $(PACKAGES) $(OUTPUT)

engine : $(files_atd_ml)
	ocamlbuild -use-ocamlfind -Is $(ENGINE_SRC) -package $(PACKAGES) $(OUTPUT)

run: interface
	./$(OUTPUT)

eng: engine
	./$(OUTPUT)

clean:
	ocamlbuild -clean
	rm -f $(files_atd_ml) $(files_atd_mli)
