INTERFACE_SRC = src/common,src/interface,src/interface/gui,src/music,src/music/tools
ENGINE_SRC = src/engine,src/common
PACKAGES = bigarray,ocsfml.graphics,atdgen,pulseaudio,threads

# We will later need to add engine, but while it is not compiled we cannot make
# the corresponding documentation
dirs := src/common src/interface src/interface/gui src/music src/music/tools
find_files = $(wildcard $(dir)/*.ml*)
files := $(foreach dir,$(dirs),$(find_files))

OUTPUT = main.native

OUTPUT_ENGINE = main_engine.native

find_files_atd = $(wildcard $(dir)/*.atd)

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
	ocamlbuild -use-ocamlfind -tag thread src/music/tools/audio_c.o -Is $(INTERFACE_SRC) -package $(PACKAGES) $(OUTPUT) -lflags src/music/tools/audio_c.o

engine : $(files_atd_ml)
	ocamlbuild -use-ocamlfind -Is $(ENGINE_SRC) -package $(PACKAGES) $(OUTPUT_ENGINE)

run: interface
	./$(OUTPUT)

eng: engine
	./$(OUTPUT_ENGINE)

# For now, we cannot handle engine
doc : interface
	mkdir -p documentation
	ocamlfind ocamldoc -stars -package $(PACKAGES) -d documentation \
	-t "Projet Genie Logiciel MPRI 2014" \
	-I _build/src/common -I _build/src/interface -I _build/src/interface/gui \
	-I _build/src/engine -html -colorize-code $(files)
	rm -f documentation.html
	ln -s documentation/index.html documentation.html

clean:
	ocamlbuild -clean
	rm -f $(files_atd_ml) $(files_atd_mli)
	rm -f documentation/*.html documentation/*.css
	rmdir documentation 2>/dev/null || true
	rm -f documentation.html

test:
	echo "To be completed, this is a command that returns 0 for Travis."
