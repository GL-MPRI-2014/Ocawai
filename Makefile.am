

#SUBDIRS = src doc test

bin_PROGRAMS = server interface engine


PACKAGES = -package ocsfml,graphics,atdgen
LIBS = -libs unix
#CLEANFILES = *.html *.css *.cmo *.cmi *~ $(bin_PROGRAMS)

INTERFACE_SRC = src/common,src/interface,src/interface/gui
ENGINE_SRC = src/engine,src/common
SERVER_SRC = src/Reseaux

# We will later need to add engine, but while it is not compiled we cannot make
# the corresponding documentation
dirs := src/common src/interface src/interface/gui
find_files = $(wildcard $(dir)/*.ml*)
files := $(foreach dir,$(dirs),$(find_files))

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

.PHONY: interface engine run server eng doc distclean test

interface: $(files_atd_ml)
	ocamlbuild -use-ocamlfind -Is $(INTERFACE_SRC) $(PACKAGES) main.native

engine : $(files_atd_ml)
	ocamlbuild -use-ocamlfind -Is $(ENGINE_SRC) $(PACKAGES) main.native

server:	
	ocamlbuild -use-ocamlfind $(LIBS) -Is $(SERVER_SRC) $@.native


# For now, we cannot handle engine
doc : interface
	mkdir -p doc
	ocamlfind ocamldoc -stars -package $(PACKAGES) -d doc \
	-t "Projet Genie Logiciel MPRI 2014" \
	-I _build/src/common -I _build/src/interface -I _build/src/interface/gui \
	-I _build/src/engine -html -colorize-code $(files)
	rm -f documentation.html
	ln -s doc/index.html documentation.html

clean:
	ocamlbuild -clean
	rm -f $(files_atd_ml) $(files_atd_mli)
	rm -f documentation/*.html documentation/*.css
	rm -f doc/*.html doc/*.css
	printf "#!/bin/bash\n\n[ -d ./documentation ] && rmdir -- ./documentation" >> cleanDoc.sh
	bash cleanDoc.sh || true
	rm -f -- cleanDoc.sh
	rm -f documentation.html


distclean:
	rm -f config.status config.log
	rm -rf autom4te.cache

test:
	echo "To be completed, this is a command that returns 0 for Travis."
