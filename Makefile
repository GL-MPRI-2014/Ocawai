package = no_name
version = 0.1
tarname = $(package)
distdir = $(tarname)-$(version)

root_src=src
ressources_dir=$(root_src)/ressources
engine_src=$(root_src)/engine
common_src=$(root_src)/common
interface_src=$(root_src)/interface
network_src=$(root_src)/Reseaux

sources = $(engine_src) $(common_src) $(interface_src) $(network_src)
engine_dependencies = atdgen
interface_dependencies = ocsfml.graphics
network_libraries = unix

output_interface = main.native
output_engine = main_engine.native
output_network = Server.native

###########################################
#    Generate .ml files from .atd files   #
###########################################

# atd files
%_t.ml: %.atd
	atdgen -t $<
%_j.ml: %.atd
	atdgen -j $<


###########################################
#  get all the atd files in the project   #
###########################################

find_files_atd = $(wildcard $(sources)/*.atd)

files_atd := $(foreach dir,$(sources),$(find_files_atd))
files_atd_ml := $(files_atd:.atd=_t.ml) $(files_atd:.atd=_j.ml)

#useless ?
files_atd_mli := $(files_atd_ml:.ml=.mli)


interface: $(files_atd_ml) 
	ocamlbuild -use-ocamlfind -Is $(interface_src) -package $(interface_dependencies) $(output_interface)

engine :  $(file_atd_ml)
	ocamlbuild -use-ocamlfind -Is $(engine_src),$(common_src) -package $(engine_dependencies) $(output_engine)

network :
	ocamlbuild -use-ocamlfind -libs $(network_libraries) -Is $(network_src) $(output_network)


dist: $(distdir).tar.gz

#option h is for deference symlinks
$(distdir).tar.gz: $(distdir)
	tar chf - $(distdir) | gzip -9 -c > $@
	rm -rf $(distdir)

#curly braces do not work !			
$(distdir): FORCE
	mkdir -p $(distdir)/src
	mkdir $(distdir)/$(common_src)
	mkdir $(distdir)/$(engine_src)
	mkdir $(distdir)/$(interface_src)
	mkdir $(distdir)/$(network_src)
	mkdir $(distdir)/$(ressources_dir)
	cp Makefile $(distdir)
	cp README.md $(distdir)
	cp configure $(distdir)
	cp $(common_src)/*.ml $(distdir)/$(common_src)
	cp $(common_src)/*.atd $(distdir)/$(common_src)
	cp $(engine_src)/*.ml $(distdir)/$(engine_src)
	cp $(interface_src)/*.ml $(distdir)/$(interface_src)
	-cp $(network_src)/*.ml $(distdir)/$(network_src)
	cp $(ressources_dir)/* $(distdir)/$(ressources_dir)

FORCE:
	-rm $(distdir).tar.gz > /dev/null 2>&1
	-rm -rf $(distdir) > /dev/null 2>&1

clean:
	ocamlbuild -clean

check:
	@echo "To be completed, this is a command that returns 0 for Travis."

.PHONY: FORCE all clean dist engine interface network
