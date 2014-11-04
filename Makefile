# Makefile.  Generated from Makefile.in by configure.


#variable from configure
package = projet_GL_2014_2015
version = V0.1
tarname = projet_gl_2014_2015
distdir = $(tarname)-$(version)

#prefix = /usr/local
#exec_prefix = $(prefix)
#bindir = $(exec_prefix)/bin

prefix = /usr/local
exec_prefix = ${prefix}
bindir = ${exec_prefix}/bin

#VPATH variable
#VPATH allow to build the project in other tree than were
#are located the original sources files
#not supported by ocamlbuild because ocamlbuild
#do not support relative path, WTF ?!?
#srcdir = .
#VPATH = .

src_root=src
resources_dir=resources
documentation_dir=doc
engine_dir=$(src_root)/engine
common_dir=$(src_root)/common
interface_dir=$(src_root)/interface
gui_dir=$(interface_dir)/gui
network_dir=$(src_root)/network

dirs = $(engine_dir) $(common_dir) $(interface_dir) $(network_dir) $(gui_dir)

engine_src=$(engine_dir),$(common_dir)
interface_src=$(engine_dir),$(common_dir),$(interface_dir),$(gui_dir)
network_src=$(network_dir)

common_dependencies = atdgen
engine_dependencies = $(common_dependencies)
interface_dependencies = ocsfml.graphics,$(common_dependencies)
network_libraries = unix

output_interface = main.native
output_engine = main_engine.native
output_network = server.native


#get every source file of the project
find_files = $(wildcard $(dir)/*.ml*)
files := $(foreach dir,$(dirs),$(find_files))



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

#temporary solution : find a way toreplace src/common
find_files_atd = $(wildcard src/common/*.atd)

files_atd := $(foreach dir,$(dirs),$(find_files_atd))
files_atd_ml := $(files_atd:.atd=_t.ml) $(files_atd:.atd=_j.ml)
files_atd_mli := $(files_atd_ml:.ml=.mli)

all: interface engine network

interface: $(files_atd_ml) 
	ocamlbuild -use-ocamlfind -Is $(interface_src) -package $(interface_dependencies) $(output_interface)

engine:  $(file_atd_ml)
	ocamlbuild -use-ocamlfind -Is $(engine_src) -package $(engine_dependencies) $(output_engine)

network:
	ocamlbuild -use-ocamlfind -libs $(network_libraries) -Is $(network_src) $(output_network)


#variable destdir is useful for distcheck and more generally if you want to change the directory for installation.
install:
	install -d "$(DESTDIR)$(bindir)"
	install -m 0755 $(output_interface) "$(DESTDIR)$(bindir)"
	install -m 0755 $(output_engine) "$(DESTDIR)$(bindir)"
	install -m 0755 $(output_network) "$(DESTDIR)$(bindir)"

uninstall:
	-rm $(bindir)/$(output_interface)
	-rm $(bindir)/$(output_engine)
	-rm $(bindir)/$(output_network)



dist: $(distdir).tar.gz


#option h is for deference symlinks
$(distdir).tar.gz: $(distdir)
	tar chf - $(distdir) | gzip -9 -c > $@
	rm -rf $(distdir)

#curly braces do not work !			
$(distdir): FORCE
	mkdir -p $(distdir)/src
	mkdir $(distdir)/$(common_dir)
	mkdir $(distdir)/$(engine_dir)
	mkdir $(distdir)/$(interface_dir)
	mkdir $(distdir)/$(gui_dir)
	mkdir $(distdir)/$(network_dir)
	mkdir $(distdir)/$(resources_dir)
	cp Makefile.in $(distdir)
	cp README.md $(distdir)
	cp configure.ac $(distdir)
	cp configure $(distdir)
	cp install-sh $(distdir)
	cp libtool $(distdir)
	cp missing $(distdir)
	cp depcomp $(distdir)
	cp $(common_dir)/*.ml* $(distdir)/$(common_dir)
	cp $(common_dir)/*.atd $(distdir)/$(common_dir)
	cp $(engine_dir)/*.ml* $(distdir)/$(engine_dir)
	cp $(interface_dir)/*.ml* $(distdir)/$(interface_dir)
	cp $(gui_dir)/*.ml* $(distdir)/$(gui_dir)
	-cp $(network_dir)/*.ml* $(distdir)/$(network_dir)
	cp -R $(resources_dir)/* $(distdir)/$(resources_dir)

distcheck: $(distdir).tar.gz
	gzip -cd $(distdir).tar.gz | tar xvf -
	cd $(distdir) && ./configure
	cd $(distdir) && $(MAKE) network
	cd $(distdir) && $(MAKE) engine
	cd $(distdir) && $(MAKE) interface
	cd $(distdir) && $(MAKE) check
	cd $(distdir) && $(MAKE) install DESTDIR="$${PWD}/_inst"
	cd $(distdir) && $(MAKE) uninstall DESTDIR="$${PWD}/_inst"
	#check if uninstall works
	@remaining="`find $${PWD}/$(distdir)/_inst -type f | wc -l`"; \
	if test "$${remainning}" -ne 0; then \
		echo "*** $${remaining} file(s) remaining in temporary install directory!"; \
		exit 1; \
	fi
	cd $(distdir) && $(MAKE) clean
	rm -rf  $(distdir)
	@echo "*** Package $(distdir).tar.gz is ready for distribution"

FORCE:
	-rm $(distdir).tar.gz > /dev/null 2>&1
	-rm -rf $(distdir) > /dev/null 2>&1

#recompile the Makefile if you change something in Makefile.in
#without start over configure
Makefile: Makefile.in config.status
	./config.status $@

config.status: configure
	./config.status --recheck

doc: interface engine
	mkdir -p $(documentation_dir)
	ocamlfind ocamldoc -stars -package $(interface_dependencies) -d doc \
	-t "Projet_Genie_Logiciel_MPRI_2014" \
	-I _build/src/common -I _build/src/interface -I _build/src/interface/gui \
	-I _build/src/engine -html -colorize-code $(files)
	rm -f documentation.html
	ln -s doc/index.html documentation.html

clean:
	ocamlbuild -clean
	rm -f $(files_atd_ml) $(files_atd_mli)
	-rm -f $(documentation_dir)/*.html $(documentation_dir)/*.css
	-rmdir doc 
	rm -f documentation.html

check:
	@echo "To be completed, this is a command that returns 0 for Travis."

.PHONY: FORCE all clean dist distcheck engine interface network
