package = no_name
version = 0.1
tarname = $(package)
distdir = $(tarname)-$(version)

ressources_dir=ressources
engine_src=engine
common_src=common
interface_src=interface
network_src=Reseaux

#go in sub directories
all clean engine interface serveur:
	cd src && $(MAKE) $@

dist: $(distdir).tar.gz

#option h is for deference symlinks
$(distdir).tar.gz: $(distdir)
	tar chf - $(distdir) | gzip -9 -c > $@
	rm -rf $(distdir)

$(distdir):
	mkdir -p $(distdir)/src
	#curly braces do not work !
	mkdir $(distdir)/src/$(common_src)
	mkdir $(distdir)/src/$(engine_src)
	mkdir $(distdir)/src/$(interface_src)
	mkdir $(distdir)/src/$(network_src)
	mkdir $(distdir)/$(ressources_dir)
	cp Makefile $(distdir)
	cp README.md $(distdir)
	cp configure $(distdir)
	cp src/$(common_src)/*.ml $(distdir)/src/$(common_src)
	cp src/$(engine_src)/*.ml $(distdir)/src/$(engine_src)
	cp src/$(interface_src)/*.ml $(distdir)/src/$(interface_src)
	cp src/$(network_src)/*.ml $(distdir)/src/$(network_src)
	cp $(ressources_dir)/* $(distdir)/$(ressources_dir)

.PHONY: all clean dist
