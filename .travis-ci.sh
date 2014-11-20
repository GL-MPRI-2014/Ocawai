#!/bin/bash

# Here are the various libs that will be required. They will be installed
# either with opam or with the system manager.


OPAM_DEPENDS="ocamlfind ocsfml atdgen mm pulseaudio oUnit"
LIB_DEPENDS="libboost-all-dev cmake libsfml-dev pulseaudio libpulse-dev"
COMPILER_DEPENDS="g++ binutils make"

sudo add-apt-repository -y ppa:avsm/ocaml42+opam12
sudo add-apt-repository -y ppa:sonkun/sfml-development

sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam \
						 ${LIB_DEPENDS} ${COMPILER_DEPENDS}

export OPAMYES=1
opam init 
opam switch $OPAM_VERSION
eval `opam config env`
opam install ${OPAM_DEPENDS}
aclocal -I m4
autoreconf configure.ac
./configure
make interface
make engine
make doc
make check
make clean
make dist
make distcheck
make clean
