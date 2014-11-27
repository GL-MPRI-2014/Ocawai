#!/bin/bash

# Here are the various libs that will be required. They will be installed
# either with opam or with the system manager.


OPAM_DEPENDS="ocamlfind ocsfml atdgen mm oUnit dolog"
LIB_DEPENDS="libboost-all-dev cmake libsfml-dev pulseaudio libpulse-dev"
COMPILER_DEPENDS="g++ binutils make"

case "$OCAML_VERSION" in
	3.12.1) ppa=avsm/ocaml312+opam12 ;;
	4.00.1) ppa=avsm/ocaml40+opam12 ;;
	4.01.0) ppa=avsm/ocaml41+opam12 ;;
	4.02.0) ppa=avsm/ocaml42+opam12 ;;
	4.02.1) ppa=avsm/ocaml42+opam12 ;; #There is no repo for 4.02.1,
					   #hence it will be compiled.
	*) echo Unknown OCaml version $OCAML_VERSION; exit 1 ;;
esac


sudo add-apt-repository -y ppa:$ppa
sudo add-apt-repository -y ppa:sonkun/sfml-development

sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam \
						 ${LIB_DEPENDS} ${COMPILER_DEPENDS}

export OPAMYES=1
opam init 

if [ "$OCAML_VERSION" = "4.02.1" ]
then
	opam switch 4.02.1
fi

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
