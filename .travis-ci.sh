# Edit this for your own project dependencies
OPAM_DEPENDS="ocamlfind ocsfml"
LIB_DEPENDS="libboost-all-dev cmake libsfml-dev"

echo "yes" | sudo add-apt-repository ppa:avsm/ocaml42+opam12
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam ${LIB_DEPENDS}

export OPAMYES=1
opam init 
eval `opam config env`
opam switch 4.02.0
opam install ${OPAM_DEPENDS}
make
make test
