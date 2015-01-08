#Dépendances nécéssaires au projet

Il vous est conseillé d'utiliser **opam** pour installer les dépendances nécéssaires du projet.

* *ocsfml*, vous aurez probablement besoin d'installer *sfml* avant (une librairie C++)
* *atdgen*
* *dolog*
* *mm*
* *pulseaudio*
* *num*
* *threads*
* *oUnit*


#Installing OCAWAI

1 Type `./configure` to create the configuration and check that you have all the dependencies

2 Type `make` to build OCAWAI.

3 Type `make install` to install all the binairies buit.

4 Type `ocawai` to launch the game

#Générer la documentation

Il suffit d'utiliser la commande `make doc`

#Notice

* Autoconf version >= 2.68 is required 
* Dolog 1.0 is not supported. Use Dolog 0.5
