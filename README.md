OCAWAI
======

Projet de Génie Logiciel du MPRI, année 2014-2015

[![Is it building ? Click here for more details.](https://travis-ci.org/GL-MPRI-2014/GL_MPRI_2014.svg?branch=master)](https://travis-ci.org/GL-MPRI-2014/GL_MPRI_2014/builds)

# Comment utiliser l'application

*Il s'agit bien sûr d'une version de démo et tout sera potentiellement jeté
en ce qui concerne le fonctionnement*

Depuis le menu principal, il suffit d'appuyer sur **entrée** après avoir
sélectionné **game on!** pour lancer le jeu.

* **ctrl+Q** ou **ctrl+C** : Quitter l'application
* **touches fléchées** : Déplacer la caméra
* **espace** : Actions (déplacements, menus)
* **entrée** : Affiche le menu (*forfeit* permet de revenir au menu
  principal)
* **A** : Dézoomer
* **Z** : Zoomer
* **M** : Zoome au minimum/Rétablit le zoom
* **0** : Réinitialiser le zoom
* **X** : Prochaine unité (ou bâtiment) n'ayant pas joué
* **W** : Idem à l'envers
* **S** : Baisser la vitesse des animations
* **D** : Augmenter la vitesse des animations


# Compilation

Si c'est la première fois que vous générer le projet, lancer la
commande `aclocal -I m4`.


* `autoreconf configure.ac` : Générer le fichier *configure*
* `./configure` : Générer le makefile si les dépendances sont satisfaites
* `make interface` : Compiler l'interface graphique
* `make engine` : Compiler le moteur
* `make network` : Compiler la partie réseaux
* `make doc` : Compiler la documentation
* `make` : Compile l'interface
* `make run` : Compile le jeu et le lance

Si vous faites une modification dans le Makefile.in
(et surtout pas le Makefile). Vous avez juste à utiliser `make Makefile`
pour le mettre à jour.

# Backtrace

Lors qu'une exception est levée, il est possible d'afficher la trace d'éxécution
si on a au préalable lancé `export OCAMLRUNPARAM=b` (ce qui peut être mis
dans son `.bashrc` ou `.bash_profile`).

# Distribuer le projet

* `make dist` : Créer une archive **tar.gz** du projet

# Documentation

La documentation est accessible depuis le lien symbolique `documentation.html`
qui est créé après `make doc`.
