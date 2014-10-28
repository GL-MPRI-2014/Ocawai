GL_MPRI_2014
============

Projet de Génie Logiciel du MPRI, année 2014-2015

[![Is it building ? Click here for more details.](https://travis-ci.org/GL-MPRI-2014/GL_MPRI_2014.svg?branch=master)](https://travis-ci.org/GL-MPRI-2014/GL_MPRI_2014/builds)

# Comment utiliser l'application

*Il s'agit bien sûr d'une version de démo et tout sera potentiellement jeté
en ce qui concerne le fonctionnement*

Depuis le menu principal, il suffit d'appuyer sur **espace** pour lancer le
jeu.

* **F** : Mettre l'application en plein-écran
* **esc** : Repasser en mode fenêtre
* **ctrl+Q** ou **ctrl+C** : Quitter l'application
* **flèches** : déplacer la caméra
* **espace** : entrer en/sortir du mode déplacement d'unité;
  permet aussi d'afficher le menu dans le cas où aucune unité n'est
  sélectionnable (et une fois dans le menu, permet d'effectuer les actions
  affichées -- *forfeit* permet de revenir au menu principal)
* **A** : Dézoomer
* **Z** : Zoomer


# Compilation

* `make interface` : compiler l'interface graphique
* `make run` : compiler l'interface et la lancer
* `make doc` : compile la documentation

# Documentation

La documentation est accessible depuis le lien symbolique `documentation.html`
qui est créé après `make doc`.
