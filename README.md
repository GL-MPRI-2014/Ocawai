GL_MPRI_2014
============

Projet de Génie Logiciel du MPRI, année 2014-2015

[![Is it building ? Click here for more details.](https://travis-ci.org/GL-MPRI-2014/GL_MPRI_2014.svg?branch=master)](https://travis-ci.org/GL-MPRI-2014/GL_MPRI_2014/builds)

# Comment utiliser l'application

*Il s'agit bien sûr d'une version de démo et tout sera potentiellement jeté
en ce qui concerne le fonctionnement*

Depuis le menu principal, il suffit d'appuyer sur **entrée** après avoir
sélectionné **game on!** pour lancer le jeu.

* **F** : Mettre l'application en plein-écran
* **esc** : Repasser en mode fenêtre
* **ctrl+Q** ou **ctrl+C** : Quitter l'application
* **touches fléchées** : Déplacer la caméra
* **espace** : Entrer/sortir du mode déplacement d'unité
* **entrée** : Affiche le menu (*forfeit* permet de revenir au menu
  principal)
* **A** : Dézoomer
* **Z** : Zoomer
* **M** : Zoome au minimum/Rétablit le zoom
* **0** : Réinitialiser le zoom


# Compilation

* `make interface` : Compiler l'interface graphique
* `make run` : Compiler l'interface et la lancer
* `make doc` : Compile la documentation

# Documentation

La documentation est accessible depuis le lien symbolique `documentation.html`
qui est créé après `make doc`.
