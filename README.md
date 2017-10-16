# Penvins-Cup-2017
Université Rennes 1 - Master 1 EFCE

## Script de départ

C’est le script préliminaire proposé par Geoffrey, qui peut servir de base et d’ossature à l’ensemble du script, mais chacun est libre de le conserver en partie ou de tout changer pour obtenir la fonction qui lui semble judicieuse.

## Règles

1. Tout le monde peut collaborer à n’importe quelle partie
2. Envoyez vos modifications au responsable de la fonction pour un dialogue afin de décider du/des code/améliorations à retenir

## Responsables

0. Mise en forme et commentaires : Geoffrey.Ruaux
1. Vérifier que l’objet est bien de la classe data.frame
2. Compter les colonnes de l’objet à examiner
3. Vérifier les intitulés de colonne : Camille.Pilisi
4. Vérifier que les colonnes sont bien de la classe attendue
5. Vérifier la modalité du facteur attendu : Guillaume.Weecxsteen
6. Vérifier que le contenu numérique ne sort pas de l’intervalle
7. Ratios larg/haut et peri/larg réalistes : Thomas.Brazier
8. Présenter les résultats en un seul fichier : Thomas.Brazier

## Mode d’emploi de Github

S’inscrire sur Github.
Installer Github desktop et se connecter sur son compte.

Ajouter le repository : https://github.com/ThomasBrazier/PenvinsCup_2017.git

Avant chaque session de travail, « fetch origin » permet de récupérer le projet synchronisé sur le serveur.

Quand vous avez fini vos modifications, il faut les enregistrer en faisant un « commit ».

Quand vous avez fini de travailler sur le projet, il faut faire un « fetch origin » puis un « push » pour mettre à jour le projet sur le serveur.

## Recommandation - Messages utilisateur et erreurs

La fonction sink() est initialisée au début de la fonction checkPenvins(). Par conséquent, tous les sorties des fonctions cat(), warnings() et stop() sont automatiquement écrites dans un fichier texte (tests_summary.txt) qui sera affiché à la fin de la fonction si tout se passe bien.

Pour que stop s’affiche dans la console au lieu du fichier texte, il est donc impératif de mettre la ligne : sink(type = "message") avant la ligne contenant le stop().



## Recommandation - Accents

Comme nous avons tous des systèmes d’exploitations différents, il est préférable de ne pas utiliser les accents dans le script.

## Liste des fonctions

* checkRatio() -> Vérifie le ratio attendu entre deux valeurs
* checkInd() -> vérifie que le fichier est bien de type ind
* checkQuad() -> vérifie que le fichier est bien de type quad
