Présentation de l'EWS de Madagascar
========================================================
author: Herimanitra RANAIVOSON,
date: 11 juillet 2016
autosize: true

Contexte
========================================================

- 
    A été pour la première déployée à Madagascar en collaboration avec le Ministère de la Santé Malagasy et l'IPM

- Persistence des maladies à potentiel épidémiques notamment la Malaria

- S'inscrit aussi dans la valorisation des données de surveillance sentinelles collectés par l'IPM depuis 2007.




Utilité
========================================================

- Anticipation du risque d'épidémie de Malaria

- Des algorithmes de détection d'épidémies sont utilisés (recommandés par WHO, CDC)

- S.I fiable, dynamique et en temps réel pour les décideurs et acteurs oeuvrant dans la santé

- L'implémentation du EWS est une étape clée dans l'amélioration des interventions au service de la santé publique.

- En gros, renforcer le système de surveillance sanitaire du pays.

Apercu de l'EWS de Madagascar
========================================================

![](overview-ews.png)


Historiques
========================================================

- Etendue collecte de données (2007: 13sites ; 2011: 34 sites ; 2016: 54 sites )

- Avant 2007, les données étaient collectées par SMS et saisies manuellement dans une BD traditionnelle (Microsoft Access).

- Depuis 2007 , les données sont envoyées directement depuis un serveur postgreSQL en utilisant un formulaire android sur smartphone.

==> Risque d'erreur de saisie minimisé!




Petit tour de l'application web
========================================================

L'application web inclut:

- Un tableau de bord pour les paramètres(ex:le choix des maladies à analyser)

- Une carte *interactive* pour visualiser les états des alertes sur les sites (Normale-Pas de données - Alerte )
- Un heatmap *interactive* pour la visualisation des alertes passées.

- Le reste est composé de courbes de tendance et d'occurrence que ca soit individuel ou agrégées.
- Nous venons également d'inclure un tableau de bord résumant les rapports hebdomadaires sur l'ensemble des sites (envoyés automatiquement)


Perspectives pour 2016-2017
========================================================
Dans le but de répondre rapidement et effectivement aux maladies à potentiel épidémique, nous devons améliorer la capacité du système notammment sur :

    * L'insertion,validation des modèles de prédiction pour toutes les maladies et l'ensemble des sites. (comment agréger les sites pour la prédiction?)
    
    * expansion du réseau de surveillance sentinelle

    * L'amélioration de la performance de l'application shiny.
    
     * En faire un package R

    * L'expérimentation d'un autre système open source qui permet d'avoir la fonctionnalité multiuser (jupyter/jupyter-dashboard)

    



Merci pour votre attention!
========================================================

* [rherimanitra@pasteur.mg; air_manitra@yahoo.fr; airmanitra@gmail.com](https://google.com)
* [@air_manitra sur twitter](https://twitter.com/@air_manitra)
* [RanaivosonHerimanitra sur github](github.com/RanaivosonHerimanitra)
