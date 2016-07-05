Présentation de l'EWS de Madagascar
========================================================
author: Herimanitra RANAIVOSON,
date: 11 juillet 2016
autosize: true

Contexte
========================================================

- 
    A été pour la première déployée à Madagascar en collaboration avec le Ministère de la Santé Malagasy et l'IPM

- L'implémentation du EWS est une étape clée dans l'amélioration des interventions pour la santé publique.
- Cette implémentation s'inscrit dans la valorisation des données sentinelles collectés par l'IPM depuis 2007.




Utilité
========================================================

- Persistence des maladies à potentiel épidémiques notamment la Malaria

- Anticipation du risque d'épidémie de Malaria

- S.I fiable, dynamique et en temps réel pour les décideurs et acteurs de la  santé

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


Avantages de l'application web
========================================================


Petit tour de l'application web
========================================================

L'application web inclut:

- Un tableau de bord pour le choix des maladies à analyser.
- Une carte *interactive* pour visualiser les états des alertes sur les sites (Normale-Pas de données - Alerte )
- Un heatmap *interactive* pour la visualisation des alertes passées.

- Le reste est composé de courbes de tendance et d'occurrence que ca soit individuel ou agrégées.
- Nous venons également d'inclure un tableau de bord résumant les rapports hebdomadaires sur l'ensemble des sites

- Un tableau de bord sur la prédiction et la modélisation.
