<!--
Aide contextuelle des cards — version française.

Traduction de help_en.md : ne jamais toucher aux lignes "# clé", elles doivent
rester identiques d'une langue à l'autre. Une clé absente ici retombe
automatiquement sur l'anglais.
-->

# wave-card_presence
## Variables absentes d'une vague
Ce tableau n'apparaît que si une variable est **présente dans une vague et
absente dans une autre**, parmi les vagues comparées.

- Chaque colonne est une vague, chaque case liste les variables de cette vague.
- ✔ la variable contient des valeurs, ✖ la variable est vide ou absente.

Une variable qui disparaît est souvent normal (changement de questionnaire,
nouveau routage). L'enjeu est de vérifier que chaque disparition est voulue,
avant de lire les tableaux suivants : une variable absente d'une vague ne peut
pas être comparée.

# wave-card_cat
## Variables catégorielles qui ont bougé
Une ligne par variable catégorielle dont la distribution a changé entre la vague
sélectionnée dans le panneau de gauche et les vagues de comparaison.

- Un bloc de colonnes par vague, avec `missing` (part de valeurs manquantes) et
  `Nmod` (nombre de modalités observées).
- Une case devient rouge quand la variation entre vagues dépasse le seuil
  *Taux de variation minimum* (bouton **Seuils de détection**).
- Les variables calculées sur trop peu de lignes ou trop peu de valeurs valides
  sont ignorées (seuils *Nombre minimum de lignes* et *Nombre minimum de valeurs
  valides*).

Cliquez sur une ligne pour afficher le graphique à droite. Cas typiques : une
modalité qui disparaît, un saut de valeurs manquantes, un recodage appliqué à
une seule vague.

# wave-card_cat_detail
## Répartition entre les vagues
Part de chaque modalité, vague par vague, pour la variable sélectionnée dans le
tableau de gauche.

- Les valeurs manquantes apparaissent sous la modalité `.NA`.
- Les couleurs sont fixées par modalité : une même modalité garde sa couleur
  d'une vague à l'autre.
- Au-delà de 15 modalités, le graphique n'est pas tracé (illisible).

Une rupture dans les bandes de couleur entre deux vagues confirme ce que le
tableau a signalé ; des bandes stables suggèrent une fausse alerte.

# wave-card_num
## Variables numériques qui ont bougé
Une ligne par variable numérique dont le niveau a changé entre la vague
sélectionnée dans le panneau de gauche et les vagues de comparaison.

- Un bloc de colonnes par vague, avec `missing` (part de valeurs manquantes) et
  `median` (médiane).
- Une case devient rouge quand la variation entre vagues dépasse le seuil
  *Taux de variation minimum* (bouton **Seuils de détection**).
- La médiane est préférée à la moyenne, car moins sensible aux valeurs
  extrêmes.

Cliquez sur une ligne pour afficher le graphique à droite. Une médiane
multipliée ou divisée par un facteur rond signale en général un changement
d'unité ou d'échelle plutôt qu'une évolution réelle.

# wave-card_num_detail
## Densité entre les vagues
Densité de la variable sélectionnée dans le tableau de gauche, une courbe par
vague.

- Le graphique est tronqué aux 1er et 99e centiles, pour que quelques valeurs
  extrêmes n'écrasent pas les courbes.
- Des courbes superposées indiquent une distribution stable ; une courbe
  décalée ou resserrée confirme la rupture signalée par le tableau.

# intvwr-card_indics
## Choix des indicateurs
Indicateurs utilisés dans le tableau *Synthèse* et dans le score.

Décocher un indicateur le retire du tableau **et** du calcul du score. Utile
quand un indicateur n'a pas de sens pour l'enquête, ou pour vérifier si
le classement ne repose pas sur un seul indicateur.

# intvwr-card_synthesis
## Synthèse par enquêteur·rice
Une ligne par enquêteur·rice, pour la vague et le filtre sélectionnés dans le
panneau de gauche.

- `RANK` classe les enquêteur·rices selon le score d'anomalie (Isolation
  Forest) : plus la combinaison d'indicateurs est rare, plus le rang est élevé.
- `NB_INTV` est le nombre d'entretiens, `INDEX_H` l'homogénéité des réponses,
  `MAX_CHI2` le plus grand écart observé sur une variable.
- Les indicateurs en `DURATION_` viennent des timers, ceux en `PC_` viennent 
  de Blaise.
- Une case rouge dépasse de plus de 2 écarts-types la moyenne de sa colonne.
- Les enquêteur·rices sous le seuil *Nombre minimum de lignes* ne sont pas
  affichés.

Le rang oriente l'examen, il ne prouve rien : cliquez sur une ligne pour ouvrir
le détail dans les onglets du dessous.

# intvwr-card_distrib
## Détail par indicateur
Un histogramme par indicateur, sur l'ensemble des enquêteur·rices affichés.

La ligne rouge verticale marque la valeur de l'enquêteur·rice sélectionné dans
le tableau *Synthèse*. Elle montre s'il est réellement isolé, ou simplement au
bord d'une distribution large — ce que le rang seul ne dit pas.

# intvwr-card_intv
## Entretiens de l'enquêteur·rice
Tous les entretiens de l'enquêteur·rice sélectionné, reconstitués à partir des
timers.

- `DURATION_INTV` est la durée totale, `DURATION_INTER_INTV` l'écart avec
  l'entretien précédent.
- Un 🔴 signale un drapeau : entretien trop court, travail de nuit, plusieurs
  sessions, ou écart négatif (entretiens qui se chevauchent).

Cliquez sur un entretien pour ouvrir ses sessions en dessous.

# intvwr-card_ssn
## Sessions de l'entretien
Un entretien peut être découpé en plusieurs sessions (interrompu puis repris).

- Une ligne par session, avec son début, sa durée et l'écart avec la
  précédente.
- `CHECK_SCTN` affiche un point par section du questionnaire : 🟢 la section a
  été renseignée dans cette session, 🔴 elle ne l'a pas été.

Beaucoup de sessions très courtes, ou des sections renseignées dans le
désordre, méritent un coup d'œil.

# intvwr-card_details
## Détail de l'entretien
Données brutes des timers pour l'entretien sélectionné : une ligne par item du
questionnaire, avec son horodatage et sa durée.

C'est le niveau le plus fin disponible. Il sert à confirmer une anomalie vue
plus haut, par exemple une série d'items répondus en quelques secondes.

# intvwr-card_var
## Variables de l'enquêteur·rice
Variables de l'enquêteur·rice sélectionné, classées par écart avec les autres
enquêteur·rices de la même vague et du même filtre.

- `chi2` mesure la distance entre la distribution de l'enquêteur·rice et la
  distribution de référence.
- `standard` est cette distance normalisée par variable : c'est elle qui est
  comparée au seuil *Distance de chi² minimum*.
- Les variables numériques sont découpées au préalable en 5 classes : elles se
  comparent donc comme les variables catégorielles.

Cliquez sur une variable pour afficher sa distribution à droite.

# intvwr-card_var_distrib
## Comparaison des distributions
Distribution de la variable sélectionnée pour l'enquêteur·rice (en foncé) face à
l'ensemble des autres enquêteur·rices (en gris).

- Variable catégorielle : part de chaque modalité, le pourcentage affiché est
  celui de l'enquêteur·rice.
- Le tableau à côté donne les statistiques usuelles pour
  l'enquêteur·rice et le reste des interviews (éventuellement filtrés).

Lisez la forme, pas seulement l'écart : une modalité jamais utilisée, ou
utilisée systématiquement, est plus parlante qu'une petite différence répartie
sur tout.

# intvwr-card_var_mods
## Distribution des modalités
Pour chaque modalité de la variable sélectionnée, la distribution de sa part
**parmi tous les enquêteur·rices**, avec une ligne rouge sur la valeur de
l'enquêteur·rice.

Cette carte répond à une question que la précédente ne traite pas : cet
enquêteur·rice est-il isolé sur cette modalité, ou beaucoup d'autres sont-ils au
même niveau ?

# intvwr-card_geo
## Localisation des entretiens
Répartition des entretiens de l'enquêteur·rice sélectionné selon la variable
géographique déclarée dans `config.txt` (`var_info_geo`).

Un·e enquêteur·rice couvrant une zone inhabituelle, ou une seule petite zone,
peut expliquer des écarts observés ailleurs : les répondant·es ne sont tout
simplement pas les mêmes.

# intvwr-card_stat
## Nombre d'entretiens par vague
Nombre d'entretiens de l'enquêteur·rice sélectionné·e, par vague et par valeur 
du filtre.

Cette carte n'est pas filtrée par le panneau de gauche : elle montre tout
l'historique. Elle permet d'analyser l'expérience et le travail actuel d'un·e
enquêteur·rice.

# intvwr_variable-card_cross
## Écarts enquêteur·rice × variable
Une ligne par couple enquêteur·rice / variable dont l'écart dépasse le seuil
*Distance de chi² minimum*.

- `chi2` est la distance entre la distribution de l'enquêteur·rice et la
  distribution de référence de la vague et du filtre.
- `standard` est cette distance normalisée par variable, ce qui rend les
  variables comparables entre elles.
- Les variables numériques sont découpées au préalable en 5 classes : tout le
  tableau se lit donc de la même façon.

Le tableau est trié par écart décroissant : c'est le point d'entrée quand on ne
sait pas par où commencer. Cliquez sur une ligne pour afficher les
distributions en bas de page.

# intvwr_variable-heatmap
## Carte des anomalies
Une ligne par enquêteur·rice, une colonne par variable. Une case est colorée
quand l'enquêteur·rice s'écarte des autres sur cette variable.

- *Choix des lignes et colonnes* zoome sur les lignes à risque, les colonnes à
  risque, ou uniquement les cases au-dessus du seuil.
- *Choix du classement* réordonne lignes et colonnes pour regrouper les profils
  proches (sériation) ; `None` conserve l'ordre alphabétique.
- Survolez une case pour voir les valeurs sous-jacentes, cliquez dessus pour
  ouvrir les distributions en bas de page.

Une case isolée n'est pas un problème. Une ligne rouge signale un comportement
récurrent, une colonne rouge désigne une variable fragile plutôt que les
enquêteur·rices.

# intvwr_variable-card_intvwr_ranking
## Classement des enquêteur·rices
Enquêteur·rices classé·es par score d'anomalie, calculé sur leurs écarts sur
l'ensemble des variables.

- `score` vient d'un Isolation Forest : plus le profil d'écarts est atypique,
  plus le score est élevé.
- `N_outliers` compte les variables au-dessus du seuil de détection.
- `Nrow` est le nombre d'entretiens, utile pour relativiser un score construit
  sur peu d'observations.

Sélectionnez un·e enquêteur·rice pour lister ses variables à droite.

# intvwr_variable-card_variable_listing
## Variables de l'enquêteur·rice sélectionné
Variables de l'enquêteur·rice sélectionné à gauche, avec leur écart (`diff`),
triées par valeur absolue décroissante.

Cette liste indique quelles variables portent le score, et si elles partagent un
thème (même section du questionnaire, même routage) — ce qui oriente vers une
cause de procédure plutôt qu'individuelle.

# intvwr_variable-card_variable_ranking
## Classement des variables
Même principe que le classement des enquêteur·rices, vu du côté des variables.

- `score` est élevé quand les écarts de cette variable sont inégalement
  répartis entre les enquêteur·rices.
- `N_outliers` compte les enquêteur·rices au-dessus du seuil de détection.

Une variable en tête de ce tableau relève souvent d'un problème de
questionnaire (formulation ambiguë, routage) plutôt que d'un·e enquêteur·rice.
Sélectionnez-la pour lister les enquêteur·rices concernés à droite.

# intvwr_variable-card_intvwr_listing
## Enquêteur·rices de la variable sélectionnée
Enquêteur·rices qui s'écartent sur la variable sélectionnée à gauche, avec leur
écart (`diff`), triés par valeur absolue décroissante.

Si un ou deux enquêteur·rices ressortent, la cause est probablement
individuelle. S'ils sont nombreux, regardez la variable elle-même, ou un effet
de sous-population (essayez le filtre du panneau de gauche).

# intvwr_variable-card_distrib
## Comparaison des distributions
Distribution de la variable sélectionnée pour l'enquêteur·rice sélectionné (en
foncé) face à l'ensemble des autres (en gris).

- Part de chaque modalité, le pourcentage affiché est
  celui de l'enquêteur·rice.
- Une variable numérique est transformé en catégorielle par quintile avant 
  analyse
- Le tableau à côté donne les statistiques usuelles pour l'enquêteur·rice.

Cette carte se remplit en cliquant sur une case de la heatmap ou sur une ligne
des tableaux du dessus.

# intvwr_variable-card_distrib_mods
## Distribution des modalités
Pour chaque modalité de la variable sélectionnée, la distribution de sa part
parmi tous les enquêteur·rices, avec une ligne rouge sur la valeur de
l'enquêteur·rice sélectionné.

Elle permet de vérifier si l'enquêteur·rice est réellement isolé sur une
modalité, ou si toute l'équipe s'étale sur une plage large.

# data-card_data
## Données brutes
Les données de l'enquête telles qu'elles ont été préparées, sans autre filtre
que la sélection de variables au-dessus.

Utile pour vérifier une valeur vue dans une analyse, ou comprendre comment une
variable est codée. Les cases de recherche en haut de chaque colonne filtrent
les lignes ; les valeurs manquantes sont affichées `NA`.

# dict-card_dict
## Dictionnaire des variables
Libellés et descriptions des variables, issus du fichier dictionnaire fourni au
lancement.

C'est l'endroit où vérifier la formulation exacte d'une question avant de
conclure sur un écart : une différence entre vagues peuvent s'expliquer par un
changement de libellé ou de routage.

# archive-card_archive
## Archive des observations
Commentaires enregistrés depuis l'interface avec le bouton **Ajouter à
l'archive**.

Chaque ligne conserve la date, l'utilisateur·rice, l'enquête, l'enquêteur·rice
et la variable concerné·es, ainsi que le commentaire. Le fichier est partagé par
toutes les personnes qui utilisent le même chemin d'archive.

Notez ce que vous avez vérifié, y compris les fausses alertes : cela évite à la
personne suivante de refaire la même investigation.

# summary-card_survey
## L'enquête chargée
Identité de l'enquête sélectionnée dans le panneau de gauche.

- *Préparé le* est la date du dernier `prepa_survey()` : tout ce que vous voyez
  dans l'application date de ce moment, pas des données en cours de collecte.
- *Vague*, *Filtre* et *Enquêteur·rice* rappellent quelles variables jouent ces
  rôles, telles que déclarées dans `config.txt`. Deux variables séparées par une
  barre oblique signalent une ventilation à deux niveaux.
- *Timers* indique si l'audit trail est disponible ; sans lui, les détails des
  entretiens et des sessions restent vides.

# summary-card_waves
## Entretiens par vague
Nombre d'entretiens de chaque vague, sur toute l'enquête — ce graphique n'est
pas affecté par la sélection du panneau de gauche.

Quand la vague a deux niveaux, chaque barre est découpée selon le second niveau
(les trimestres d'une année, par exemple). Une vague bien plus petite que les
autres est normale pendant la collecte, mais mérite vérification une fois
celle-ci terminée.

# summary-card_intvwr
## Par où commencer côté enquêteur·rices
Nombre d'enquêteur·rices présentant au moins un écart au-dessus des seuils de
détection, pour la vague et le filtre sélectionnés dans le panneau de gauche.

La liste donne les cinq enquêteur·rices avec le plus de variables concernées.
Ouvrez l'onglet *Enquêteur* pour le classement complet, les indicateurs d'audit
et le détail des entretiens.

# summary-card_wave
## Par où commencer côté vagues
Nombre de variables dont le niveau a bougé entre les vagues, pour le filtre
sélectionné dans le panneau de gauche.

La règle est celle de l'onglet *Vague* (variation d'un indicateur comparée au
seuil *Taux de variation minimum*), appliquée ici à toutes les vagues d'un coup.
L'onglet *Vague* permet, lui, de choisir les vagues à comparer : sa liste peut
donc être plus courte.

# summary-card_cross
## Par où commencer côté croisement
Nombre de couples enquêteur·rice × variable au-dessus des seuils de détection,
pour la vague et le filtre sélectionnés dans le panneau de gauche.

La liste donne les cinq variables signalées chez le plus grand nombre
d'enquêteur·rices — souvent le signe d'une variable fragile plutôt que d'un
problème d'enquêteur·rice. Ouvrez l'onglet *Croisé* pour la heatmap et les
classements.
