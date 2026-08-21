<!--
Visite guidée — version française. Même principe que help_fr.md.

Une section = une étape, dans l'ordre du fichier.
  tab:    onglet à ouvrir (facultatif)
  target: sélecteur CSS de l'élément à entourer (facultatif -> bulle centrée)
  ##      titre de la bulle, le reste est du markdown
  {xxx}   valeur des données courantes, voir tour_values() dans mod_tour.R
          {survey} {n_rows} {n_vars} {n_intvwr} {n_waves}
          {wave} {filter} {n_outliers} {var_intvwr} {var_wave}

Si la cible n'existe pas à l'écran, la bulle s'affiche au centre sans
encadrement : l'étape reste lisible.
-->

# 01-bienvenue
tab: tab_summary
## Bienvenue dans vizsurvey
Ce tutoriel parcourt l'interface en dix étapes, sur l'enquête que vous avez
chargée : **{survey}**, soit {n_rows} entretiens et {n_vars} variables
analysées.

vizsurvey ne conclut rien à votre place : il calcule des écarts et les met en
évidence pour orienter votre analyse de l'enquête.

Utilisez **Suivant**, les flèches du clavier, ou **Échap** pour quitter.

# 02-source
target: #source-path_survey
## Choisir l'enquête
Vous devez commencer par choisir le dossier puis l'enquête. Les statistiques 
sont préparées à l'avance par `prepa_survey()` et stockées dans un fichier
`global.rds`.

Changer d'enquête recharge instantanément tous les onglets.

# 03-filtres
target: #filters-config_wave
## La vague et le filtre
Ces deux sélecteurs commandent **tout le reste de l'application**. Vous êtes
actuellement sur la vague **{wave}** et le filtre **{filter}**.

Cette enquête compte {n_waves} vagues, définies par la variable {var_wave}.
Le filtre restreint l'analyse à une sous-population : utile pour vérifier
qu'un écart n'est pas simplement un effet de zone géographique.

# 04-seuils
target: #filters-button_parms
## Les seuils de détection
Les seuils déterminent à partir de quel écart une case devient rouge, 
et combien de lignes ou de valeurs valides sont exigées pour qu'une 
variable soit analysée.

Il n'existe pas de seuil statistiquement fondé : c'est à votre équipe de le
régler par tâtonnement. Baissez-le pour voir plus de cas, montez-le pour ne
garder que les cas les plus flagrants.

# 05-resume
tab: tab_summary
target: #summary-card_intvwr
## Par où commencer
Le premier onglet donne trois points d'entrée chiffrés. Pour la vague et le
filtre courants, {n_outliers} couples enquêteur·rice × variable dépassent les
seuils.

Chaque carte a un bouton qui ouvre l'onglet correspondant : c'est le chemin
le plus court entre « il y a quelque chose » et « voici quoi ».

# 06-vague
tab: tab_wave
target: #wave-card_cat
## Cohérence entre les vagues
Ici on compare la vague sélectionnée à une ou plusieurs autres vagues : 
une variable s'affiche si au moins un élément de la distribution a varier 
(selon le seuil défini). Il y a un tableau pour les variables catégorielles 
et un tableau pour les variables continues.

Les cas typiques sont une modalité qui apparaît ou disparaît, un saut de 
valeurs manquantes, ou un changement d'unité. 

Cliquez une ligne : le graphique de droite montre l'évolution, vague par vague.

# 07-enqueteur
tab: tab_intvwr
target: #intvwr-card_synthesis
## Cohérence entre enquêteur·rices
Une ligne par enquêteur·rice, classée par score d'anomalie. Le score vient
d'un Isolation Forest : plus la combinaison d'indicateurs est rare, plus le
rang est élevé.

L'enquête {survey} compte actuellement {n_intvwr} enquêteur·rices, 
identifiés par la variable {var_intvwr}. Cliquez une ligne pour ouvrir 
le détail : entretiens, sessions, variables, distributions.

# 09-croise
tab: tab_intvwr_variable
target: #intvwr_variable-cross_ranking
## Le croisement enquêteur·rice × variable
Le tableau croise chaque enquêteur·rice et chaque variable de l'enquête. 
Pour chaque croisement, on mesure l'écart (indicateur chi²) entre la 
distribution de la variable pour cet·te enquêteur·rice et celle pour 
le reste de la population.

Les variables continues sont d'abord transformée en cinq quantiles 
(+ missings) avant de calculer la distance de chi².

Les écarts sont classement par ordre décroissant pour observer en priorité 
les cas les plus surprenants. Cliquez une case pour voir les distributions 
comparées en bas de page.

# 08-croise2
tab: tab_intvwr_variable
target: #intvwr_variable-heatmap
## La carte de chaleur (heatmap)
Une ligne par enquêteur·rice, une colonne par variable, une case est colorée 
quand l'écart dépasse le seuil.

La lecture tient en trois règles : une case isolée n'est pas forcément un 
problème, une **ligne** rouge signale un comportement récurrent, 
une **colonne** rouge désigne une variable sans doute complexe à comprendre. 
Cliquez une case pour voir les distributions comparées en bas de page.

# 10-archive
target: #archive-add_button
## Garder une trace
Quand vous avez vérifié un cas, notez-le. L'archive conserve la date,
l'utilisateur·rice, l'enquête, l'enquêteur·rice et la variable concernés.

Notez aussi les fausses alertes : c'est ce qui évite à la personne suivante
de refaire la même investigation.

# 11-aide
tab: tab_summary
target: #summary-card_survey .btn-link
## L'aide de chaque carte
Ce petit **?** est présent sur toutes les cartes de l'application. Il explique
ce que la carte affiche, ce que valent ses colonnes, et comment la lire.
