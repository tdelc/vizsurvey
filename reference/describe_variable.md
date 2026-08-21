# Description d'une variable, globale ou par groupe

Remplace summarytools::dfSummary. Renvoie un data.frame prêt à afficher
: une ligne par modalité (variables catégorielles) ou par statistique
(variables continues), une colonne par groupe.

## Usage

``` r
describe_variable(
  x,
  by = NULL,
  type = c("auto", "categorical", "continuous"),
  max_levels = 20,
  digits = 1
)
```

## Arguments

- x:

  vecteur à décrire

- by:

  (optionnel) vecteur de même longueur, un bloc de colonnes par valeur

- type:

  "auto" (numérique et plus de 15 valeurs distinctes -\> continue),
  "categorical" ou "continuous"

- max_levels:

  nombre maximum de modalités affichées, le reste est regroupé dans
  "(other)"

- digits:

  nombre de décimales

## Value

data.frame

## Details

Les pourcentages des modalités sont calculés sur les valeurs valides,
celui de la ligne "(missing)" sur l'ensemble des lignes.
