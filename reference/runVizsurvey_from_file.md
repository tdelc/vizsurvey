# Shiny vizsurvey from a csv/tsv

Shiny vizsurvey from a csv/tsv

## Usage

``` r
runVizsurvey_from_file(
  path,
  vars_discretes = NULL,
  vars_continous = NULL,
  var_wave = NULL,
  var_zone = NULL,
  var_group = NULL
)
```

## Arguments

- path:

  path of a data.frame (can be readed by fread)

- vars_discretes:

  (optional) preset of discretes variables

- vars_continous:

  (optional) preset of continous variables

- var_wave:

  (optional) name of wave variable

- var_zone:

  (optional) name of zone variable

- var_group:

  (optional) name of group variable

## Value

shinyapp

## Examples

``` r
path <- "inst/extdata/SILC/HFILE/BE_2012h_EUSILC.csv"
if (FALSE) runVizsurvey_from_file(path,var_itw = "NR_ITW",var_zone = "db040") # \dontrun{}
```
