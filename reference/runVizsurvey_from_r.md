# Shiny vizsurvey from a R data.frame

Shiny vizsurvey from a R data.frame

## Usage

``` r
runVizsurvey_from_r(
  df,
  vars_discretes = NULL,
  vars_continous = NULL,
  var_wave = NULL,
  var_zone = NULL,
  var_group = NULL
)
```

## Arguments

- df:

  data.frame

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
library(laeken)
data(eusilc)
set.seed(123)
eusilc$NR_ITW <- paste(eusilc$db040,sample(1:5,nrow(eusilc),replace = TRUE),sep="-")
if (FALSE) runVizsurvey_from_r(eusilc,var_group = "NR_ITW",var_zone = "db040") # \dontrun{}
```
