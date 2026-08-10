# Shiny vizsurvey from a R data.frame

Shiny vizsurvey from a R data.frame

## Usage

``` r
runVizsurvey_from_r(
  df,
  vars_discretes = NULL,
  vars_continuous = NULL,
  var_wave = NULL,
  var_filter = NULL,
  var_intvwr = NULL
)
```

## Arguments

- df:

  data.frame

- vars_discretes:

  (optional) preset of discretes variables

- var_wave:

  (optional) name of wave variable

- var_filter:

  (optional) name of filter variable

- var_intvwr:

  (optional) name of interviewer variable

- vars_continous:

  (optional) preset of continous variables

## Value

shinyapp

## Examples

``` r
library(laeken)
data(eusilc)
set.seed(123)
eusilc$NR_ITW <- paste(eusilc$db040,sample(1:5,nrow(eusilc),replace = TRUE),sep="-")
if (FALSE) runVizsurvey_from_r(eusilc,var_intvwr = "NR_ITW",var_filter = "db040") # \dontrun{}
```
