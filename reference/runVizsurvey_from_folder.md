# Shiny vizsurvey with already prepared data

Shiny vizsurvey with already prepared data

## Usage

``` r
runVizsurvey_from_folder(
  path,
  path_dict = NULL,
  path_archive = NULL,
  data_rds_pattern = "global",
  depth_folder = 1
)
```

## Arguments

- data_rds_pattern:

  name of the rds file contains all the data

- depth_folder:

  level of depth for the tree structure

- link:

  link to directory of data

## Value

shinyapp

## Examples

``` r
# We assume that config.txt, and prepa_surveys are already done here.
if (FALSE) runVizsurvey_from_folder("inst/extdata",depth_folder = 3) # \dontrun{}
```
