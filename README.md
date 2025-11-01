
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vizsurvey

<!-- badges: start -->
<!-- badges: end -->

vizsurvey is an R package designed to streamline the quality assessment
of survey data by providing intuitive visual diagnostics through an
interactive dashboard. It takes as input a survey database containing
annual or quarterly data, enriched with both interviewer-specific
variables and a wide array of survey variables.

The package outputs a user-friendly dashboard that highlights
anomalies—defined as deviations from expected norms—either by year or by
interviewer. A central feature of the tool is its heatmap visualization,
which allows users to quickly detect patterns of irregularities across
interviewers and variables, helping to identify potential data quality
issues or interviewer effects.

vizsurvey is especially useful for institutions or researchers
conducting large-scale surveys with multiple interviewers, enabling a
fast and systematic overview of data quality over time.

## Installation

You can install the development version of vizsurvey like so:

`install.github('tdelc/vizsurvey')`

## Example

This is tree option to lauch the dashboard:

### From a R object

``` r
library(vizsurvey)
library(laeken)
data(eusilc)
runVizsurvey_from_r(eusilc,var_itw = "db040")
```

### From a csv file

``` r
runVizsurvey_from_file(path="inst/shiny-examples/complete/data/ESS/ESS9/ESS9.csv",var_itw = "INTNUM1",var_domain="CNTRY",var_group = "PREINTF")
```

### From a directory of prepared data

``` r
# We assume that config.txt, and prepa_surveys are already done here.
runVizsurvey_from_folder("data",is_double_folder = T)
```
