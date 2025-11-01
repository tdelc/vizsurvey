
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

``` r
install.github('tdelc/vizsurvey')
```

## Example

This is a basic example how to lauch the dashboard:

``` r
library(vizsurvey)

# vizsurvey_dashboard() # To be created

# OR

# vizsurvey_dashboard("link") # Link to your data, # To be created
```
