# Variables of each level of the wave or of the filter

Kept compatible with the global.rds prepared before the multi-level
option : vars_wave (or vars_filter) is then missing and var_wave is
used.

## Usage

``` r
vars_levels(configs, type = "wave")
```

## Arguments

- configs:

  list of configuration

- type:

  "wave" or "filter"

## Value

vector of the variables of each level

## Examples

``` r
vars_levels(list(var_wave = "YEAR"),"wave")
#> [1] "YEAR"
```
