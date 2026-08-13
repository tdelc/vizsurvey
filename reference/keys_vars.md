# Keys (modalities) of a wave or filter variable

Keys (modalities) of a wave or filter variable

## Usage

``` r
keys_vars(df, vars, var_key, level = "all", sep = " / ")
```

## Arguments

- df:

  data.frame

- vars:

  variables of each level (see vars_levels)

- var_key:

  name of the variable containing the complete key

- level:

  1 (first level only), 2 (complete key) or "all" (both)

- sep:

  separator between the levels

## Value

vector of keys

## Examples

``` r
keys_vars(mtcars,"cyl","cyl")
#> [1] "4" "6" "8"
```
