# Rows of a data.frame matching one or several keys

A row matches a key when its complete key is one of the keys, or when
its first level is one of the keys (selection of a year without the
quarter).

## Usage

``` r
match_keys(df, vars, var_key, keys, sep = " / ")
```

## Arguments

- df:

  data.frame

- vars:

  variables of each level (see vars_levels)

- var_key:

  name of the variable containing the complete key

- keys:

  vector of keys to keep

- sep:

  separator between the levels

## Value

logical vector

## Examples

``` r
match_keys(mtcars,"cyl","cyl",4)
#>  [1] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE
#> [13] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE
#> [25] FALSE  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE
```
