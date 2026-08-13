# Combine several variables in one key (multi-level wave or filter)

Used when var_wave (or var_filter) contains more than one variable in
the configuration file, for example the year and the quarter. The
complete key of a row is then "2024 / T1".

## Usage

``` r
combine_vars(df, vars, sep = " / ")
```

## Arguments

- df:

  data.frame

- vars:

  vector of the variable of each level

- sep:

  separator between the levels

## Value

vector of the complete keys

## Examples

``` r
head(combine_vars(mtcars,c("cyl","gear")))
#> [1] "6 / 4" "6 / 4" "4 / 4" "6 / 3" "8 / 3" "6 / 3"
```
