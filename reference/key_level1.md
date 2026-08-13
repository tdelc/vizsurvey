# First level of a multi-level key

First level of a multi-level key

## Usage

``` r
key_level1(x, sep = " / ")
```

## Arguments

- x:

  vector of keys

- sep:

  separator between the levels

## Value

vector of the first level values

## Examples

``` r
key_level1(c("2024 / T1","2024 / T2","2023 / T4"))
#> [1] "2024" "2024" "2023"
```
