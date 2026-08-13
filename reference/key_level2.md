# Other levels of a multi-level key (used as label in the interface)

Other levels of a multi-level key (used as label in the interface)

## Usage

``` r
key_level2(x, sep = " / ")
```

## Arguments

- x:

  vector of keys

- sep:

  separator between the levels

## Value

vector of the values after the first level

## Examples

``` r
key_level2(c("2024 / T1","2024 / T2","2023 / T4"))
#> [1] "T1" "T2" "T4"
```
