# List distribution of discrete variables

List distribution of discrete variables

## Usage

``` r
list_dist(df, vars_vd)
```

## Arguments

- df:

  data.frame

- vars_vd:

  vector of discrete variables

## Value

list

## Examples

``` r
list_dist(mtcars,c("cyl","vs","gear"))
#> $cyl
#>       4       6       8 
#> 0.34375 0.21875 0.43750 
#> 
#> $vs
#>      0      1 
#> 0.5625 0.4375 
#> 
#> $gear
#>       3       4       5 
#> 0.46875 0.37500 0.15625 
#> 
```
