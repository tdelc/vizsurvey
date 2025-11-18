# Create a summarise of all the difference

Create a summarise of all the difference

## Usage

``` r
prepa_stats(df, var_group, vars_vd = NULL, vars_vc = NULL)
```

## Arguments

- df:

  data frame for the summary

- vars_vd:

  (optional) Vector of discrete variables

- vars_vc:

  (optional) Vector of continuous variables

- var_zone:

  Name of zone variable

## Value

data frame

## Examples

``` r
library(laeken)
data(eusilc)

info_vars <- classify_df(eusilc)
vars_vd <- info_vars[info_vars$type == "Modal", ]$variable
vars_vc <- info_vars[info_vars$type == "Continuous", ]$variable
prepa_stats(eusilc, "db040", vars_vd, vars_vc)
#> Warning: There was 1 warning in `summarise()`.
#> ℹ In argument: `across(...)`.
#> ℹ In group 1: `db040 = "Burgenland"`.
#> Caused by warning in `stats::chisq.test()`:
#> ! Chi-squared approximation may be incorrect
#> # A tibble: 972 × 9
#>    db040      variable  Nrow  Nval type  stat      value value_ref standard
#>    <chr>      <chr>    <int> <dbl> <chr> <chr>     <dbl>     <dbl>    <dbl>
#>  1 Burgenland hsize      549   549 cha   missing   0         0        0    
#>  2 Burgenland hsize      549   549 cha   presence  1         1        0    
#>  3 Burgenland hsize      549   549 cha   Nmod      6         7.89    -2.04 
#>  4 Burgenland hsize      549   549 cha   chi2     37.3      67.3     -0.234
#>  5 Burgenland pb220a     549   476 cha   missing   0.133     0.184   -1.28 
#>  6 Burgenland pb220a     549   476 cha   presence  1         1        0    
#>  7 Burgenland pb220a     549   476 cha   Nmod      4         4        0    
#>  8 Burgenland pb220a     549   476 cha   chi2     30.6      35.7      0    
#>  9 Burgenland pl030      549   476 cha   missing   0.133     0.184   -1.28 
#> 10 Burgenland pl030      549   476 cha   presence  1         1        0    
#> # ℹ 962 more rows
```
