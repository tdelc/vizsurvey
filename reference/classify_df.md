# Classify all variable of a data.frame

Classify all variable of a data.frame

## Usage

``` r
classify_df(df, threhold = 15)
```

## Arguments

- df:

  A data frame

- threhold:

  Maximum number of modalities to classify variable as modal

## Value

a data frame

## Examples

``` r
classify_df(iris)
#> # A tibble: 5 × 2
#>   variable     type      
#>   <chr>        <chr>     
#> 1 Petal.Length Continuous
#> 2 Petal.Width  Continuous
#> 3 Sepal.Length Continuous
#> 4 Sepal.Width  Continuous
#> 5 Species      Modal     
```
