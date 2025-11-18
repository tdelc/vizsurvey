# Create a heatmap

Create a heatmap

## Usage

``` r
heatmap_group(df_stats, threshold = 5, color = "red2")
```

## Arguments

- df_stats:

  data frame from prepa_stats function

- threshold:

  threshold to show difference

- color:

  color of the cells

## Value

heatmap (ggplot)

## Examples

``` r
library(laeken)
data(eusilc)

df_stats <- prepa_stats(eusilc, "db040")
#> Warning: There was 1 warning in `summarise()`.
#> ℹ In argument: `across(...)`.
#> ℹ In group 1: `db040 = "Burgenland"`.
#> Caused by warning in `stats::chisq.test()`:
#> ! Chi-squared approximation may be incorrect
heatmap_group(df_stats, 5)
```
