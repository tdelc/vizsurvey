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
heatmap_group(df_stats, 5)
```
