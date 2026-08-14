# Create statistics from database

Create statistics from database

## Usage

``` r
create_df_stats(
  df_,
  configs_,
  var_calculs,
  mod_filter = NULL,
  na.rm = FALSE,
  force_cat = FALSE
)
```

## Arguments

- df\_:

  database

- var_calculs:

  variable to create stats

- mod_filter:

  (optional) modality to filter data

- na.rm:

  include or not missing values as modality

- configs:

  configs

## Value

df data.frame
