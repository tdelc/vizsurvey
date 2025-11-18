# tranform data from folder to config and df

tranform data from folder to config and df

## Usage

``` r
folder_to_df(folder, file_pattern = "*.csv", file_config = "config.txt")
```

## Arguments

- folder:

  folder of databases

- file_pattern:

  pattern of the databases (\*.csv by default)

- file_config:

  name of the configuration file (config.txt by default)

## Value

list(df,configs)

## Examples

``` r
if (FALSE) { # \dontrun{
folder_to_df("ESS10")
} # }
```
