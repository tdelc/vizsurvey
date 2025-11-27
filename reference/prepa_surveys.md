# Preparation of all surveys from a folder

Preparation of all surveys from a folder

## Usage

``` r
prepa_surveys(
  folder_path,
  depth_folder = 1,
  file_pattern = "*.csv",
  file_config = "config.txt"
)
```

## Arguments

- folder_path:

  folder of the folders of survey

- depth_folder:

  level of depth for the tree structure

- file_pattern:

  pattern of the databases (\*.csv by default)

- file_config:

  name of the configuration file (config.txt by default)

## Value

NULL (creation of rds)

## Examples

``` r
if (FALSE) { # \dontrun{
prepa_surveys("inst/extdata/SILC/HFILE")
} # }
```
