# Preparation of all surveys from a folder

Preparation of all surveys from a folder

## Usage

``` r
prepa_surveys(folder_path, depth_folder = 1, ...)
```

## Arguments

- folder_path:

  folder of the folders of survey

- depth_folder:

  level of depth for the tree structure

- ...:

  argument to pass to folder_to_df (example : file_pattern)

## Value

NULL (creation of rds)

## Examples

``` r
if (FALSE) { # \dontrun{
prepa_surveys("inst/extdata/SILC/HFILE")
} # }
```
