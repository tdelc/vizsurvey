# Specific chisq test to NA and Other modality

Specific chisq test to NA and Other modality

## Usage

``` r
my_chisq_test(x, varname, ldist, useNA = "ifany")
```

## Arguments

- x:

  value to procede chisq test

- varname:

  name of the variable

- ldist:

  named list of expected probability

- useNA:

  useNA parameter for the table function

## Value

chisq value

## Examples

``` r
ldist <- list_dist(mtcars,c("cyl","gear"))
sub_mtcars <- subset(mtcars,vs == 1)
my_chisq_test(sub_mtcars$cyl,"cyl",ldist)
#> [1] 12.00371
```
