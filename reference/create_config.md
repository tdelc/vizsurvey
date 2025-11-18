# Create a template of configuration file

Create a template of configuration file

## Usage

``` r
create_config(
  folder_path,
  file_name = "config.txt",
  name_survey = NULL,
  vars_discretes = NULL,
  vars_continous = NULL,
  var_wave = NULL,
  var_zone = NULL,
  var_group = NULL
)
```

## Arguments

- folder_path:

  folder where create the file

- file_name:

  Name of the config file (config.txt by default)

- name_survey:

  Name of the survey (not used)

- vars_discretes:

  (optional) preset discretes variables name (VAR1,VAR2,...)

- vars_continous:

  (optional) preset continous variables name (VAR1,VAR2,...)

- var_wave:

  (optional) variable name of wave

- var_zone:

  (optional) variable name of zone

- var_group:

  variable name of group

## Examples

``` r
create_config(".") # creation of config.txt in working directory
#> ✔ File ./config.txt created.
```
