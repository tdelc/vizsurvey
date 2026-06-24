# Create a template of configuration file

Create a template of configuration file

## Usage

``` r
create_config(
  folder_path,
  file_name = "config.txt",
  name_survey = NULL,
  vars_discretes = NULL,
  vars_continuous = NULL,
  vars_ignore = NULL,
  prefix_discretes = NULL,
  prefix_continuous = NULL,
  prefix_ignore = NULL,
  var_wave = NULL,
  var_filter = NULL,
  var_intvwr = NULL,
  var_intv = NULL,
  var_date = NULL,
  var_info_geo = NULL,
  var_timer = NULL,
  var_itm_duration = NULL,
  var_session = NULL,
  duration_min_during = NULL,
  duration_min_inter = NULL,
  night_start = NULL,
  night_end = NULL
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

- vars_continuous:

  (optional) preset continuous variables name (VAR1,VAR2,...)

- prefix_discretes:

  (optional) preset prefix for discretes variables name

- prefix_continuous:

  (optional) preset prefix for continuous variables name

- var_wave:

  (optional) variable name of wave

- var_filter:

  (optional) variable name of filter

- var_intvwr:

  (optional) variable name of interviewer id

- var_intv:

  (optional) variable name of interview id

- var_date:

  (optional) variable name of the date of the interview

- var_timer:

  (optional, audit trail) variable name of the beginning of a item

- var_itm_duration:

  (optional, audit trail) variable name of the duration of a item

- var_session:

  (optional, audit trail) variable name of the session of a item

- duration_min_during:

  (optional, audit trail) threshold of a interview duration

- duration_min_inter:

  (optional, audit trail) threshold of the duration between two
  interviews

- night_start:

  (optional, audit trail) hour of beginning of the night

- night_end:

  (optional, audit trail) hour of end of the night

## Examples

``` r
create_config(".") # creation of config.txt in working directory
#> ✔ File ./config.txt created.
```
