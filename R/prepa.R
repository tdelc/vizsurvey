#' Classify all variable of a data.frame
#'
#' @param df A data frame
#' @param threhold Maximum number of modalities to classify variable as modal
#'
#' @returns a data frame
#' @export
#'
#' @examples
#' classify_df(iris)
classify_df <- function(df, threhold = 15) {
  df %>%
    summarise(across(everything(), ~ {
      if (n_distinct(.) == 1) {
        return("Solo")
      } else if (n_distinct(.) <= 15) {
        return("Modal")
      } else {
        if (is.numeric(.)) {
          return("Continuous")
        } else {
          return("Text")
        }
      }
    })) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable", values_to = "type"
    ) %>%
    arrange(variable)
}

classify_df_pattern <- function(df,configs){
  # List of all variables
  vars <- names(df)
  
  # Remove variable of calculation 
  vars_remove <- c(configs$var_intvwr,configs$var_intv,configs$var_date,
                   configs$var_wave,configs$var_filter)
  vars <- setdiff(vars, vars_remove)
  
  # 1. apply pattern and remove vars # PREFIX
  configs$prefix_discretes  <- paste0("^",configs$prefix_discretes)
  configs$prefix_continuous <- paste0("^",configs$prefix_continuous)
  vars_vd <- vars[grep(paste(configs$prefix_discretes,collapse = "|"),vars)]
  vars_vc <- vars[grep(paste(configs$prefix_continuous,collapse = "|"),vars)]
  
  if (is.na(configs$prefix_discretes)) vars_vd <- NULL
  if (is.na(configs$prefix_continuous)) vars_vc <- NULL
  vars_left <- setdiff(setdiff(vars,vars_vd),vars_vc)
  
  # 2. add force variable
  vars_vd <- c(vars_vd, intersect(vars,configs$vars_discretes))
  vars_vc <- c(vars_vc, intersect(vars,configs$vars_continuous))
  
  # 3. apply classify
  info_vars    <- classify_df(df)
  info_vars_vd <- info_vars[info_vars$type == "Modal", ]$variable
  info_vars_vc <- info_vars[info_vars$type == "Continuous", ]$variable
  
  other_vars <- setdiff(names(df),c(info_vars_vd,info_vars_vc))
  vars_vd <- c(vars_vd, intersect(vars_left,info_vars_vd))
  vars_vc <- c(vars_vc, intersect(vars_left,info_vars_vc))
  
  # 4. Force ignore some variables
  if (all(!is.na(configs$prefix_ignore))){
    configs$prefix_ignore <- paste0("^",configs$prefix_ignore)
    vars_ignore <- vars[grep(paste(configs$prefix_ignore,collapse = "|"),vars)]
  }else{
    vars_ignore <- NULL
  }
  vars_ignore <- c(vars_ignore,configs$vars_ignore)
  
  vars_vd <- setdiff(vars_vd, vars_ignore)
  vars_vc <- setdiff(vars_vc, vars_ignore)
  
  # 5. Format variables discretes and continuous
  
  vars_vd <- sort(unique(vars_vd))
  vars_vc <- sort(unique(vars_vc))
                           
  # 6. Checks
  vars <- setdiff(setdiff(setdiff(setdiff(vars,info_vars_vd),info_vars_vc),vars_ignore),other_vars)
  dup <- duplicated(c(vars_vd,vars_vc))
  dup <- paste(c(vars_vd,vars_vc)[dup],collapse = ",")
  
  mis <- paste(setdiff(names(df),c(vars_vd,vars_vc,other_vars,configs$var_intvwr)),collapse = ",")
  
  if (mis != "" | dup != "" | length(vars) != 0){
    cli::cli_alert_warning("Variables classification may not work :")
    cli::cli_alert_warning("{length(vars)} remaining variables")
    cli::cli_alert_warning("Duplicated : {dup}")
    cli::cli_alert_warning("Missing : {mis}")
    
    writeLines(paste0("Variables classification may not work :\n", 
                      length(vars)," remaining variables\n",
                      "Duplicated : ",dup,"\n",
                      "Missing : ",mis,"\n"),
               file.path(configs$path,"log.txt"))
  }
  list(
    vars_vd = vars_vd,
    vars_vc = vars_vc
  )
}

#' Create a summarise of all the difference
#'
#' @param df data frame for the summary
#' @param var_group Name of group variable
#' @param vars_vd (optional) Vector of discrete variables
#' @param vars_vc (optional) Vector of continuous variables
#'
#' @returns data frame
#' @export
#'
#' @examples
#' library(laeken)
#' data(eusilc)
#'
#' info_vars <- classify_df(eusilc)
#' vars_vd <- info_vars[info_vars$type == "Modal", ]$variable
#' vars_vc <- info_vars[info_vars$type == "Continuous", ]$variable
#' prepa_stats(eusilc, "db040", vars_vd, vars_vc)
prepa_stats <- function(df, var_group, vars_vd=NULL, vars_vc=NULL) {
  
  if (length(var_group) == 0) {
    return(tibble(NULL))
  }

  if (is.null(vars_vd) & is.null(vars_vc)){
    info_vars <- classify_df(df)
    vars_vd <- info_vars[info_vars$type == "Modal", ]$variable
    vars_vc <- info_vars[info_vars$type == "Continuous", ]$variable
  }

  vars_vd <- setdiff(vars_vd, var_group)
  vars_vc <- setdiff(vars_vc, var_group)

  vars_vd <- intersect(vars_vd, names(df))
  vars_vc <- intersect(vars_vc, names(df))

  df <- df %>%
    mutate(
      across(any_of(vars_vc), as.numeric),
      across(any_of(vars_vd), as.factor),
      across(any_of(vars_vd), as.numeric),
      across(any_of(var_group), as.character)
    )
  
  ldist <- list_dist(df,vars_vd)
  
  df_stats <- df %>%
    group_by(!!sym(var_group)) %>%
    summarise(
      Nrow = n(),
      across(
        .cols = tidyselect::all_of(vars_vd),
        .fns = list(
          Nval = ~ sum(!is.na(.x)),
          missing = ~ mean(is.na(.x)),
          presence = ~ mean(is.na(.x)) != 1,
          Nmod = ~ ifelse(mean(is.na(.x)) > 0.95, NA, length(unique(.x))),
          chi2 = ~ my_chisq_test(.x, cur_column(),ldist)
        ),
        .names = "{.col}|cha|{.fn}"
      ),
      across(
        .cols = tidyselect::all_of(vars_vc),
        .fns = list(
          Nval     = ~ sum(!is.na(.x)),
          missing  = ~ mean(is.na(.x)),
          presence = ~ mean(is.na(.x)) != 1,
          mean     = ~ mean(.x, na.rm = TRUE),
          median   = ~ median(.x, na.rm = TRUE)
        ),
        .names = "{.col}|num|{.fn}"
      )
    ) %>%
    pivot_longer(
      cols = -c(!!sym(var_group), Nrow),
      names_to = c("variable", "type", "stat"),
      names_pattern = "^(.*?)\\|(.*?)\\|(.*?)$"
    ) %>%
    group_by(!!sym(var_group), variable) %>%
    mutate(
      Nval = ifelse(stat == "Nval", value, NA),
      Nval = mean(Nval, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(stat != "Nval") %>%
    group_by(variable, type, stat) %>%
    mutate(
      value = ifelse(is.infinite(value), 1000, value),
      value_ref = mean(value, na.rm = TRUE),
      standard = scale_IQR(value),
      standard = case_when(
        sd(value, na.rm = TRUE) == 0 ~ 0,
        is.nan(standard) ~ NA,
        TRUE ~ standard
      )
    ) %>%
    dplyr::relocate(!!sym(var_group), variable, Nrow, Nval) %>%
    ungroup()

  return(df_stats)
}

#' Create a template of configuration file
#'
#' @param folder_path folder where create the file
#' @param file_name Name of the config file (config.txt by default)
#' @param name_survey Name of the survey (not used)
#' @param var_wave (optional) variable name of wave. Two variables can be given
#'   (c("YEAR","QUARTER")) : the second one is then a second level of wave
#' @param vars_discretes (optional) preset discretes variables name (VAR1,VAR2,...)
#' @param vars_continuous (optional) preset continuous variables name (VAR1,VAR2,...)
#' @param prefix_discretes (optional) preset prefix for discretes variables name
#' @param prefix_continuous (optional) preset prefix for continuous variables name 
#' @param var_filter (optional) variable name of filter. Two variables can be
#'   given : the second one is then a second level of filter
#' @param var_intvwr (optional) variable name of interviewer id
#' @param var_intv  (optional) variable name of interview id
#' @param var_date (optional) variable name of the date of the interview
#' @param var_timer (optional, audit trail) variable name of the beginning of a item
#' @param var_itm_duration (optional, audit trail) variable name of the duration of a item
#' @param var_session (optional, audit trail) variable name of the session of a item
#' @param duration_min_during (optional, audit trail) threshold of a interview duration 
#' @param duration_min_inter (optional, audit trail) threshold of the duration between two interviews 
#' @param night_start (optional, audit trail) hour of beginning of the night
#' @param night_end (optional, audit trail) hour of end of the night
#'
#' @returns NULL
#' @export
#'
#' @examples
#' create_config(".") # creation of config.txt in working directory
create_config <- function(folder_path, file_name = "config.txt",
                          name_survey         = NULL,
                          vars_discretes      = NULL,
                          vars_continuous     = NULL,
                          vars_ignore         = NULL,
                          prefix_discretes    = NULL,
                          prefix_continuous   = NULL,
                          prefix_ignore       = NULL,
                          var_wave            = NULL,
                          var_filter          = NULL,
                          var_intvwr          = NULL,
                          var_intv            = NULL,
                          var_date            = NULL,
                          var_info_geo        = NULL,
                          var_timer           = NULL,
                          var_itm_duration    = NULL,
                          var_session         = NULL,
                          duration_min_during = NULL,
                          duration_min_inter  = NULL,
                          night_start         = NULL,
                          night_end           = NULL) {
  if (!dir.exists(folder_path)) {
    stop("folder_path does not exists")
  }

  # complete path of config file
  file_path <- file.path(folder_path, file_name)

  # several variables are written on the same line (VAR1,VAR2,...)
  cl <- function(x) paste(x, collapse = ",")

  # content of config file
  content <- c(
    paste("name_survey =", name_survey),
    "",
    paste("vars_discretes =", cl(vars_discretes)),
    paste("vars_continuous =", cl(vars_continuous)),
    paste("vars_ignore =", cl(vars_ignore)),
    "",
    paste("prefix_discretes =", cl(prefix_discretes)),
    paste("prefix_continuous =", cl(prefix_continuous)),
    paste("prefix_ignore =", cl(prefix_ignore)),
    "",
    paste("var_wave =", cl(var_wave)),
    paste("var_filter =", cl(var_filter)),
    paste("var_intvwr =", var_intvwr),
    paste("var_intv =", var_intv),
    "",
    paste("var_date =", var_date),
    paste("var_info_geo =", var_info_geo),
    paste("var_timer =", var_timer),
    paste("var_itm_duration =", var_itm_duration),
    paste("var_session =", var_session),
    paste("duration_min_during =", duration_min_during),
    paste("duration_min_inter  =", duration_min_inter),
    paste("night_start         =", night_start),
    paste("night_end           =", night_end)
  )

  writeLines(content, file_path)

  cli::cli_alert_success("File {file_path} created.")
}

#' load a config file for prepare data
#'
#' @param file_path path of the configuration file
#'
#' @returns df
load_config <- function(file_path) {
  lines <- readLines(file_path)

  # filter comments lines
  lines <- trimws(lines)
  lines <- lines[lines != "" & !startsWith(lines, "#")]

  # separe pair key=value
  lines %>% map_df(~ {
    parts <- strsplit(.x, "=")[[1]]
    key <- trimws(parts[1])
    value <- trimws(parts[2])
    value <- gsub('^"|"$', "", value)

    value <- strsplit(value, ",")[[1]]
    value <- trimws(value)

    return(tibble(key = key, value = list(value)))
  })
}

#' corrections of each df of a list
#'
#' @param list_df list of df
#'
#' @returns list
correct_list_df <- function(list_df) {
  list_DB_FORMAT <- lapply(list_df, function(df) {
    tibble(variable = names(df), format = sapply(
      df, function(x) class(x)[[1]]))
  })

  variables_correction <- dplyr::bind_rows(list_DB_FORMAT, .id = "df") %>%
    group_by(variable) %>%
    mutate(fus_format = paste(format, collapse = "-")) %>%
    mutate(check = n_distinct(format) == 1) %>%
    filter(
      !check, fus_format != "integer-logical",
      fus_format != "logical-integer"
    ) %>%
    pull(variable) %>%
    unique()

  lapply(list_df, function(df) {
    df %>% mutate(across(any_of(variables_correction), as.character))
  })
}

#' extract a config from key (config from load_config)
#'
#' @param config df of configuration
#' @param key_ key to extract
#'
#' @returns string
extract_config <- function(config, key_) {
  config %>%
    filter(key == key_) %>%
    pull(value) %>%
    unlist() %>%
    toupper()
}

#' tranform data from folder to config and df
#'
#' @param folder folder of databases
#' @param file_config name of the configuration file (config.txt by default)
#' @param file_pattern pattern of the databases (*.csv by default)
#'
#' @returns list(df,configs)
#' @export
#'
#' @examples
#' \dontrun{
#' folder_to_df("ESS10")
#' }
folder_to_df <- function(folder,
                         file_pattern = "*.csv",
                         file_config = "config.txt") {
  path_df <- list.files(folder, full.names = TRUE, pattern = file_pattern)
  list_df <- lapply(path_df, function(link) {
    df <- tidyr::as_tibble(data.table::fread(link, encoding = "UTF-8"))
    colnames(df) <- toupper(colnames(df))
    return(df)
  })

  if (length(list_df) == 0) {
    return(NULL)
  }

  # bind rows all the df
  df <- list_df %>%
    correct_list_df() %>%
    dplyr::bind_rows() %>%
    mutate_if(is.character, function(col) iconv(col, to = "UTF-8"))

  # load config file
  link_config <- file.path(folder, file_config)
  config <- NULL
  config <- load_config(link_config)

  # extract info from config.txt
  configs <- list()
  configs$name_survey      <- config %>% extract_config("name_survey")
  configs$vars_discretes   <- intersect(config %>% extract_config("vars_discretes"), names(df))
  configs$vars_continuous  <- intersect(config %>% extract_config("vars_continuous"), names(df))
  configs$vars_ignore  <- intersect(config %>% extract_config("vars_ignore"), names(df))
  configs$prefix_discretes <- config %>% extract_config("prefix_discretes")
  configs$prefix_continuous <- config %>% extract_config("prefix_continuous")
  configs$prefix_ignore <- config %>% extract_config("prefix_ignore")
  configs$path       <- folder
  configs$var_wave   <- intersect(config %>% extract_config("var_wave"), names(df))
  configs$var_filter <- intersect(config %>% extract_config("var_filter"), names(df))
  configs$var_intvwr <- intersect(config %>% extract_config("var_intvwr"), names(df))
  configs$var_intv   <- intersect(config %>% extract_config("var_intv"), names(df))

  configs$var_date <- config %>% extract_config("var_date")
  configs$var_info_geo <- config %>% extract_config("var_info_geo")
  
  configs$var_timer           <- config %>% extract_config("var_timer")
  configs$var_itm_duration    <- config %>% extract_config("var_itm_duration")
  configs$var_session         <- config %>% extract_config("var_session")
  configs$duration_min_during <- config %>% extract_config("duration_min_during")
  configs$duration_min_inter  <- config %>% extract_config("duration_min_inter")
  configs$night_start         <- config %>% extract_config("night_start")
  configs$night_end           <- config %>% extract_config("night_end")
  
  out <- classify_df_pattern(df,configs)
  prepa_discretes  <- out$vars_vd
  prepa_continuous <- out$vars_vc
  
  # use manual classification instead avec automatic
  # config_all <- config %>%
  #   pull(value) %>%
  #   unique() %>%
  #   unlist()
  # prepa_discretes <- prepa_discretes[!prepa_discretes %in% config_all]
  # prepa_continuous <- prepa_continuous[!prepa_continuous %in% config_all]

  # add variables in config obj
  configs$vars_discretes <- sort(unique(c(configs$vars_discretes, prepa_discretes)))
  configs$vars_continuous <- sort(unique(c(configs$vars_continuous, prepa_continuous)))

  # minimal correction of the file
  df <- df %>%
    mutate(
      across(any_of(configs$vars_continuous), as.numeric),
      across(any_of(configs$vars_discretes), as.factor)
    )

  if (!is.null(configs$var_wave)){
    df <- df %>% mutate(across(any_of(configs$var_wave), as.character))
  }

  if (!is.null(configs$var_filter)){
    df <- df %>% mutate(across(any_of(configs$var_filter), as.character))
  }

  # Multi-level wave / filter : the variables of each level are kept in
  # vars_wave / vars_filter, and a variable of the complete key is created.
  # var_wave / var_filter always designate the variable used for the calculs.
  configs$vars_wave   <- configs$var_wave
  configs$vars_filter <- configs$var_filter

  if (length(configs$vars_wave) > 1) {
    df$wave <- combine_vars(df, configs$vars_wave)
    configs$var_wave <- "wave"
  }

  if (length(configs$vars_filter) > 1) {
    df$filter <- combine_vars(df, configs$vars_filter)
    configs$var_filter <- "filter"
  }

  return(list(df = df, configs = configs))
}


#' Create statistics from database
#'
#' @param df_ database
#' @param configs configs
#' @param var_calculs variable to create stats
#' @param mod_filter (optional) modality to filter data
#' @param na.rm include or not missing values as modality
#'
#' @returns df data.frame
create_df_stats <- function(df_, configs,
                            var_calculs,
                            mod_filter = NULL,
                            na.rm = FALSE) {
  df <- df_

  if (!is.null(mod_filter)) {
    df <- df[match_keys(df, vars_levels(configs, "filter"),
                        configs$var_filter, mod_filter), ]
  }

  variables <- list()
  variables$variables_vd <- configs$vd
  variables$variables_vc <- configs$vc

  df_stats <- df %>% prepa_stats_dt(var_calculs, configs, na.rm)

  if (!is.null(mod_filter)) {
    df_stats <- df_stats %>% mutate(filter = mod_filter)
  } else {
    df_stats <- df_stats %>% mutate(filter = "All")
  }
  df_stats
}

#' Loop of stats creation by filter
#'
#' @param df database
#' @param configs configs
#' @param var_calculs variable to create stats
#'
#' @returns df
loop_stats <- function(df, configs, var_calculs, na.rm = FALSE) {
  cli::cli_progress_step("create_df_stats for {var_calculs}", spinner = TRUE)
  df_stats <- create_df_stats(df, configs, var_calculs, na.rm = na.rm)

  # keys of the filter : each level when the filter has two levels
  vec_filter <- keys_vars(df, vars_levels(configs, "filter"), configs$var_filter)

  if (length(vec_filter) > 1) {

    cli::cli_alert_info("create_df_stats for {configs$var_filter}")
    df_stats_filter <- vec_filter %>% map_df(~ {
      cli::cli_progress_step("{configs$var_filter} = {.x}", spinner = TRUE)
      create_df_stats(df, configs, var_calculs, mod_filter = .x, na.rm = na.rm)
    })

    df_stats <- df_stats %>%
      dplyr::add_row(df_stats_filter)
  }
  return(df_stats)
}

#' Preparation of a survey
#'
#' @param folder_path folder of survey
#' @param file_config name of the configuration file (config.txt by default)
#' @param file_pattern pattern of the databases (*.csv by default)
#'
#' @returns NULL (creation of rds)
#' @export
#'
#' @examples
#' \dontrun{
#' prepa_survey("shiny-examples/complete/ESS10")
#' }
prepa_survey <- function(folder_path,
                         file_pattern = "*.csv",
                         file_config = "config.txt",
                         na.rm = FALSE) {
  cli::cli_h3("prepa_survey for path {folder_path}")
  list_df <- folder_to_df(folder_path, file_pattern, file_config)
  if (is.null(list_df)) {
    cli::cli_alert_danger("no df in path {folder_path}")
    return(NULL)
  }
  if (na.rm){
    cli::cli_alert_info("missing values removed before chi² calculation")
  }else{
    cli::cli_alert_info("missing values added as a modality during chi² calculation")
  }

  configs <- list_df$configs

  df <- list_df$df

  # Create a fake all wave or filter or group if null
  if (length(configs$var_wave) == 0 || all(is.na(configs$var_wave))){
    df <- df %>% mutate(wave = "All")
    configs$var_wave <- "wave"
  }

  if (length(configs$var_filter) == 0 || all(is.na(configs$var_filter))){
    df <- df %>% mutate(filter = "All")
    configs$var_filter <- "filter"
  }

  if (length(configs$var_intvwr) == 0 || all(is.na(configs$var_intvwr))){
    df <- df %>% mutate(intvwr = "All")
    configs$var_intvwr <- "intvwr"
  }

  # levels of the wave and of the filter (see folder_to_df)
  if (length(configs$vars_wave) == 0)   configs$vars_wave   <- configs$var_wave
  if (length(configs$vars_filter) == 0) configs$vars_filter <- configs$var_filter

  # Wave variation
  cli::cli_h3("Wave Variation")
  df_stats_wave <- loop_stats(df, configs, configs$var_wave, na.rm = na.rm)

  # Second level of wave : the stats are also computed for the first level
  # alone, so that the interface can work with or without the second level
  if (length(configs$vars_wave) > 1) {
    cli::cli_h3("Wave Variation for {configs$vars_wave[1]}")
    df_lvl1 <- df
    df_lvl1[[configs$var_wave]] <- key_level1(df_lvl1[[configs$var_wave]])
    df_stats_wave <- df_stats_wave %>%
      dplyr::add_row(loop_stats(df_lvl1, configs, configs$var_wave, na.rm = na.rm))
  }

  # keys of the wave : each level when the wave has two levels
  vec_wave <- keys_vars(df, configs$vars_wave, configs$var_wave)

  # Interviewer variation
  cli::cli_h3("Interviewer Variation")
  if (length(pull(unique(df[, configs$var_intvwr]))) > 1) {
    df_stats_intvwr <- vec_wave %>% map_df(~ {
      cli::cli_progress_step("df_stats_intvwr for wave {.x}",spinner = TRUE)
      sub_df <- df[match_keys(df, configs$vars_wave, configs$var_wave, .x), ] %>%
        loop_stats(configs, configs$var_intvwr, na.rm = na.rm) %>%
        mutate(!!sym(configs$var_wave) := .x)
    })
  } else {
    df_stats_intvwr <- NULL
  }

  global <- list(
    configs = configs,
    df = df,
    df_stats_wave = df_stats_wave,
    df_stats_intvwr = df_stats_intvwr
  )

  readr::write_rds(global, file.path(folder_path,"global.rds"),compress = "gz")

  cli::cli_alert_success("File global.rds created.")
  cli::cli_alert("File in directory {folder_path}")

  invisible(TRUE)
}

#' Preparation of all surveys from a folder
#'
#' @param folder_path folder of the folders of survey
#' @param depth_folder level of depth for the tree structure
#' @param file_config name of the configuration file (config.txt by default)
#' @param file_pattern pattern of the databases (*.csv by default)
#'
#' @returns NULL (creation of rds)
#' @export
#'
#' @examples
#' \dontrun{
#' prepa_surveys("inst/extdata/SILC/HFILE")
#' }
prepa_surveys <- function(folder_path,
                          depth_folder=1,
                          file_pattern = "*.csv",
                          file_config = "config.txt") {

  list_dirs <- folder_path

  if (depth_folder >= 2){
    list_dirs <- list.dirs(list_dirs, full.names = TRUE, recursive = FALSE)
  }

  if (depth_folder >= 3){
    list_dirs <- list_dirs %>%
      map(~ {
        list.dirs(.x, full.names = TRUE, recursive = FALSE)
      }) %>%
      unlist()
  }

  list_dirs %>% map(~{prepa_survey(.x,file_pattern, file_config)})
}
