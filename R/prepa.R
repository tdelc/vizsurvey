#' Classify all variable of a data.frame
#'
#' @param df A data frame
#'
#' @returns a data frame
#' @export
#'
#' @examples
#' classify_variables(iris)
classify_variables <- function(df) {
  df %>%
    summarise(across(everything(), ~ {
      if (n_distinct(.) == 1) {
        return("Solo")
      } else if (n_distinct(.) < 15) {
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

#' Create a summarise of all the difference
#'
#' @param db data frame for the summary
#' @param vars_vd Vector of discrete variables
#' @param vars_vc Vector of continuous variables
#' @param var_group Name of group variable
#'
#' @returns data frame
#' @export
#'
#' @examples
#' library(laeken)
#' data(eusilc)
#'
#' info_vars <- classify_variables(eusilc)
#' vars_vd <- info_vars[info_vars$type == "Modal", ]$variable
#' vars_vc <- info_vars[info_vars$type == "Continuous", ]$variable
#' prepa_stats(eusilc, vars_vd, vars_vc, "db040")
prepa_stats <- function(db, vars_vd, vars_vc, var_group) {
  if (length(var_group) == 0) {
    return(tibble(NULL))
  }

  vars_vd <- setdiff(vars_vd, var_group)
  vars_vc <- setdiff(vars_vc, var_group)

  vars_vd <- intersect(vars_vd, names(db))
  vars_vc <- intersect(vars_vc, names(db))

  db <- db %>%
    mutate(
      across(any_of(vars_vc), as.numeric),
      across(any_of(vars_vd), as.factor),
      across(any_of(vars_vd), as.numeric),
      across(any_of(var_group), as.character)
    )

  # Calcul de la distribution globale pour chaque variable catégorielle
  dist_list <- lapply(vars_vd, function(varname) {
    expected_prop <- prop.table(table(db[[varname]], useNA = "ifany"))
    names(expected_prop)[which(is.na(names(expected_prop)))] <- "NA_"
    expected_prop <- tibble(category = names(expected_prop), prop = expected_prop) %>%
      mutate(category = ifelse(prop < 0.01, "OTH_", category)) %>%
      group_by(category) %>%
      summarise(prop = sum(prop)) %>%
      ungroup()
    setNames(expected_prop$prop, expected_prop$category)
  })
  names(dist_list) <- vars_vd

  my_chisq_test <- function(x, varname, ldist = dist_list) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    observed_counts <- table(x, useNA = "ifany")
    expected_prop <- ldist[[varname]]

    names(observed_counts)[which(is.na(names(observed_counts)))] <- "NA_"
    rare_categories <- setdiff(names(observed_counts), names(expected_prop))
    observed_counts["OTH_"] <- sum(observed_counts[rare_categories], na.rm = TRUE)
    observed_counts <- observed_counts[!names(observed_counts) %in% rare_categories]
    observed_counts <- observed_counts[which(observed_counts > 0)]

    all_levels <- union(names(observed_counts), names(expected_prop))
    obs <- observed_counts[all_levels]
    names(obs) <- all_levels
    obs[is.na(obs)] <- 0
    exp_prop <- expected_prop[all_levels]
    exp_prop[is.na(exp_prop)] <- 0

    out <- tryCatch(
      {
        t_chi <- stats::chisq.test(obs, p = exp_prop, simulate.p.value = TRUE, B = 1)
        t_chi$statistic
      },
      error = function(e) {
        NA_real_
      }
    )
    out
  }

  scale_borne <- function(x) {
    IIQ <- quantile(x, probs = 0.75, na.rm = TRUE) - quantile(x, probs = 0.25, na.rm = TRUE)
    if (is.na(IIQ) | IIQ == 0) {
      scale(x)[, 1]
    } else {
      if (!all(x < 1, na.rm = TRUE)) IIQ <- max(IIQ, 1)
      (x - median(x, na.rm = TRUE)) / IIQ
    }
  }

  db_stat <- db %>%
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
          chi2 = ~ my_chisq_test(.x, cur_column())
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
      standard = scale_borne(value),
      standard = case_when(
        sd(value, na.rm = TRUE) == 0 ~ 0,
        is.nan(standard) ~ NA,
        TRUE ~ standard
      )
    ) %>%
    dplyr::relocate(!!sym(var_group), variable, Nrow, Nval) %>%
    ungroup()
  return(db_stat)
}

#' Create a template of configuration file
#'
#' @param folder_path folder where create the file
#' @param file_name Name of the config file (config.txt by default)
#' @param name_survey Name of the survey (not used)
#' @param var_itw variable name of interviewer
#' @param var_domain (optional) variable name of domain
#' @param var_group (optional) variable name of group
#' @param vars_discretes (optional) preset discretes variables name (VAR1,VAR2,...)
#' @param vars_continous (optional) preset continous variables name (VAR1,VAR2,...)
#'
#' @returns NULL
#' @export
#'
#' @examples
#' create_config(".") # creation of config.txt in working directory
create_config <- function(folder_path, file_name = "config.txt",
                          name_survey = NULL,
                          vars_discretes = NULL,
                          vars_continous = NULL,
                          var_domain = NULL,
                          var_group = NULL,
                          var_itw = NULL) {
  if (!dir.exists(folder_path)) {
    stop("folder_path does not exists")
  }

  # complete path of config file
  file_path <- file.path(folder_path, file_name)

  # content of config file
  content <- c(
    paste("name_survey =", name_survey),
    "",
    paste("vars_discretes =", vars_discretes),
    paste("vars_continous =", vars_continous),
    "",
    paste("var_domain =", var_domain),
    paste("var_group =", var_group),
    "",
    paste("var_itw =", var_itw)
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
    tibble(variable = names(df), format = sapply(df, class))
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
  configs$vd <- intersect(config %>% extract_config("vars_discretes"), names(df))
  configs$vc <- intersect(config %>% extract_config("vars_continous"), names(df))
  configs$id_itw <- intersect(config %>% extract_config("var_itw"), names(df))
  configs$path <- folder
  configs$vt <- config %>% extract_config("var_domain")
  configs$vg <- config %>% extract_config("var_group")
  configs$enq <- config %>% extract_config("name_survey")

  # add automatic classification
  df_variables <- classify_variables(df)
  prepa_vd <- df_variables %>%
    filter(type == "Modal") %>%
    pull(variable)
  prepa_vc <- df_variables %>%
    filter(type == "Continuous") %>%
    pull(variable)

  # use manual classification instead avec automatic
  config_all <- config %>%
    pull(value) %>%
    unique() %>%
    unlist()
  prepa_vd <- prepa_vd[!prepa_vd %in% config_all]
  prepa_vc <- prepa_vc[!prepa_vc %in% config_all]

  # add variables in config obj
  configs$vd <- sort(unique(c(configs$vd, prepa_vd)))
  configs$vc <- sort(unique(c(configs$vc, prepa_vc)))

  # minimal correction of the file
  df <- df %>%
    mutate(
      across(any_of(configs$vc), as.numeric),
      across(any_of(configs$vd), as.factor)
    )

  if (!is.na(configs$vt)){
    df <- df <- mutate(across(any_of(configs$vt), as.character))
  }

  if (!is.na(configs$vg)){
    df <- df <- mutate(across(any_of(configs$vg), as.character))
  }

  return(list(df = df, configs = configs))
}


#' Create statistics from database
#'
#' @param df_ database
#' @param configs configs
#' @param var_calculs variable to create stats
#' @param filter_group (optional) group modality to filter data
#'
#' @returns df
create_df_stats <- function(df_, configs,
                            var_calculs,
                            filter_group = NULL) {
  df <- df_

  if (!is.null(filter_group)) {
    df <- df %>% filter(!!sym(configs$vg) %in% filter_group)
  }

  variables <- list()
  variables$variables_vd <- configs$vd
  variables$variables_vc <- configs$vc

  df_stats <- df %>% prepa_stats(configs$vd, configs$vc, var_calculs)

  if (!is.null(filter_group)) {
    df_stats <- df_stats %>% mutate(group = filter_group)
  } else {
    df_stats <- df_stats %>% mutate(group = "All")
  }
  df_stats
}

#' Loop of stats creation by group
#'
#' @param df database
#' @param configs configs
#' @param var_calculs variable to create stats
#'
#' @returns df
loop_stats <- function(df, configs, var_calculs) {
  cli::cli_progress_step("df_stats for {var_calculs}", spinner = TRUE)
  df_stats <- create_df_stats(df, configs, var_calculs)

  if (length(pull(unique(df[, configs$vg]))) > 1) {
    vec_group <- pull(unique(df[, configs$vg]))

    cli::cli_alert_info("create_df_stats for group {configs$vg}")
    df_stats_group <- vec_group %>% map_df(~ {
      cli::cli_progress_step("{configs$vg} = {.x}", spinner = TRUE)
      create_df_stats(df, configs, var_calculs, filter_group = .x)
    })

    df_stats <- df_stats %>%
      dplyr::add_row(df_stats_group)
  }
  return(df_stats)
}

#' Preparation of a survey
#'
#' @param folder folder of survey
#' @param ... argument to pass to folder_to_df (example : file_pattern)
#'
#' @returns NULL (creation of rds)
#' @export
#'
#' @examples
#' \dontrun{
#' prepa_survey("shiny-examples/complete/ESS10")
#' }
prepa_survey <- function(folder, ...) {
  list_df <- folder_to_df(folder,...)
  if (is.null(list_df)) {
    return(NULL)
  }

  configs <- list_df$configs

  df <- list_df$df

  # Create a fake all domain or group if null
  if (is.na(configs$vt)){
    df <- df %>% mutate(domain = "All")
    configs$vt <- "domain"
  }

  if (is.na(configs$vg)){
    df <- df %>% mutate(group = "All")
    configs$vg <- "group"
  }

  # Domain variation
  if (length(pull(unique(df[, configs$vt]))) > 1) {
    df_stats <- loop_stats(df, configs, configs$vt)
  } else {
    df_stats <- NULL
  }

  # Interviewer variation
  df_stats_itw <- pull(unique(df[, configs$vt])) %>% map_df(~ {
    cli::cli_progress_step("df_stats_itw for domain {.x}",spinner = TRUE)
    sub_df <- df %>%
      filter(!!sym(configs$vt) == .x) %>%
      loop_stats(configs, configs$id_itw) %>%
      mutate(!!sym(configs$vt) := .x)
  })

  readr::write_rds(configs, file.path(folder,"list_config.rds"), compress = "gz")
  readr::write_rds(df, file.path(folder,"df.rds"), compress = "gz")
  readr::write_rds(df_stats, file.path(folder,"df_stats.rds"), compress = "gz")
  readr::write_rds(df_stats_itw, file.path(folder,"df_stats_itw.rds"), compress = "gz")

  cli::cli_alert_success("File list_config.rds created.")
  cli::cli_alert_success("File df.rds created.")
  cli::cli_alert_success("File df_stats.rds created.")
  cli::cli_alert_success("File df_stats_itw.rds created.")
  cli::cli_alert("Files in directory {folder}")
}

#' Preparation of all surveys from a folder
#'
#' @param folder folder of the folders of survey
#'
#' @returns NULL (creation of rds)
#' @export
#'
#' @examples
#' \dontrun{
#' prepa_all_surveys("shiny-examples/complete/data")
#' }
prepa_all_surveys <- function(folder) {
  list_dirs <- list.dirs(folder, full.names = TRUE, recursive = FALSE)

  list_dirs <- list_dirs %>%
    map(~ {
      list.dirs(.x, full.names = TRUE, recursive = FALSE)
    }) %>%
    unlist()

  list_dirs %>% map(prepa_survey)
}
