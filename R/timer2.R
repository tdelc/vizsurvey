# -----------------------------
# LECTURE
# -----------------------------

read_timer_config <- function(link_config) {
  
  config <- load_config(link_config)
  
  by_default_num <- function(value,default){
    if (length(value) > 0) as.numeric(value)
    else default
  }
  
  cfg <- list(
    duration_min_during = config %>% extract_config("duration_min_during") %>% by_default_num(15),
    duration_min_inter  = config %>% extract_config("duration_min_inter") %>% by_default_num(15),
    night_start  = config %>% extract_config("night_start") %>% by_default_num(21),
    night_end    = config %>% extract_config("night_end") %>% by_default_num(6),
    var_intvwr   = config %>% extract_config("var_intvwr"),
    var_intv     = config %>% extract_config("var_intv"),
    var_wave     = config %>% extract_config("var_wave"),
    var_timer    = config %>% extract_config("var_timer"),
    var_session  = config %>% extract_config("var_session"),
    var_itm_duration  = config %>% extract_config("var_itm_duration")
  )
  
  # Multi-level wave : the complete key is created in prepa_timer_from_df
  cfg[["vars_wave"]] = cfg$var_wave
  if (length(cfg$var_wave) > 1) cfg[["var_wave"]] = "wave"

  cfg[["night_vec"]] = c(cfg$night_start:23,0:cfg$night_end)
  cfg[["var_ids"]]  = c(cfg$var_wave,cfg$var_intvwr,cfg$var_intv)
  
  return(cfg)
}

#' Read a timer file in csv
#'
#' @param file_timer path of file
#'
#' @returns data.frame
#' @export
read_timer_csv <- function(file_timer) {
  df <- as_tibble(fread(file_timer,encoding="UTF-8"))
  colnames(df) <- toupper(colnames(df))
  return(df)
}

#' Read and rbind many timer files in csv
#'
#' @param files_timer path of files
#'
#' @returns data.frame
#' @export
read_timer_csvs <- function(files_timer) {
  dfs   <- lapply(files_timer, read_timer_csv)
  dfs   <- dfs[sapply(dfs, nrow) > 0]
  if (length(dfs) == 0) return(tibble())
  bind_rows(dfs)
}

#' Extract timer rds files from folder
#'
#' @param folder folder where check the file
#'
#' @returns path of files
#' @export
find_timer_rds <- function(folder) {
  files <- list.files(folder, full.names = TRUE, pattern = "timers.rds$")
  if (length(files) == 0) return(NULL)
  if (length(files) > 1) files <- files[1]
  return(files)
}

#' Compute timer of group
#'
#' @param df_timer df_timer 
#' @param cfg config list 
#'
#' @returns data.frame
#' @export
compute_timer_groups <- function(df_timer, cfg) {
  df_timer %>%
    group_by(!!!syms(cfg$var_ids)) %>%
    mutate(
      NB_SSN      = n_distinct(!!sym(cfg$var_session)),
      MIN_TM_INTV = min(!!sym(cfg$var_timer), na.rm = TRUE),
      MAX_TM_INTV = max(!!sym(cfg$var_timer), na.rm = TRUE)
    ) %>%
    group_by(!!!syms(cfg$var_ids), !!sym(cfg$var_session)) %>%
    mutate(
      MIN_TM_SSN = min(!!sym(cfg$var_timer), na.rm = TRUE),
      MAX_TM_SSN = max(!!sym(cfg$var_timer), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(FL_NIGHT = hour(!!sym(cfg$var_timer)) %in% cfg$night_vec)
}

# -----------------------------
# Synthesis
# -----------------------------

#' Build the df_timer for days
#'
#' @param df_timer df_timer 
#' @param cfg config list 
#'
#' @returns data.frame
build_df_timer_day <- function(df_timer, cfg) {
  df_timer %>%
    mutate(DT_INTV = as.Date(MAX_TM_INTV)) %>%
    distinct(!!!syms(cfg$var_ids), DT_INTV) %>%
    group_by(!!sym(cfg$var_wave), !!sym(cfg$var_intvwr), DT_INTV) %>%
    mutate(NB_INTV_DAY = n()) %>%
    group_by(!!sym(cfg$var_wave), !!sym(cfg$var_intvwr)) %>%
    mutate(NB_INTV = n()) %>%
    group_by(!!sym(cfg$var_wave), !!sym(cfg$var_intvwr), NB_INTV) %>%
    summarise(MAX_NB_INTV_DAY = max(NB_INTV_DAY), .groups = "drop")
}

#' Build the df_timer of sections
#'
#' @param df_timer df_timer 
#' @param cfg config list 
#'
#' @returns data.frame
build_df_timer_sctn <- function(df_timer, cfg) {
  df_timer %>%
    group_by(!!!syms(cfg$var_ids), !!sym(cfg$var_session)) %>%
    count(TX_QSTNR_SCTN, name = "N") %>%
    mutate(flag = "\U0001f7e2") %>%
    group_by(!!!syms(cfg$var_ids)) %>%
    complete(!!sym(cfg$var_session), TX_QSTNR_SCTN) %>%
    replace_na(list(flag = "\U0001f534")) %>%
    arrange(!!!syms(cfg$var_ids), !!sym(cfg$var_session), TX_QSTNR_SCTN) %>%
    group_by(!!!syms(cfg$var_ids), !!sym(cfg$var_session)) %>%
    summarise(CHECK_SCTN = paste0(flag, collapse = ""), .groups = "drop")
}

#' Build the df_timer of sessions
#'
#' @param df_timer df_timer 
#' @param cfg config list 
#' @param df_timer_sctn df_timer_sctn
#'
#' @returns data.frame
build_df_timer_ssn <- function(df_timer, df_timer_sctn, cfg) {
  df_timer %>%
    group_by(
      !!!syms(cfg$var_ids), !!sym(cfg$var_session), NB_SSN, MIN_TM_SSN, MAX_TM_SSN
    ) %>%
    summarise(
      FL_NIGHT  = max(FL_NIGHT, na.rm = TRUE) == 1,
      # NB_MB     = n_distinct(NR_MB, na.rm = TRUE),
      DURATION_SSN = sum(!!sym(cfg$var_itm_duration), na.rm = TRUE) / 60,
      .groups   = "drop"
    ) %>%
    arrange(!!!syms(cfg$var_ids), MIN_TM_SSN) %>%
    group_by(!!!syms(cfg$var_ids)) %>%
    mutate(
      DURATION_INTER_SSN = as.numeric(
        difftime(MIN_TM_SSN, lag(MAX_TM_SSN), units = "mins")
      )
    ) %>%
    ungroup() %>%
    left_join(
      df_timer_sctn,
      by = c(cfg$var_ids, cfg$var_session)
    )
}

#' Build the df_timer of interview
#'
#' @param cfg config list 
#' @param df_timer_ssn df_timer_ssn
#' @param df_timer_day df_timer_day 
#'
#' @returns data.frame
#' @export
build_df_timer_intv <- function(df_timer_ssn, df_timer_day, cfg) {
  df_timer_intv <- df_timer_ssn %>%
    group_by(!!!syms(cfg$var_ids), NB_SSN) %>%
    summarise(
      MIN_TM_INTV   = min(MIN_TM_SSN, na.rm = TRUE),
      MAX_TM_INTV   = max(MAX_TM_SSN, na.rm = TRUE),
      DURATION_INTV = sum(DURATION_SSN, na.rm = TRUE),
      FL_NIGHT      = max(FL_NIGHT, na.rm = TRUE),
      .groups       = "drop"
    ) %>%
    arrange(!!sym(cfg$var_wave), !!sym(cfg$var_intvwr), MIN_TM_INTV) %>%
    group_by(!!sym(cfg$var_wave), !!sym(cfg$var_intvwr)) %>%
    mutate(
      DURATION_INTER_INTV = as.numeric(
        difftime(MIN_TM_INTV, lag(MAX_TM_INTV), units = "mins")
      ),
      FL_DURATION       = DURATION_INTV < cfg$duration_min_during,
      FL_DURATION_INTER = DURATION_INTER_INTV < cfg$duration_min_inter & 
        DURATION_INTER_INTV > 0,
      FL_INTER          = DURATION_INTER_INTV < 0,
      FL_NB_SSN         = NB_SSN > 1,
      FL_NIGHT          = FL_NIGHT == 1
    ) %>%
    ungroup() %>%
    relocate(FL_NIGHT, .after = FL_NB_SSN)
  
  df_timer_intv %>%
    left_join(df_timer_day, by = c(cfg$var_wave, cfg$var_intvwr))
}

# -----------------------------
# Synthesis by interviewer
# -----------------------------

#' Build the df_timer of interviewer
#'
#' @param df_timer_intv df_timer_intv
#' @param cfg config list 
#'
#' @returns data.frame
#' @export
build_df_timer_intvwr <- function(df_timer_intv, cfg) {
  df_timer_intv %>%
    group_by(!!sym(cfg$var_wave), !!sym(cfg$var_intvwr)) %>%
    summarise(
      NB_INTV           = n(),
      DURATION_MEDIAN   = median(DURATION_INTV,   na.rm = TRUE),
      DURATION_MIN      = min(DURATION_INTV,      na.rm = TRUE),
      PC_DURATION       = mean(FL_DURATION,       na.rm = TRUE),
      PC_NIGHT          = mean(FL_NIGHT,          na.rm = TRUE),
      PC_DURATION_INTER = mean(FL_DURATION_INTER, na.rm = TRUE),
      PC_INTER          = mean(FL_INTER,          na.rm = TRUE),
      PC_NB_SSN         = mean(FL_NB_SSN,         na.rm = TRUE),
      MAX_INTV_DAY      = max(MAX_NB_INTV_DAY,    na.rm = TRUE),
      .groups           = "drop"
    )
}

# -----------------------------
# Main function
# -----------------------------

#' Prepation of timers from raw
#'
#' @param cfg config list 
#' @param df_timer_raw df_timer_raw
#'
#' @returns data.frame
#' @export
prepa_timer_from_df <- function(df_timer_raw, cfg) {

  # Multi-level wave : creation of the variable of the complete key
  if (length(cfg$vars_wave) > 1) {
    df_timer_raw[[cfg$var_wave]] <- combine_vars(df_timer_raw, cfg$vars_wave)
  }

  df_timer <- df_timer_raw %>%
    correct_df_timer_session() %>%
    compute_timer_groups(cfg)
  
  df_timer_day    <- build_df_timer_day(df_timer, cfg)
  df_timer_sctn   <- build_df_timer_sctn(df_timer, cfg)
  df_timer_ssn    <- build_df_timer_ssn(df_timer, df_timer_sctn, cfg)
  df_timer_intv   <- build_df_timer_intv(df_timer_ssn, df_timer_day, cfg)
  df_timer_intvwr <- build_df_timer_intvwr(df_timer_intv, cfg)
  
  list(
    cfg             = cfg,
    df_timer_day    = df_timer_day,
    df_timer_sctn   = df_timer_sctn,
    df_timer_ssn    = df_timer_ssn,
    df_timer_intv   = df_timer_intv,
    df_timer_intvwr = df_timer_intvwr
  )
}
