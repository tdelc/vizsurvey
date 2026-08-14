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
