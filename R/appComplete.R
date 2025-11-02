#' Shiny vizsurvey with already prepared data
#'
#' @param link link to directory of data
#' @param is_double_folder is the directory contains subdirectories of survey
#' @param data_rds_pattern name of the rds file contains all the data
#'
#' @returns shinyapp
#' @export
#'
#' @examples
#' # We assume that config.txt, and prepa_surveys are already done here.
#' \dontrun{runVizsurvey_from_folder("inst/extdata",is_double_folder = T)}
runVizsurvey_from_folder <- function(
    link,
    data_rds_pattern = "global",
    is_double_folder = T
) {

  appDir <- system.file("shiny-examples", "complete", package = "vizsurvey")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `vizsurvey`.", call. = FALSE)
  }

  shiny::shinyOptions(link_data_folder = normalizePath(link))
  shiny::shinyOptions(data_rds_pattern = data_rds_pattern)
  shiny::shinyOptions(is_double_folder = is_double_folder)
  shiny::runApp(appDir, display.mode = "normal")

  invisible(TRUE)
}

#' Shiny vizsurvey from a R data.frame
#'
#' @param df data.frame
#' @param var_itw name of interviewer variable
#' @param vars_discretes (optional) preset of discretes variables
#' @param vars_continous (optional) preset of continous variables
#' @param var_domain (optional) name of domain variable
#' @param var_group (optional) name of group variable
#'
#' @returns shinyapp
#' @export
#'
#' @examples
#' library(laeken)
#' data(eusilc)
#' \dontrun{runVizsurvey_from_r(eusilc,var_itw = "db040")}
runVizsurvey_from_r <- function(
    df,
    var_itw,
    vars_discretes = NULL,
    vars_continous = NULL,
    var_domain     = NULL,
    var_group      = NULL
) {

  if (is.null(df)) {
    stop("df not present.", call. = FALSE)
  }
  if (missing(var_itw) || is.null(var_itw) || !nzchar(as.character(var_itw))) {
    stop("`var_itw` is mandatory (name of interviewer variable).", call. = FALSE)
  }

  appDir <- system.file("shiny-examples", "complete", package = "vizsurvey")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `vizsurvey`.", call. = FALSE)
  }

  # Survey preparation
  temporary_dir <- tempdir()
  unlink(file.path(temporary_dir, "DATA"),recursive = T)
  dir.create(file.path(temporary_dir, "DATA"))
  link_folder <- file.path(temporary_dir, "DATA")
  dir.create(file.path(temporary_dir, "DATA","DATA"))
  link_data_folder <- file.path(temporary_dir, "DATA","DATA")
  readr::write_csv(df,file=file.path(link_data_folder,"data_from_r.csv"),
                   col_names = T)

  create_config(
    folder_path    = link_data_folder,
    file_name      = "config.txt",
    name_survey    = NULL,
    vars_discretes = vars_discretes,
    vars_continous = vars_continous,
    var_domain     = var_domain,
    var_group      = var_group,
    var_itw        = var_itw
  )

  prepa_survey(link_data_folder,"global")

  shiny::shinyOptions(link_data_folder = link_folder)
  shiny::shinyOptions(data_rds_pattern = "global")
  shiny::shinyOptions(is_double_folder = F)
  shiny::runApp(appDir, display.mode = "normal")

  invisible(TRUE)
}

#' Shiny vizsurvey from a csv/tsv
#'
#' @param path path of a data.frame (can be readed by fread)
#' @param var_itw name of interviewer variable
#' @param vars_discretes (optional) preset of discretes variables
#' @param vars_continous (optional) preset of continous variables
#' @param var_domain (optional) name of domain variable
#' @param var_group (optional) name of group variable
#'
#' @returns shinyapp
#' @export
#'
#' @examples
#' path <- "inst/extdata/ESS/ESS9/ESS9.csv"
#' \dontrun{runVizsurvey_from_file(path,var_itw = "INTNUM1",var_domain="CNTRY")}
runVizsurvey_from_file <- function(
    path,
    var_itw,
    vars_discretes = NULL,
    vars_continous = NULL,
    var_domain     = NULL,
    var_group      = NULL
) {

  if (is.null(path)) {
    stop("path not found.", call. = FALSE)
  }
  if (missing(var_itw) || is.null(var_itw) || !nzchar(as.character(var_itw))) {
    stop("`var_itw` is mandatory (name of interviewer variable).", call. = FALSE)
  }

  appDir <- system.file("shiny-examples", "complete", package = "vizsurvey")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `vizsurvey`.", call. = FALSE)
  }

  # Survey preparation
  temporary_dir <- tempdir()
  unlink(file.path(temporary_dir, "DATA"),recursive = T)
  dir.create(file.path(temporary_dir, "DATA"))
  link_folder <- file.path(temporary_dir, "DATA")
  dir.create(file.path(temporary_dir, "DATA",basename(path)))
  link_data_folder <- file.path(temporary_dir, "DATA",basename(path))
  file.copy(path,link_data_folder)

  create_config(
    folder_path    = link_data_folder,
    file_name      = "config.txt",
    name_survey    = NULL,
    vars_discretes = vars_discretes,
    vars_continous = vars_continous,
    var_domain     = var_domain,
    var_group      = var_group,
    var_itw        = var_itw
  )

  prepa_survey(link_data_folder, "global")

  shiny::shinyOptions(link_data_folder = link_folder)
  shiny::shinyOptions(data_rds_pattern = "global")
  shiny::shinyOptions(is_double_folder = F)
  shiny::runApp(appDir, display.mode = "normal")

  invisible(TRUE)
}
