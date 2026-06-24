#' Shiny vizsurvey
#'
#' @returns shinyapp
#' @export
#'
#' @examples
#' \dontrun{runVizsurvey()}
runVizsurvey <- function() {

  appDir <- system.file("shiny-examples", "bslib", package = "vizsurvey")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `vizsurvey`.", call. = FALSE)
  }

  shiny::shinyOptions(link_data_folder = NULL)
  shiny::shinyOptions(data_rds_pattern = NULL)
  shiny::shinyOptions(depth_folder = NULL)
  shiny::runApp(appDir, display.mode = "normal",launch.browser = TRUE)

  invisible(TRUE)
}

#' Shiny vizsurvey with already prepared data
#'
#' @param link link to directory of data
#' @param depth_folder level of depth for the tree structure
#' @param data_rds_pattern name of the rds file contains all the data
#'
#' @returns shinyapp
#' @export
#'
#' @examples
#' # We assume that config.txt, and prepa_surveys are already done here.
#' \dontrun{runVizsurvey_from_folder("inst/extdata",depth_folder = 3)}
runVizsurvey_from_folder <- function(
    link,
    data_rds_pattern = "global",
    depth_folder = 1
) {

  appDir <- system.file("shiny-examples", "bslib", package = "vizsurvey")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `vizsurvey`.", call. = FALSE)
  }

  shiny::shinyOptions(link_data_folder = normalizePath(link))
  shiny::shinyOptions(data_rds_pattern = data_rds_pattern)
  shiny::shinyOptions(depth_folder = depth_folder)
  shiny::runApp(appDir, display.mode = "normal",launch.browser = TRUE)

  invisible(TRUE)
}

#' Shiny vizsurvey from a R data.frame
#'
#' @param df data.frame
#' @param vars_discretes (optional) preset of discretes variables
#' @param vars_continous (optional) preset of continous variables
#' @param var_wave (optional) name of wave variable
#' @param var_filter (optional) name of filter variable
#' @param var_intvwr (optional) name of interviewer variable
#'
#' @returns shinyapp
#' @export
#'
#' @examples
#' library(laeken)
#' data(eusilc)
#' set.seed(123)
#' eusilc$NR_ITW <- paste(eusilc$db040,sample(1:5,nrow(eusilc),replace = TRUE),sep="-")
#' \dontrun{runVizsurvey_from_r(eusilc,var_intvwr = "NR_ITW",var_filter = "db040")}
runVizsurvey_from_r <- function(
    df,
    vars_discretes  = NULL,
    vars_continuous = NULL,
    var_wave       = NULL,
    var_filter     = NULL,
    var_intvwr     = NULL
) {

  if (is.null(df)) {
    stop("df not present.", call. = FALSE)
  }

  appDir <- system.file("shiny-examples", "bslib", package = "vizsurvey")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `vizsurvey`.", call. = FALSE)
  }

  # Survey preparation
  temporary_dir <- tempdir()
  unlink(file.path(temporary_dir, "DATA"),recursive = T)
  dir.create(file.path(temporary_dir, "DATA"))
  link_folder <- file.path(temporary_dir, "DATA")
  readr::write_csv(df,file=file.path(link_folder,"data_from_r.csv"),
                   col_names = T)
  
  create_config(
    folder_path    = link_folder,
    file_name      = "config.txt",
    name_survey    = NULL,
    vars_discretes  = vars_discretes,
    vars_continuous = vars_continuous,
    var_wave        = var_wave,
    var_filter      = var_filter,
    var_intvwr      = var_intvwr
  )
  
  prepa_survey(link_folder)

  shiny::shinyOptions(link_data_folder = link_folder)
  shiny::shinyOptions(data_rds_pattern = "global")
  shiny::shinyOptions(depth_folder = 1)
  shiny::runApp(appDir, display.mode = "normal",launch.browser = TRUE)

  invisible(TRUE)
}
