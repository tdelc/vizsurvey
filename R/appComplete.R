#' Shiny vizsurvey
#'
#' @param link_data_rep Link of the data folder
#'
#' @returns shinyapp
#' @export
#'
#' @examples
#' # runVizsurvey()
runVizsurvey <- function(
    link_data_folder = NULL,
    is_double_folder = T
    ) {

  appDir <- system.file("shiny-examples", "complete", package = "vizsurvey")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `vizsurvey`.", call. = FALSE)
  }

  shiny::shinyOptions(link_data_folder = link_data_folder)
  shiny::shinyOptions(is_double_folder = is_double_folder)
  shiny::runApp(appDir, display.mode = "normal")
}



# runAlone("C:/Users/thomas.delclite/OneDrive - GCloud Belgium/Statbel/Projets/Dashboard de suivi de collecte/vizsurvey/inst/shiny-examples/complete/data/ESS/ESS9/ESS9.csv","intnum1")
runAlone <- function(
    link_file,
    var_itw,
    vars_discretes = NULL,
    vars_continous = NULL,
    var_domain = NULL,
    var_group = NULL
    ) {

  appDir <- system.file("shiny-examples", "complete", package = "vizsurvey")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `vizsurvey`.", call. = FALSE)
  }

  # Survey preparation
  temporary_dir <- tempdir()
  if (!is.null(link_file)){
    unlink(file.path(temporary_dir, "DATA"),recursive = T)
    dir.create(file.path(temporary_dir, "DATA"))
    dir.create(file.path(temporary_dir, "DATA",basename(link_file)))
    link_folder <- file.path(temporary_dir, "DATA")
    link_data_folder <- file.path(temporary_dir, "DATA",basename(link_file))
    file.copy(link_file,link_data_folder)
    create_config(folder_path = link_data_folder,
                  file_name = "config.txt",
                  name_survey = NULL,
                  vars_discretes,
                  vars_continous,
                  var_domain,
                  lab_domain,
                  var_group,
                  lab_group,
                  var_itw)
    prepa_survey(link_data_folder)
  }

  shiny::shinyOptions(link_data_folder = link_folder)
  shiny::shinyOptions(is_double_folder = F)
  shiny::runApp(appDir, display.mode = "normal")
}
