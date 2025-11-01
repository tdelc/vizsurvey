#' Shiny Example of vizsurvey
#'
#' @returns shinyapp
#' @export
#'
#' @examples
#' # runExample()
runExample <- function() {
  appDir <- system.file("shiny-examples", "minimal", package = "vizsurvey")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `vizsurvey`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
