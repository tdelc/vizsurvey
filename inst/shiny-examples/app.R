library(devtools)
load_all()
runVizsurvey_from_folder("../extdata",depth_folder = 3)

# link <- "inst/extdata"
# data_rds_pattern = "global"
# depth_folder = 3
# appDir <- system.file("shiny-examples", "complete", package = "vizsurvey")
# 
# shiny::shinyOptions(link_data_folder = normalizePath(link))
# shiny::shinyOptions(data_rds_pattern = data_rds_pattern)
# shiny::shinyOptions(depth_folder = depth_folder)
# shiny::runApp(appDir, display.mode = "normal")
  