source("ui.R")
source("server.R")

source("https://raw.githubusercontent.com/tdelc/vizsurvey/master/R/data.R")
source("https://raw.githubusercontent.com/tdelc/vizsurvey/master/R/prepa.R")
source("https://raw.githubusercontent.com/tdelc/vizsurvey/master/R/ranking.R")
source("https://raw.githubusercontent.com/tdelc/vizsurvey/master/R/utils.R")
source("https://raw.githubusercontent.com/tdelc/vizsurvey/master/R/visu.R")

shinyApp(ui = ui, server = server)
