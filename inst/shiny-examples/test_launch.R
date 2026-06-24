setwd("Z:/E8/1054-Monitoring-Citizens/data_dashboard/vizsurvey")
library(devtools)
load_all()

# df_timer <- read.csv("inst/extdata/SILC/HFILE/timers.csv")
# 
# df_timer %>% 
#   correct_df_timer %>% 
#   correct_df_timer_session() 

library(laeken)
data(eusilc)
set.seed(123)
eusilc$NR_ITW <- paste(eusilc$db040,sample(1:5,nrow(eusilc),replace = TRUE),sep="-")
runVizsurvey_from_r(eusilc,var_intvwr = "NR_ITW",var_filter = "db040")
