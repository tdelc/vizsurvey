library(tidyverse)

import_ess <- function(name_CF,name,year,var_itw){

  path = "C:/Users/thomas.delclite/Downloads/"

  temp_dir <- tempdir()

  unzip (paste0(path,name,".zip"), exdir = temp_dir)
  unzip (paste0(path,name_CF,".zip"), exdir = temp_dir)

  df_ess <- read_csv(paste0(temp_dir,"/",name,".csv"))
  df_essCF <- read_csv(paste0(temp_dir,"/",name_CF,".csv"))

  colnames(df_ess) <- toupper(colnames(df_ess))
  colnames(df_essCF) <- toupper(colnames(df_essCF))

  df_ess <<- df_ess
  df_essCF <<- df_essCF

  unlink(temp_dir)

  df_ess <- df_ess %>%
    left_join(df_essCF %>% select(CNTRY,IDNO,!!sym(var_itw)))

  path_out <- paste0("data/ESS/ESS",year,"/ESS",year,".csv")

  write_csv(df_ess,file=path_out)

}

import_ess("ESS1CFe01","ESS1INTe05_1",1,"INTNUM1")
import_ess("ESS2CFe03_2","ESS2INTe03_2",2,"INTNUM1")
import_ess("ESS3CF_ed1_1","ESS3INTe02",3,"INTNUM1")
import_ess("ESS4CF_e02_1","ESS4INTe03",4,"INTNUM1")
import_ess("ESS5CFe02_1","ESS5INTe03",5,"INTNUM1")
import_ess("ESS6CFe02","ESS6INTe02_1",6,"INTNUM1")
import_ess("ESS7CFe02_1","ESS7INTe02_1",7,"INTNUM1")
import_ess("ESS8CFe03","ESS8INTe02",8,"INTNUM1")
import_ess("ESS9CFe03","ESS9INTe03",9,"INTNUM1")
import_ess("ESS9CFe03","ESS9INTe03",9,"INTNUM1")
import_ess("ESS10CF","ESS10I",10,"INTNUM1")
import_ess("ESS11CF","ESS11",11,"INTNUM1")

df_ess10 <- read_csv("C:/Users/thomas.delclite/Downloads/ESS10I/ESS10I.csv")
df_ess10CF <- read_csv("C:/Users/thomas.delclite/Downloads/ESS10CF/ESS10CF.csv")

df_ess11 <- read_csv("inst/shiny-examples/complete/data/ESS11/ESS11.csv")
df_ess11CF <- read_csv("inst/shiny-examples/complete/data/ESS11/ESS11CF.csv")

# Nettoyage
# df_ess9 <- read_csv("data/ESS/ESS9/ESS9.csv")
