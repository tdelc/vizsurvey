folder_path <- tempdir()

dir.create(file.path(folder_path,"DATA_TEST"),showWarnings = F)

folder_path <- file.path(folder_path,"DATA_TEST")

subfolder1 <- file.path(folder_path,"SUB1")
subfolder2 <- file.path(folder_path,"SUB2")

dir.create(subfolder1,showWarnings = F)
dir.create(subfolder2,showWarnings = F)

write.csv(mtcars,file=file.path(subfolder1,"data.csv"))
write.csv(mtcars,file=file.path(subfolder2,"data.csv"))

create_config(folder_path = subfolder1, file_name = "config.txt")
create_config(folder_path = subfolder2, file_name = "config.txt")

prepa_survey(subfolder1)

global <- readRDS(file.path(subfolder1,"global.rds"))

test_that("prepa_survey works", {
  expect_equal(names(global), c("configs","df","df_stats","df_stats_group"))
  expect_equal(length(global$configs), 7)
  expect_equal(global$df_stats_group, NULL)
  expect_equal(global$df_stats, NULL)
})

prepa_surveys(folder_path,depth_folder = 2)

global1 <- readRDS(file.path(subfolder1,"global.rds"))
global2 <- readRDS(file.path(subfolder2,"global.rds"))

global1$configs$path <- ""
global2$configs$path <- ""

test_that("prepa_survey works", {
  expect_equal(global1,global2)
})
