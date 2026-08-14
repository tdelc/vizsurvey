folder_path <- tempdir()

create_config(folder_path,
              file_name = "config.txt",
              name_survey = "name_survey",
              vars_discretes = "vars_discretes",
              vars_continuous = "vars_continuous",
              var_wave = "var_wave",
              var_filter = "var_filter",
              var_intvwr = "var_intvwr")

output <- readLines(file.path(folder_path,"config.txt"))

expected <- c("name_survey = name_survey",
              "vars_discretes = vars_discretes",
              "vars_continuous = vars_continuous",
              "var_wave = var_wave",
              "var_filter = var_filter",
              "var_intvwr = var_intvwr")

test_that("create_config works", {
  expected %>% map(~{
    expect_true(.x %in% output)
  })
})
