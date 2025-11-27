folder_path <- tempdir()

create_config(folder_path,
              file_name = "config.txt",
              name_survey = "name_survey",
              vars_discretes = "vars_discretes",
              vars_continous = "vars_continous",
              var_wave = "var_wave",
              var_zone = "var_zone",
              var_group = "var_group")

output <- readLines(file.path(folder_path,"config.txt"))

expected <- c("name_survey = name_survey",
              "vars_discretes = vars_discretes",
              "vars_continous = vars_continous",
              "var_wave = var_wave",
              "var_zone = var_zone",
              "var_group = var_group")

test_that("create_config works", {
  expected %>% map(~{
    expect_true(.x %in% output)
  })
})
