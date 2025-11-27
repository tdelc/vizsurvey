folder_path <- tempdir()

create_config(folder_path,
              file_name = "config.txt",
              name_survey = "name_survey",
              vars_discretes = "vars_discretes",
              vars_continous = "vars_continous",
              var_wave = "var_wave",
              var_zone = "var_zone",
              var_group = "var_group")

df_config <- load_config(file.path(folder_path,"config.txt"))

expected <- c("name_survey","vars_discretes","vars_continous",
                  "var_wave","var_zone","var_group")

test_that("load_config works", {
  expect_equal(df_config$key, expected)
  expect_equal(unlist(df_config$value), expected)
})

test_that("extract_config works", {
  expect_equal(extract_config(df_config,"vars_discretes"), "VARS_DISCRETES")
  expect_equal(extract_config(df_config,"vars_continous"), "VARS_CONTINOUS")
  expect_equal(extract_config(df_config,"var_wave"), "VAR_WAVE")
  expect_equal(extract_config(df_config,"var_zone"), "VAR_ZONE")
  expect_equal(extract_config(df_config,"var_group"), "VAR_GROUP")
  expect_equal(extract_config(df_config,"no"), character(0))
})
