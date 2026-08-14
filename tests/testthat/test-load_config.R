folder_path <- tempdir()

create_config(folder_path,
              file_name = "config.txt",
              name_survey = "name_survey",
              vars_discretes = "vars_discretes",
              vars_continuous = "vars_continuous",
              var_wave = "var_wave",
              var_filter = "var_filter",
              var_intvwr = "var_intvwr")

df_config <- load_config(file.path(folder_path,"config.txt"))

expected <- c("name_survey","vars_discretes","vars_continuous",
                  "var_wave","var_filter","var_intvwr")

test_that("load_config works", {
  expect_equal(df_config$key, expected)
  expect_equal(unlist(df_config$value), expected)
})

test_that("extract_config works", {
  expect_equal(extract_config(df_config,"vars_discretes"), "VARS_DISCRETES")
  expect_equal(extract_config(df_config,"vars_continuous"), "VARS_CONTINUOUS")
  expect_equal(extract_config(df_config,"var_wave"), "VAR_WAVE")
  expect_equal(extract_config(df_config,"var_filter"), "VAR_FILTER")
  expect_equal(extract_config(df_config,"var_intvwr"), "VAR_INTVWR")
  expect_equal(extract_config(df_config,"no"), character(0))
})
