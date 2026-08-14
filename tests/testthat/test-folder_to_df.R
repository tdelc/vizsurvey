folder_path <- tempdir()

write.csv(iris,file=file.path(folder_path,"data_from_r.csv"))

create_config(
  folder_path    = folder_path,
  file_name      = "config.txt",
  var_intvwr     = "Species"
)

output <- folder_to_df(folder_path)

names_expected <- c("name_survey","vars_discretes","vars_continuous","vars_ignore",        
                    "prefix_discretes","prefix_continuous","prefix_ignore","path",               
                    "var_wave","var_filter","var_intvwr","var_intv",
                    "var_date","var_info_geo","var_timer","var_itm_duration",
                    "var_session","duration_min_during","duration_min_inter","night_start",
                    "night_end","vars_wave","vars_filter")

test_that("multiplication works", {
  expect_equal(names(output), c("df", "configs"))
  expect_equal(names(output$configs), names_expected)
})
