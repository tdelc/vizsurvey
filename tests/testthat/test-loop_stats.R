folder_path <- tempdir()

write.csv(mtcars,file=file.path(folder_path,"data_from_r.csv"))

create_config(
  folder_path    = folder_path,
  file_name      = "config.txt",
  var_zone       = "gear"
)

output <- folder_to_df(folder_path)
df_stats <- loop_stats(output$df,output$configs,"VS")

test_that("loop_stats works", {
  expect_equal(dim(df_stats), c(320,10))
  expect_equal(unique(df_stats$zone), c("All","4","3","5"))
  expect_equal(round(mean(df_stats$standard,na.rm=T),4), 0)
})
