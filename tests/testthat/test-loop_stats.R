folder_path <- tempdir()

write.csv(mtcars,file=file.path(folder_path,"data_from_r.csv"))

create_config(
  folder_path    = folder_path,
  file_name      = "config.txt",
  var_filter     = "gear"
)

output <- folder_to_df(folder_path)
df_stats <- loop_stats(output$df,output$configs,"VS")

test_that("loop_stats works", {
  expect_equal(dim(df_stats), c(288,10))
  expect_equal(sort(unique(df_stats$filter)), c("3","4","5","All"))
  expect_equal(round(mean(df_stats$standard,na.rm=T),0.36), 0)
})
