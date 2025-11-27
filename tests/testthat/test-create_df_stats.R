folder_path <- tempdir()

write.csv(mtcars,file=file.path(folder_path,"data_from_r.csv"))

create_config(
  folder_path    = folder_path,
  file_name      = "config.txt",
  var_zone       = "gear"
)

output <- folder_to_df(folder_path)

df_stats <- create_df_stats(output$df, output$configs,"CYL")
df_stats2 <- create_df_stats(output$df, output$configs,"CYL",zone_filter = 4)

expected <- c("CYL","variable","Nrow","Nval","type",
              "stat","value","value_ref","standard","zone")

test_that("create_df_stats works", {
  expect_equal(dim(df_stats), c(120,10))
  expect_equal(dim(df_stats2), c(80,10))
  expect_equal(names(df_stats), expected)
  expect_equal(names(df_stats2), expected)
  expect_equal(round(mean(df_stats$standard),4), -0.0119)
  expect_equal(round(mean(df_stats2$standard,na.rm=T),4), 0)
})
