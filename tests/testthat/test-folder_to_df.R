folder_path <- tempdir()

write.csv(iris,file=file.path(folder_path,"data_from_r.csv"))

create_config(
  folder_path    = folder_path,
  file_name      = "config.txt",
  var_group      = "Species"
)

output <- folder_to_df(folder_path)

test_that("multiplication works", {
  expect_equal(names(output), c("df", "configs"))
  expect_equal(names(output$configs), c("vd","vc","path","vg","vw","vz","enq"))
})
