iris_cha <- iris %>%
  mutate(Sepal.Length = as.character(Sepal.Length),
         Sepal.Width = as.character(Sepal.Width))

list_df <- correct_list_df(list(iris_cha,iris))

test_that("correct_list_df  works", {
  expect_equal(length(list_df), 2)
  expect_equal(colnames(list_df[[1]]), colnames(list_df[[2]]))
  expect_equal(nrow(list_df[[1]]), nrow(list_df[[2]]))
})
