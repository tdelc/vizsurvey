score <- score_isoforest(iris %>% dplyr::select(where(is.numeric)))
score_NA <- score_isoforest(iris[1:4,] %>% dplyr::select(where(is.numeric)))

test_that("score_isoforest works", {
  expect_error(score_isoforest(iris))
  expect_equal(class(score), "numeric")
  expect_equal(length(score), nrow(iris))
  expect_lte(abs(mean(score) - 0.46), 0.01)
  expect_equal(score_NA, NA)
})
