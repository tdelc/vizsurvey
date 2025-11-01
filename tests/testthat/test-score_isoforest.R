score <- score_isoforest(iris %>% dplyr::select(where(is.numeric)))

test_that("score_isoforest works", {
  expect_error(score_isoforest(iris))
  expect_equal(class(score), "numeric")
  expect_equal(length(score), nrow(iris))
  expect_lte(abs(mean(score) - 0.46), 0.01)
})
