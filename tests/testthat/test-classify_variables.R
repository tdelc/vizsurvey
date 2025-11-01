df <- classify_variables(iris)
modalities <- c("Continuous", "Modal", "Text")

test_that("classify_variables classify a data frame", {
  expect_equal(colnames(df), c("variable", "type"))
  expect_equal(sum(duplicated(df$variable)), 0)
  expect_true(all(df$type %in% modalities))
  expect_equal(df$type, c("Continuous", "Continuous", "Continuous", "Continuous", "Modal"))
})
