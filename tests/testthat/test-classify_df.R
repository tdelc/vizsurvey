df <- classify_df(iris)
modalities <- c("Continuous", "Modal", "Text", "Solo")

df2 <- classify_df(tibble(txt=letters,solo="1"))

test_that("classify_df classify a data frame", {
  expect_equal(colnames(df), c("variable", "type"))
  expect_equal(sum(duplicated(df$variable)), 0)
  expect_true(all(df$type %in% modalities))
  expect_equal(df$type, c("Continuous", "Continuous", "Continuous", "Continuous", "Modal"))
  expect_equal(df2$type, c("Solo", "Text"))
})
