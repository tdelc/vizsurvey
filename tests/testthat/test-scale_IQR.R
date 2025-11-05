vec_scale <- scale_IQR(iris$Sepal.Length)

test_that("scale_IQR works", {
  expect_lt(abs(mean(vec_scale)-0.03333), 0.0001)
  expect_lt(abs(sd(vec_scale)-0.636973), 0.0001)
})
