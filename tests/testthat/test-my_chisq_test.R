ldist <- list_dist(mtcars,c("cyl","gear"))
sub_mtcars <- subset(mtcars,vs == 1)
my_chisq <- my_chisq_test(sub_mtcars$cyl,"cyl",ldist)

test_that("my_chisq_test works", {
  expect_lt(abs(my_chisq-12.00371), 0.0001)
})
