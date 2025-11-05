list_distance <- list_dist(mtcars,c("cyl","vs","gear"))

test_that("list_dist works", {
  expect_equal(names(list_distance),c("cyl","vs","gear"))
  expect_equal(sapply(list_distance,length),c("cyl"=3,"vs"=2,"gear"=3))
  expect_equal(sapply(list_distance,mean),c("cyl"=1/3,"vs"=1/2,"gear"=1/3))
})
