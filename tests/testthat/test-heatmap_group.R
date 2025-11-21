var_group <- "cyl"
db_stats <- prepa_stats(mtcars, var_group)

test_that("prepa_stats works", {
  p <- heatmap_group(db_stats, 0)
  expect_true(all(c("gg", "ggplot") %in% class(p)))

  expect_equal(colnames(p$data), c(
    var_group, "variable", "Nrow", "Nval", "type",
    "standard", "stat_standard", "info"
  ))
})
