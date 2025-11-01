df <- classify_variables(mtcars)
var_d <- df %>%
  filter(type == "Modal") %>%
  pull(variable)
var_c <- df %>%
  filter(type == "Continuous") %>%
  pull(variable)
var_group <- "cyl"
db_stats <- prepa_stats(mtcars, var_d, var_c, var_group)

test_that("prepa_stats works", {
  p <- heat_map_group(db_stats, 0)
  expect_equal(class(p), c("gg", "ggplot"))

  expect_equal(colnames(p$data), c(
    var_group, "variable", "Nrow", "Nval", "type",
    "standard", "stat_standard", "info"
  ))
})
