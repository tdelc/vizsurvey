var_group <- "cyl"

test_that("prepa_stats works", {
  db_stats <- prepa_stats(mtcars,  var_group)
  colnames_expected <- c(
    var_group, "variable", "Nrow", "Nval", "type",
    "stat", "value", "value_ref", "standard"
  )
  mod_type <- c("cha", "num")
  mod_stat <- c("missing", "Nmod", "presence", "chi2", "median", "mean")

  # Format
  expect_equal(dim(db_stats), c(120, 9))
  expect_equal(colnames(db_stats), colnames_expected)

  # Modalities
  expect_true(all(db_stats$type %in% mod_type))
  expect_true(all(db_stats$stat %in% mod_stat))

  # Unique
  syn <- db_stats %>%
    group_by(group = !!sym(var_group)) %>%
    summarise(Nrow = unique(Nrow), Nval = unique(Nval))
  expect_equal(sum(duplicated(syn$group)), 0)

  # Missing
  expect_true(all(!is.na(db_stats$value)))
  expect_true(all(!is.na(db_stats$value_ref)))
  expect_true(all(!is.na(db_stats$standard)))
})
