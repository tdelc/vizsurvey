df <- airquality
df[which(is.na(airquality$Ozone)), "Ozone"] <- ""
df_cor <- df
df_cor$Ozone <- empty_as_na(df_cor$Ozone)

test_that("empty_as_na correct a vector", {
  expect_equal(unique(df_cor[df$Ozone == "", "Ozone"]), NA_character_)
  expect_true(all(df_cor$Ozone != "", na.rm = T))
})
