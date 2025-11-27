eusilc_sim <- create_eusilc_sim()

vec_itw <- sort(unique(eusilc_sim$nr_itw))
vec_year <- sort(unique(eusilc_sim$db010))

expanded_itw <- expand.grid(sort(unique(eusilc_sim$db040)),"-",1:5) %>%
  mutate(itw = paste0(Var1,Var2,Var3)) %>% pull(itw) %>% sort()

test_that("create_eusilc_sim works", {
  expect_equal(dim(eusilc_sim), c(14827,30))
  expect_equal(vec_itw, expanded_itw)
  expect_equal(vec_year, 2018:2020)
})
