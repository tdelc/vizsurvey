## 2025-05-15 - [Optimization of my_chisq_test]
**Learning:** In the `vizsurvey` package, `my_chisq_test` was using `stats::chisq.test` with `simulate.p.value = TRUE` (which defaults to 2000 replicates). However, only the test statistic ($statistic) was being used, and the p-value was discarded. Disabling p-value simulation provides the same statistic but is ~400x faster in the observed benchmark (~110x faster in more realistic conditions).
**Action:** Always check if both the statistic and the p-value are needed when using `stats::chisq.test`. If only the statistic is required, set `simulate.p.value = FALSE` to avoid unnecessary Monte Carlo simulations.

## 2025-05-15 - [Optimization of list_dist]
**Learning:** In the `vizsurvey` package, `list_dist` was using a heavy `dplyr` pipeline inside a `purrr::map` loop to calculate proportions and group rare categories. For large datasets with many categorical variables, the overhead of creating tibbles and calling multiple `dplyr` verbs for every variable is significant.
**Action:** Replace `dplyr` pipelines inside loops with efficient base R operations (`table`, `prop.table`, `tapply`) when only basic aggregation is needed. This optimization resulted in a ~2x performance gain in benchmarks.
