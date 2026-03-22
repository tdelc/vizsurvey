## 2025-05-15 - [Optimization of my_chisq_test]
**Learning:** In the `vizsurvey` package, `my_chisq_test` was using `stats::chisq.test` with `simulate.p.value = TRUE` (which defaults to 2000 replicates). However, only the test statistic ($statistic) was being used, and the p-value was discarded. Disabling p-value simulation provides the same statistic but is ~400x faster in the observed benchmark (~110x faster in more realistic conditions).
**Action:** Always check if both the statistic and the p-value are needed when using `stats::chisq.test`. If only the statistic is required, set `simulate.p.value = FALSE` to avoid unnecessary Monte Carlo simulations.

## 2025-05-15 - [Optimization of list_dist]
**Learning:** In the `vizsurvey` package, `list_dist` was using a heavy `dplyr` pipeline inside a `purrr::map` loop to calculate proportions and group rare categories. For large datasets with many categorical variables, the overhead of creating tibbles and calling multiple `dplyr` verbs for every variable is significant.
**Action:** Replace `dplyr` pipelines inside loops with efficient base R operations (`table`, `prop.table`, `tapply`) when only basic aggregation is needed. This optimization resulted in a ~2x performance gain in benchmarks.

## 2025-05-16 - [Optimization of classify_df]
**Learning:** Using `dplyr::summarise(across(...))` followed by `pivot_longer` for column-wise metadata classification (e.g., in `classify_df`) is less efficient than using `vapply`. For a dataset with 100 columns and 100k rows, the `vapply` approach is ~2.3x faster.
**Action:** Prefer `vapply` over `dplyr::summarise(across(...))` for high-performance column-wise metadata classification in this codebase, especially when the results need to be reshaped into a long format.

## 2025-05-16 - [Optimization of scale_IQR]
**Learning:** In the `vizsurvey` package, `scale_IQR` was calling `quantile` multiple times (for 0.25 and 0.75) and `median` once (which calls `quantile` for 0.5 internally). For large vectors, this means multiple sorting or scanning passes.
**Action:** Use a single `stats::quantile(x, probs = c(0.25, 0.5, 0.75), names = FALSE)` call to retrieve all three percentiles at once. This reduces the number of passes over the data and provides a ~2.5x performance gain for the function.

## 2025-05-16 - [Optimization of empty_as_na]
**Learning:** In the `vizsurvey` package, `empty_as_na` was using `ifelse(vec == "", NA_character_, vec)`. While concise, `ifelse` is slow in R due to its handling of arguments and construction of the result vector. Vectorized assignment (`vec[vec == ""] <- NA_character_`) is significantly faster.
**Action:** Replace `ifelse` with vectorized assignment for simple value replacement in vectors. This optimization provided a ~5-6x performance gain for character vectors with 1 million elements.
