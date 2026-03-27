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

## 2026-03-25 - [Optimization of heatmap_group string formatting]
**Learning:** In , using  is approximately 2x faster than the combined overhead of  and . Vectorized C-level formatting is much more efficient for large datasets.
**Action:** Prefer  over  +  for formatting numeric values in tooltips or labels within the  package.

## 2025-05-16 - [Optimization of heatmap_group string formatting]
**Learning:** In `heatmap_group`, using `sprintf("%s : %.2f", stat, standard)` is approximately 2x faster than the combined overhead of `round(standard, 2)` and `paste(stat, ..., sep = " : ")`. Vectorized C-level formatting is much more efficient for large datasets.
**Action:** Prefer `sprintf()` over `paste()` + `round()` for formatting numeric values in tooltips or labels within the `vizsurvey` package.

## 2025-05-17 - [Optimization of prepa_stats]
**Learning:** The `prepa_stats` function had several performance bottlenecks: redundant `mutate(across())` calls, inefficient `group_by %>% mutate` pattern to spread a single statistic across long data, and use of `ifelse` for large vector replacements. Combining transformations and using `left_join` for statistic distribution is more efficient in `dplyr`.
**Action:** Minimize the number of `across()` passes in `mutate()`. Use `left_join()` to distribute group-level statistics to long-format data instead of re-grouping. Prefer `replace()` over `ifelse()` for simple vector substitutions.

## 2025-05-18 - [Optimization of chi-square and prepa_stats summarise]
**Learning:** In `prepa_stats`, calculating simple statistics like `missing` and `presence` rates within the `summarise(group_by(...))` phase is redundant and slow because they can be derived via vectorized operations on the resulting summary table using `Nval` and `Nrow`. Additionally, `stats::chisq.test` has significant overhead; a manual calculation of the chi-square statistic is ~20x faster when p-values are not needed.
**Action:** Move simple rate calculations out of heavy grouping phases into vectorized post-processing. Use manual chi-square calculations ($\sum (O-E)^2/E$) instead of the full `stats::chisq.test` when only the statistic is required.

## 2025-05-19 - [Optimization of heatmap_group info column]
**Learning:** Constructing a complex multi-line tooltip string (the 'info' column) using 'paste0' for 10,000+ tiles in a ggplot heatmap is significantly slower than using 'sprintf'. Vectorized C-level formatting handles multiple substitutions and newline characters more efficiently.
**Action:** Use 'sprintf' for all complex string formatting in 'heatmap_group' to maximize performance of visualization preparation on large datasets.
