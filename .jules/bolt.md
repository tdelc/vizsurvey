## 2025-05-15 - [Optimization of my_chisq_test]
**Learning:** In the `vizsurvey` package, `my_chisq_test` was using `stats::chisq.test` with `simulate.p.value = TRUE` (which defaults to 2000 replicates). However, only the test statistic ($statistic) was being used, and the p-value was discarded. Disabling p-value simulation provides the same statistic but is ~400x faster in the observed benchmark (~110x faster in more realistic conditions).
**Action:** Always check if both the statistic and the p-value are needed when using `stats::chisq.test`. If only the statistic is required, set `simulate.p.value = FALSE` to avoid unnecessary Monte Carlo simulations.

## 2025-05-16 - [Optimization of classify_df]
**Learning:** Using `dplyr::summarise(across(...))` followed by `pivot_longer` for column-wise metadata classification (e.g., in `classify_df`) is less efficient than using `vapply`. For a dataset with 100 columns and 100k rows, the `vapply` approach is ~2.3x faster.
**Action:** Prefer `vapply` over `dplyr::summarise(across(...))` for high-performance column-wise metadata classification in this codebase, especially when the results need to be reshaped into a long format.
