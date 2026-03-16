## 2025-05-15 - Redundant Monte Carlo simulations in chisq.test
**Learning:** `stats::chisq.test` with `simulate.p.value = TRUE` performs expensive Monte Carlo simulations (default 2000 replicates) to compute the p-value. If only the test statistic (`$statistic`) is required, these simulations are redundant and significantly degrade performance.
**Action:** Always check if both p-value and statistic are needed when using `simulate.p.value = TRUE`. If only the statistic is needed, set `simulate.p.value = FALSE` to avoid unnecessary computation.
