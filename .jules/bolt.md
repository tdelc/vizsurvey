## 2025-05-15 - Redundant P-value Simulation in chisq.test
**Learning:** In R, `stats::chisq.test(..., simulate.p.value = TRUE)` is significantly slower (often by several orders of magnitude) because it performs 2,000 Monte Carlo simulations by default. If the code only requires the test statistic (`$statistic`) and not the p-value, this simulation is entirely redundant as the statistic itself is calculated deterministically.
**Action:** Disable `simulate.p.value` when only the Chi-squared statistic is needed in performance-sensitive loops.
