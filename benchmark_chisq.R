
.libPaths(c("R_libs", .libPaths()))
library(dplyr)
library(purrr)
library(tibble)
# Source the R files
r_files <- list.files("R", full.names = TRUE)
for (f in r_files) source(f)

# Benchmark function
benchmark_chisq <- function(simulate_p_value) {
  # Re-define my_chisq_test locally with the toggle for benchmarking
  my_chisq_test_bench <- function(x, varname, ldist, sim_p) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    observed_counts <- table(x, useNA = "ifany")
    expected_prop <- ldist[[varname]]

    names(observed_counts)[which(is.na(names(observed_counts)))] <- "NA_"
    rare_categories <- setdiff(names(observed_counts), names(expected_prop))
    observed_counts["OTH_"] <- sum(observed_counts[rare_categories], na.rm = TRUE)
    observed_counts <- observed_counts[!names(observed_counts) %in% rare_categories]
    observed_counts <- observed_counts[which(observed_counts > 0)]

    all_levels <- union(names(observed_counts), names(expected_prop))
    obs <- observed_counts[all_levels]
    names(obs) <- all_levels
    obs[is.na(obs)] <- 0
    exp_prop <- expected_prop[all_levels]
    exp_prop[is.na(exp_prop)] <- 0

    out <- tryCatch(
      {
        t_chi <- stats::chisq.test(obs, p = exp_prop, simulate.p.value = sim_p)
        t_chi$statistic
      },
      error = function(e) {
        NA_real_
      }
    )
    return(out)
  }

  ldist <- list_dist(mtcars, c("cyl", "gear"))
  sub_mtcars <- subset(mtcars, vs == 1)

  start_time <- Sys.time()
  replicate(100, my_chisq_test_bench(sub_mtcars$cyl, "cyl", ldist, simulate_p_value))
  end_time <- Sys.time()

  return(end_time - start_time)
}

print(paste("With simulate.p.value = TRUE:", benchmark_chisq(TRUE)))
print(paste("With simulate.p.value = FALSE:", benchmark_chisq(FALSE)))
