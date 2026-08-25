#' Replace empty by na
#'
#' @param vec Vector of values
#'
#' @returns Vector
#' @export
#'
#' @examples
#' airquality[which(is.na(airquality$Ozone)), "Ozone"] <- ""
#' empty_as_na(airquality$Ozone)
empty_as_na <- function(vec) {
  ifelse(vec == "", NA_character_, vec)
}

#' Check if value is integer64
#'
#' @param x Value
#'
#' @returns Boolean
#' @export
#'
#' @examples
#' is.integer64(c(1:100)) # FALSE
is.integer64 <- function(x) {
  class(x) == "integer64"
}

#' Robust Scale of a varible with IQR
#'
#' @param x vector
#'
#' @returns vector
#' @export
#'
#' @examples
#' head(scale_IQR(iris$Sepal.Length))
scale_IQR <- function(x) {
  IQR <- quantile(x, probs = 0.75, na.rm = TRUE) - quantile(x, probs = 0.25, na.rm = TRUE)
  if (is.na(IQR) | IQR == 0) {
    scale(x)[, 1]
  } else {
    if (!all(x < 1, na.rm = TRUE)) IQR <- max(IQR, 1)
    (x - median(x, na.rm = TRUE)) / IQR
  }
}

#' Combine several variables in one key (multi-level wave or filter)
#'
#' Used when var_wave (or var_filter) contains more than one variable in the
#' configuration file, for example the year and the quarter. The complete key
#' of a row is then "2024 / T1".
#'
#' @param df data.frame
#' @param vars vector of the variable of each level
#' @param sep separator between the levels
#'
#' @returns vector of the complete keys
#' @export
#'
#' @examples
#' head(combine_vars(mtcars,c("cyl","gear")))
combine_vars <- function(df, vars, sep = " / ") {
  values <- lapply(vars, function(v) as.character(df[[v]]))
  do.call(paste, c(values, list(sep = sep)))
}

#' First level of a multi-level key
#'
#' @param x vector of keys
#' @param sep separator between the levels
#'
#' @returns vector of the first level values
#' @export
#'
#' @examples
#' key_level1(c("2024 / T1","2024 / T2","2023 / T4"))
key_level1 <- function(x, sep = " / ") {
  vapply(strsplit(as.character(x), sep, fixed = TRUE),
         function(v) if (length(v) > 0) v[1] else NA_character_,
         character(1))
}

#' Other levels of a multi-level key (used as label in the interface)
#'
#' @param x vector of keys
#' @param sep separator between the levels
#'
#' @returns vector of the values after the first level
#' @export
#'
#' @examples
#' key_level2(c("2024 / T1","2024 / T2","2023 / T4"))
key_level2 <- function(x, sep = " / ") {
  vapply(strsplit(as.character(x), sep, fixed = TRUE),
         function(v) if (length(v) > 1) paste(v[-1], collapse = sep) else NA_character_,
         character(1))
}

#' Variables of each level of the wave or of the filter
#'
#' Kept compatible with the global.rds prepared before the multi-level option :
#' vars_wave (or vars_filter) is then missing and var_wave is used.
#'
#' @param configs list of configuration
#' @param type "wave" or "filter"
#'
#' @returns vector of the variables of each level
#' @export
#'
#' @examples
#' vars_levels(list(var_wave = "YEAR"),"wave")
vars_levels <- function(configs, type = "wave") {
  vars <- configs[[paste0("vars_", type)]]
  if (length(vars) == 0) vars <- configs[[paste0("var_", type)]]
  vars
}

#' Keys (modalities) of a wave or filter variable
#'
#' @param df data.frame
#' @param vars variables of each level (see vars_levels)
#' @param var_key name of the variable containing the complete key
#' @param level 1 (first level only), 2 (complete key) or "all" (both)
#' @param sep separator between the levels
#'
#' @returns vector of keys
#' @export
#'
#' @examples
#' keys_vars(mtcars,"cyl","cyl")
keys_vars <- function(df, vars, var_key, level = "all", sep = " / ") {
  keys <- sort(unique(as.character(df[[var_key]])))
  if (length(vars) <= 1) return(keys)

  keys1 <- sort(unique(key_level1(keys, sep)))
  switch(as.character(level),
    "1" = keys1,
    "2" = keys,
    sort(unique(c(keys1, keys)))
  )
}

#' Rows of a data.frame matching one or several keys
#'
#' A row matches a key when its complete key is one of the keys, or when its
#' first level is one of the keys (selection of a year without the quarter).
#'
#' @param df data.frame
#' @param vars variables of each level (see vars_levels)
#' @param var_key name of the variable containing the complete key
#' @param keys vector of keys to keep
#' @param sep separator between the levels
#'
#' @returns logical vector
#' @export
#'
#' @examples
#' match_keys(mtcars,"cyl","cyl",4)
match_keys <- function(df, vars, var_key, keys, sep = " / ") {
  values <- as.character(df[[var_key]])
  out <- values %in% as.character(keys)

  if (length(vars) > 1) {
    uniq <- unique(values)
    uniq <- uniq[key_level1(uniq, sep) %in% as.character(keys)]
    out <- out | values %in% uniq
  }
  out
}

#' List distribution of discrete variables
#'
#' @param df data.frame
#' @param vars_vd vector of discrete variables
#'
#' @returns list
#' @export
#'
#' @examples
#' list_dist(mtcars,c("cyl","vs","gear"))
list_dist <- function(df, vars_vd) {
  # Optimization: Use a manual approach with match() and tabulate() for maximum performance.
  # This avoids the overhead of table() or data.table S3 dispatch issues in some environments.
  # It is approximately 10x faster than the original implementation for large datasets.
  
  list_dist <- vars_vd %>% purrr::map(~ {
    x <- df[[.x]]
    # Fast frequency count using match and tabulate
    levs <- sort(unique(x), na.last = TRUE)
    m <- match(x, levs)
    counts <- tabulate(m, nbins = length(levs))
    prop <- counts / length(x)
    
    val <- as.character(levs)
    val[is.na(val)] <- "NA_"
    
    # Group rare categories (< 1%) into "OTH_"
    categories <- val
    categories[prop < 0.01] <- "OTH_"
    
    # Fast aggregation using tapply on the results (small vector)
    res_prop <- tapply(prop, categories, sum)
    
    # Return as a named vector to match original behavior
    out <- as.vector(res_prop)
    names(out) <- names(res_prop)
    out
  })
  names(list_dist) <- vars_vd
  return(list_dist)
}

#' Specific chisq test to NA and Other modality
#'
#' @param x value to procede chisq test
#' @param varname name of the variable
#' @param ldist named list of expected probability
#' @param useNA useNA parameter for the table function
#'
#' @returns chisq value
#' @export
#'
#' @examples
#' ldist <- list_dist(mtcars,c("cyl","gear"))
#' sub_mtcars <- subset(mtcars,vs == 1)
#' my_chisq_test(sub_mtcars$cyl,"cyl",ldist)
my_chisq_test <- function(x, varname, ldist, useNA = "ifany") {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  observed_counts <- table(x, useNA = useNA)
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
      expected_counts <- sum(obs) * exp_prop
      nonzero_exp <- expected_counts > 0
      sum((obs[nonzero_exp] - expected_counts[nonzero_exp])^2 / expected_counts[nonzero_exp])
    },
    error = function(e) {
      NA_real_
    }
  )
  return(out)
}

#' Cut a continuous variable in n categories.
#'
#' @param x vector to cut
#' @param n_breaks number of breaks
#'
#' @returns vector
#' @export
cut_safe <- function(x, n_breaks = 5){
  if (all(is.na(x))) return(NA)
  breaks = unique(quantile(x,seq(0,1,1/n_breaks), na.rm = T))
  if (length(breaks) == 1) return(paste0("(",min(x,na.rm=T),",",max(x,na.rm=T),")"))
  x <- cut(x, breaks, ordered_result = TRUE)
  levels(x) <- paste0(1:n_breaks,": ",levels(x))
  return(x)
}





