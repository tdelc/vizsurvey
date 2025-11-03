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
list_dist <- function(df,vars_vd){
  list_dist <- vars_vd %>% map(~{
    expected_prop <- prop.table(table(df[[.x]], useNA = "ifany"))
    names(expected_prop)[which(is.na(names(expected_prop)))] <- "NA_"
    expected_prop <- tibble(category = names(expected_prop), prop = expected_prop) %>%
      mutate(category = ifelse(prop < 0.01, "OTH_", category)) %>%
      group_by(category) %>%
      summarise(prop = sum(prop)) %>%
      ungroup()
    setNames(expected_prop$prop, expected_prop$category)
  })
  names(list_dist) <- vars_vd
  return(list_dist)
}

#' Specific chisq test to NA and Other modality
#'
#' @param x value to procede chisq test
#' @param varname name of the variable
#' @param ldist named list of expected probability
#'
#' @returns chisq value
#' @export
#'
#' @examples
#' ldist <- list_dist(mtcars,c("cyl","gear"))
#' sub_mtcars <- subset(mtcars,vs == 1)
#' my_chisq_test(sub_mtcars$cyl,"cyl",ldist)
my_chisq_test <- function(x, varname, ldist) {
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
      t_chi <- stats::chisq.test(obs, p = exp_prop)
      t_chi$statistic
    },
    error = function(e) {
      NA_real_
    }
  )
  return(out)
}





