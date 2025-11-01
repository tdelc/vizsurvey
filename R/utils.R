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
