#' calculate isoforest score from df
#'
#' @param df database
#'
#' @returns vector
#' @export
#'
#' @examples
#' score_isoforest(iris[sapply(iris, is.numeric)])
score_isoforest <- function(df) {
  df_scaled <- df %>%
    ungroup() %>%
    lapply(scale) %>%
    as.data.frame()

  if (nrow(df_scaled) < 5) {
    return(NA)
  }

  set.seed(123)
  iso_model <- isotree::isolation.forest(df_scaled, ntrees = 1000, ndim = 1)
  scores <- predict(iso_model, newdata = df_scaled, type = "score")

  scores
}
