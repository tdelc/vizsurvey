#' Create a heatmap
#'
#' @param db_stat data frame from preparation_stats function
#' @param threshold threshold to show difference
#'
#' @returns heatmap (ggplot)
#' @export
#'
#' @examples
#' library(laeken)
#' data(eusilc)
#'
#' info_vars <- classify_variables(eusilc)
#' vars_vd <- info_vars[info_vars$type == "Modal", ]$variable
#' vars_vc <- info_vars[info_vars$type == "Continuous", ]$variable
#' db_stats <- prepa_stats(eusilc, vars_vd, vars_vc, "db040")
#' heat_map_group(db_stats, 5)
heat_map_group <- function(db_stat, threshold) {
  stopifnot(is.data.frame(db_stat))
  stopifnot(is.numeric(threshold))
  stopifnot(threshold >= 0)

  var_group <- colnames(db_stat)[1]

  db_stat <- db_stat %>%
    mutate(stat_standard = paste(stat, round(standard, 2), sep = " : ")) %>%
    group_by(!!sym(var_group), variable, Nrow, Nval, type) %>%
    summarise(
      standard = max(abs(standard)),
      stat_standard = paste(stat_standard, collapse = "\n")
    ) %>%
    ungroup() %>%
    mutate(
      info = paste0(
        "Variable : ", variable, " (", type, ")\n",
        "Group : ", var_group, "\n",
        "Number of rows : ", Nrow, "\n",
        "Number of valid row : ", Nval, "\n",
        stat_standard
      )
    )

  threshold <- max(threshold, 0.01)
  tmax <- max(threshold * 2, 2)

  p <- ggplot(db_stat) +
    aes(variable, !!sym(var_group), fill = standard, text = info) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1) +
    scale_fill_gradientn(
      na.value = "cyan3", colours = c("red2", "white", "white", "red2"),
      values = scales::rescale(c(-tmax, -threshold, threshold, tmax)),
      limits = c(-tmax * 2, tmax * 2),
      oob = scales::squish
    ) +
    theme_minimal() +
    labs(x = "") +
    theme(
      panel.background = element_rect(fill = "grey80"),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
      panel.grid = element_blank()
    )

  p
}
