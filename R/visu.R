#' Create a heatmap
#'
#' @param df_stats data frame from prepa_stats function
#' @param threshold threshold to show difference
#' @param color color of the cells
#'
#' @returns heatmap (ggplot)
#' @export
#'
#' @examples
#' library(laeken)
#' data(eusilc)
#'
#' df_stats <- prepa_stats(eusilc, "db040")
#' heatmap_group(df_stats, 5)
heatmap_group <- function(df_stats, threshold = 5, color = "red2") {
  stopifnot(is.data.frame(df_stats))
  stopifnot(is.numeric(threshold))
  stopifnot(threshold >= 0)

  var_group <- colnames(df_stats)[1]

  df_stats <- df_stats %>%
    tidyr::complete(!!sym(var_group), variable,
                    fill=list(Nrow=0,Nval=0,standard=0,type ="missing")) %>%
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
        "Group : ", !!sym(var_group), "\n",
        "Number of rows : ", Nrow, "\n",
        "Number of valid row : ", Nval, "\n",
        stat_standard
      )
    ) %>%
    mutate(standard = ifelse(standard == 0,NA,standard))

  threshold <- max(threshold, 0.01)
  tmax <- max(threshold * 2, 2)

  p <- ggplot(df_stats) +
    aes(variable, !!sym(var_group), fill = standard, text = info) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1) +
    scale_fill_gradientn(
      na.value = "white", colours = c("white", color),
      limits = c(threshold, tmax * 2),
      oob = scales::squish
    ) +
    theme_minimal() +
    labs(x = "") +
    theme(
      panel.background = element_rect(fill = "white"),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
      panel.grid = element_blank()
    )

  p
}
