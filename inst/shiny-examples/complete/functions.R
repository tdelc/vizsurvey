#### Correlation ####

group_main_corr <- function(M, variable) {
  df_strip <- tibble::tibble(
    var = colnames(M),
    corr = as.numeric(M[variable, ])
  )

  df_strip$var <- factor(df_strip$var, ordered = TRUE, levels = df_strip$var)

  ggplot(df_strip, aes(x = var, y = "", fill = corr)) +
    geom_tile() +
    geom_text(aes(label = ifelse(
      is.na(corr), "?", scales::number(corr, accuracy = 0.01)
    ))) +
    scale_x_discrete(position = "top") +
    scale_fill_gradient2(limits = c(-1, 1), na.value = "white") +
    labs(x = NULL, y = NULL, fill = "corr") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.margin = margin(t = 0, r = 2, b = 0, l = 2, unit = "cm")
    )
}


### GT ####

tidy_to_gt <- function(tidy_db, sensibility = 0.05) {
  colnames(tidy_db) <- colnames(tidy_db) %>% stringr::str_remove("^value\\|")

  vars <- colnames(tidy_db)
  indicators <- vars[stringr::str_starts(vars, "sd\\|.*?\\|")] %>%
    stringr::str_remove("sd\\|.*?\\|") %>%
    unique()

  group <- vars[stringr::str_starts(vars, "sd\\|.*?\\|")] %>%
    stringr::str_extract(".*?\\|(.*?)\\|.*", group = 1) %>%
    unique()

  hide <- vars[stringr::str_starts(vars, "sd")] %>% unique()

  specs <- lapply(indicators, function(ind) {
    list(
      sd_col = paste0("sd|", group[1], "|", ind),
      value_cols = paste0(group, "|", ind)
    )
  })

  if (nrow(tidy_db) == 0) {
    return(
      tibble(INFO = "No data, probable causes : <br/>
         - Only one wave <br/>
         - No outlier at this level of sensitivity") %>%
        mutate(INFO = gt::html(INFO)) %>%
        gt::gt() %>% gt::fmt_markdown() %>% gt::cols_align("left")
    )
  }

  gt_table <- tidy_db %>%
    mutate(type = case_when(
      type == "cha" ~ "Categorial",
      type == "num" ~ "Continuous",
      TRUE ~ "Error"
    )) %>%
    gt::gt(rowname_col = "variable", groupname_col = "type")

  for (s in specs) {
    sd_col_sym <- sym(s$sd_col)
    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(cell_fill(color = "red2"), cell_text(color = "white")),
        locations = cells_body(
          columns = all_of(s$value_cols),
          rows = abs(!!sd_col_sym) > sensibility
        )
      )
  }

  gt_table %>%
    gt::cols_hide(columns = hide) %>%
    gt::tab_spanner_delim(delim = "|") %>%
    gt::fmt_number(
      columns = vars[stringr::str_ends(
        vars, "\\|Nmod|\\|mean|\\|median|\\|sd|\\|khi2")],
      decimals = 2,
      drop_trailing_zeros = TRUE,
      suffixing = TRUE
    ) %>%
    gt::fmt_percent(
      columns = vars[stringr::str_ends(vars, "\\|missing")],
      decimals = 2,
      drop_trailing_zeros = TRUE
    ) %>%
    gt::sub_missing(
      missing_text = "-"
    ) %>%
    gt::text_transform(
      locations = cells_body(
        columns = vars[stringr::str_ends(vars, "presence")]
      ),
      fn = function(x) {
        ifelse(x == 1,
          "<span style='color:green;font-weight:bold'>✔️</span>",
          "<span style='color:red;font-weight:bold'>❌</span>"
        )
      }
    ) %>%
    gt::tab_style(
      style = list(cell_fill(color = "white")),
      locations = cells_body(
        columns = vars[stringr::str_ends(vars, "presence")]
      )
    )
}
