### Graphics ####

multi_plots <- function(loop, facet, group, db, h = "400px") {
  lapply(loop, function(i) {
    plot_name_trans <- paste0(i, "_plot_domain")
    plot_name_evo <- paste0(i, "_plot_evol")
    if (i %in% c(facet, group)) {
      list(tags$br())
    } else {
      list(
        fluidRow(
          column(10, h4(i))
        ),
        fluidRow(
          column(6, plotOutput(plot_name_trans, height = h)),
          column(6, plotOutput(plot_name_evo, height = h))
        ), tags$br()
      )
    }
  })
}

#### Shiny distribution ####

shiny_distrib_discrete <- function(db, quali_var, facet = NULL, filter_exp = T,
                                   group = NULL, export_path = NULL,
                                   pal = "sienna2", ...) {
  if (!is.null(facet) && quali_var == facet) {
    return(NULL)
  }
  if (!is.null(group) && quali_var == group) {
    return(NULL)
  }

  if (is.null(facet)) facet <- NULL else facet <- as.name(facet)
  if (filter_exp == TRUE) {
    sub_db <- db
  } else {
    sub_db <- db %>% filter(!!(parse_expr(filter_exp)))
  }

  nb_row <- nrow(sub_db)
  indic_na <- is.na(sub_db[, quali_var]) | str_trim(sub_db[, quali_var]) == ""
  nb_mis <- sum(indic_na)
  pc_mis <- round(100 * mean(indic_na))

  filter_exp <- parse(text = filter_exp)[[1]]

  if (!is.null(group)) {
    eval(substitute(
      distrib_group_discrete(
        data = db,
        group = VAR1,
        quali_var = VAR0,
        facet = VAR2,
        na.rm.group = FALSE,
        na.rm.var = FALSE,
        na.rm.facet = FALSE,
        filter_exp = VAR3,
        export_path = export_path,
        title = paste0(
          "N rows : ", nb_row, " (",
          nb_mis, " missings, ", pc_mis, "%)"
        )
      ), list(
        VAR0 = as.name(group),
        VAR1 = as.name(quali_var),
        VAR2 = facet,
        VAR3 = filter_exp
      )
    ))
  } else {
    eval(substitute(
      distrib_discrete(
        data = db,
        quali_var = VAR1,
        facet = VAR2,
        na.rm.var = FALSE,
        na.rm.facet = FALSE,
        filter_exp = VAR3,
        pal = pal,
        export_path = export_path,
        title = paste0(
          "N rows : ", nb_row, " (",
          nb_mis, " missings, ", pc_mis, "%)"
        )
      ), list(
        VAR1 = as.name(quali_var),
        VAR2 = facet,
        VAR3 = filter_exp
      )
    ))
  }
}

shiny_distrib_continuous <- function(db, quanti_exp, facet = NULL,
                                     filter_exp = TRUE, group = NULL, type,
                                     pal = c("#00708C", "mediumturquoise"),
                                     export_path = NULL, ...) {
  if (is.null(facet)) facet <- NULL else facet <- as.name(facet)
  if (filter_exp == TRUE) {
    sub_db <- db
  } else {
    sub_db <- db %>% filter(!!(parse_expr(filter_exp)))
  }
  nb_row <- nrow(sub_db)
  nb_mis <- sum(is.na(sub_db[, quanti_exp]))
  pc_mis <- round(100 * mean(is.na(sub_db[, quanti_exp])))

  filter_exp <- parse(text = filter_exp)[[1]]

  if (!is.null(group)) {
    eval(substitute(
      distrib_group_continuous(
        data = db,
        group = VAR0,
        quanti_exp = VAR1,
        facet = VAR2,
        na.rm.group = FALSE,
        na.rm.facet = FALSE,
        filter_exp = VAR3,
        type = type,
        pal = pal,
        export_path = export_path,
        title = paste0(
          "N rows : ", nb_row, " (",
          nb_mis, " missings, ", pc_mis, "%)"
        )
      ), list(
        VAR0 = as.name(group),
        VAR1 = as.name(quanti_exp),
        VAR2 = facet,
        VAR3 = filter_exp
      )
    ))
  } else {
    eval(substitute(
      distrib_continuous(
        data = db,
        quanti_exp = VAR1,
        facet = VAR2,
        na.rm.facet = FALSE,
        filter_exp = VAR3,
        type = type,
        pal = pal,
        export_path = export_path,
        title = paste0(
          "N rows : ", nb_row, " (",
          nb_mis, " missings, ", pc_mis, "%)"
        )
      ), list(
        VAR1 = as.name(quanti_exp),
        VAR2 = facet,
        VAR3 = filter_exp
      )
    ))
  }
}


#### Heatmap ####

heat_map_itw <- function(db_stat, threshold) {
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
        "Itw : ", !!sym(var_group), "\n",
        "N rows : ", Nrow, "\n",
        "Valid N rows : ", Nval, "\n",
        stat_standard
      )
    )

  p <- ggplot(db_stat) +
    aes(variable, !!sym(var_group), fill = standard, text = info) +
    geom_tile(color = "white", lwd = 1.5, linetype = 1) +
    scale_fill_gradientn(
      na.value = "cyan3", colours = c("green2", "white", "white", "red2"),
      values = scales::rescale(c(
        -threshold * 2, -threshold,
        threshold, threshold * 2
      )),
      limits = c(-threshold * 2, threshold * 2),
      oob = scales::squish
    ) +
    theme_minimal() +
    theme_fonctionr() +
    labs(x = "") +
    theme(
      panel.background = element_rect(fill = "grey80"),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),
      panel.grid = element_blank()
    )

  p
}

#### Correlation ####

itw_main_corr <- function(M, variable) {
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
      axis.text.x = element_text(angle = 45, hjust = -0.1),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      plot.margin = margin(t = 0, r = 2, b = 0, l = 2, unit = "cm")
    )
}


### GT ####

tidy_to_gt <- function(tidy_db, sensibility = 0.05) {
  colnames(tidy_db) <- colnames(tidy_db) %>% str_remove("^value\\|")

  vars <- colnames(tidy_db)
  indicators <- vars[str_starts(vars, "sd\\|.*?\\|")] %>%
    str_remove("sd\\|.*?\\|") %>%
    unique()

  group <- vars[str_starts(vars, "sd\\|.*?\\|")] %>%
    str_extract(".*?\\|(.*?)\\|.*", group = 1) %>%
    unique()

  hide <- vars[str_starts(vars, "sd")] %>% unique()

  specs <- lapply(indicators, function(ind) {
    list(
      sd_col = paste0("sd|", group[1], "|", ind),
      value_cols = paste0(group, "|", ind)
    )
  })

  if (nrow(tidy_db) == 0) {
    return(
      tibble(INFO = "No data, probable causes : <br/>
         - Only one domain <br/>
         - No outlier at this level of sensitivity") %>%
        mutate(INFO = gt::html(INFO)) %>%
        gt() %>% fmt_markdown() %>% cols_align("left")
    )
  }

  gt_table <- tidy_db %>%
    mutate(type = case_when(
      type == "cha" ~ "Categorial",
      type == "num" ~ "Continuous",
      TRUE ~ "Error"
    )) %>%
    gt(rowname_col = "variable", groupname_col = "type")

  for (s in specs) {
    sd_col_sym <- sym(s$sd_col)
    gt_table <- gt_table %>%
      tab_style(
        style = list(cell_fill(color = "red2"), cell_text(color = "white")),
        locations = cells_body(
          columns = all_of(s$value_cols),
          rows = abs(!!sd_col_sym) > sensibility
        )
      )
  }

  gt_table %>%
    cols_hide(columns = hide) %>%
    tab_spanner_delim(delim = "|") %>%
    fmt_number(
      columns = vars[str_ends(vars, "\\|Nmod|\\|mean|\\|median|\\|sd|\\|khi2")],
      decimals = 2,
      drop_trailing_zeros = TRUE,
      suffixing = TRUE
    ) %>%
    fmt_percent(
      columns = vars[str_ends(vars, "\\|missing")],
      decimals = 2,
      drop_trailing_zeros = TRUE
    ) %>%
    sub_missing(
      missing_text = "-"
    ) %>%
    text_transform(
      locations = cells_body(
        columns = vars[str_ends(vars, "presence")]
      ),
      fn = function(x) {
        ifelse(x == 1,
          "<span style='color:green;font-weight:bold'>✔️</span>",
          "<span style='color:red;font-weight:bold'>❌</span>"
        )
      }
    ) %>%
    tab_style(
      style = list(cell_fill(color = "white")),
      locations = cells_body(
        columns = vars[str_ends(vars, "presence")]
      )
    )
}
