prepa_ranking <- function(df_stats_intvwr,var_rank_main,var_rank_sub,filt){
  
  df_count <- df_stats_intvwr() %>%
    filter(stat %in% c("chi2","median")) %>%
    group_by(!!sym(var_rank_main)) %>%
    count(name="N_outliers")
  
  df_prepa <- df_stats_intvwr() %>%
    filter(stat %in% c("chi2","median")) %>%
    select(!!sym(var_rank_main),!!sym(var_rank_sub),standard,Nrow) %>%
    pivot_wider(
      id_cols = c(!!sym(var_rank_main),Nrow),
      names_from = var_rank_sub,
      values_from = standard
    )
  
  df_prepa %>%
    mutate(score = score_isoforest(across(where(is.numeric))),
           score = round(score*100,1)) %>%
    ungroup() %>%
    left_join(df_count) %>%
    arrange(desc(score))
}

prepa_listing <- function(prepa_ranking,var_rank_main,value_rank_main,var_rank_sub){
  prepa_ranking() %>%
    filter(!!sym(var_rank_main) == value_rank_main) %>%
    select(-!!sym(var_rank_main),-Nrow,-N_outliers,-score) %>%
    pivot_longer(cols = where(is.numeric),
                 names_to = var_rank_sub,
                 values_to = "diff") %>%
    mutate(diff = round(diff,1)) %>%
    arrange(desc(abs(diff)))
}

count_auto <- function(data,
                       group_var,
                       var,
                       type         = c("auto", "continuous", "categorical"),
                       n_bins       = 5,
                       distinct_min = 10,
                       dig_lab      = 5) {
  
  type <- match.arg(type)
  stopifnot(is.character(group_var), is.character(var),
            group_var %in% names(data))
  
  if (!var %in% names(data)) return(NULL)
  
  x <- data[[var]]
  
  is_continuous <- switch(
    type,
    continuous  = TRUE,
    categorical = FALSE,
    auto        = is.numeric(x) &&
      dplyr::n_distinct(x, na.rm = TRUE) > distinct_min
  )
  
  if (is_continuous) {
    breaks <- unique(stats::quantile(
      x, probs = seq(0, 1, length.out = n_bins + 1),
      na.rm = TRUE, type = 7
    ))
    if (length(breaks) < 3L) {
      warning("Too few distinct values to divide ", var," ; used as categorical.")
      data <- dplyr::mutate(data, .cls = .data[[var]])
    } else {
      data <- data %>% 
        dplyr::mutate(
          .cls = cut(.data[[var]], breaks = breaks, include.lowest = TRUE,
                     ordered_result = TRUE, dig.lab = dig_lab),
          .cls = gsub(","," , ",.cls)
        )
    }
  } else {
    data <- dplyr::mutate(data, .cls = .data[[var]])
  }
  
  g <- rlang::sym(group_var)
  
  data %>%
    dplyr::count(!!g, .cls, name = "n") %>%
    dplyr::arrange(.cls) %>% 
    dplyr::rename(!!var := .cls)
}

#### df to DT avec mise en forme ####

df_to_formated_dt <- function(df,
                              var_rank = "RANK",
                              var_quanti,
                              var_outliers){
  
  stats <- lapply(var_outliers, function(num_col) {
    x <- df[[num_col]]
    m <- mean(x, na.rm = TRUE)
    s <- sd(x,   na.rm = TRUE)
    list(mean = m, sd = s, cutoff = m + 2*s)
  })
  names(stats) <- var_outliers
  
  dt <- datatable(df, filter = "top", selection = "single",
                  rownames = FALSE, 
                  options = list(pageLength = 10, dom = "tp"))
  
  for(col in var_outliers) {
    cutoff <- stats[[col]]$cutoff
    dt <- dt %>% formatStyle(col, backgroundColor = styleInterval(cutoff,c("","#FF000060")))
  }
  
  for(col in var_quanti) {
    dt <- dt %>% 
      formatStyle(
        col,
        background = styleColorBar(df[[col]], 'steelblue'),
        backgroundSize = '90% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  }
  
  dt
}

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

tidy_to_dt <- function(tidy_db, sensibility = 0.05,
                       group_rows = TRUE, drop_inds = character(0)) {
  
  colnames(tidy_db) <- colnames(tidy_db) %>% stringr::str_remove("^value\\|")
  vars <- colnames(tidy_db)
  
  meta_cols  <- intersect(c("variable", "type"), vars)
  sd_cols    <- vars[stringr::str_starts(vars, "sd\\|")]
  value_cols <- setdiff(vars, c(meta_cols, sd_cols))
  
  grp_of <- function(x) stringr::str_split_fixed(x, "\\|", 2)[, 1]
  ind_of <- function(x) stringr::str_split_fixed(x, "\\|", 2)[, 2]
  
  # équivalent de cols_hide(ends_with("mean"), ...) : on retire proprement
  if (length(drop_inds)) {
    drop_v  <- value_cols[ind_of(value_cols) %in% drop_inds]
    drop_sd <- sd_cols[ind_of(stringr::str_remove(sd_cols, "^sd\\|")) %in% drop_inds]
    tidy_db    <- tidy_db[, setdiff(vars, c(drop_v, drop_sd)), drop = FALSE]
    value_cols <- setdiff(value_cols, drop_v)
    sd_cols    <- setdiff(sd_cols, drop_sd)
  }
  
  # ---- cas vide ----
  if (nrow(tidy_db) == 0) {
    return(DT::datatable(data.frame(NULL), rownames = FALSE, escape = FALSE, options = list(dom = "t")))
  }
  
  df <- tidy_db
  if ("type" %in% names(df))
    df$type <- dplyr::recode(df$type, cha = "Categorial",
                             num = "Continuous", .default = "Error")
  
  # pictos presence
  pic <- function(x) ifelse(x == 1,
                            "<span style='color:green;font-weight:bold'>\u2714\ufe0f</span>",
                            "<span style='color:red;font-weight:bold'>\u274c</span>")
  presence_cols <- value_cols[stringr::str_ends(value_cols, "presence")]
  for (col in presence_cols) df[[col]] <- pic(df[[col]])
  
  # ordonner les value_cols par groupe -> spanners contigus
  groups     <- unique(grp_of(value_cols))
  value_cols <- unlist(lapply(groups, function(g) value_cols[grp_of(value_cols) == g]))
  col_order  <- c(meta_cols, value_cols, sd_cols)
  df         <- df[, col_order, drop = FALSE]
  pos        <- function(n) match(n, col_order) - 1L      # index 0-based
  
  # en-tête 2 niveaux : spanner = groupe / sous-titre = indicateur
  top <- list()
  for (m in meta_cols)       top <- c(top, list(htmltools::tags$th(rowspan = 2, m)))
  for (g in groups)          top <- c(top, list(htmltools::tags$th(
    colspan = sum(grp_of(value_cols) == g), g)))
  for (s in sd_cols)         top <- c(top, list(htmltools::tags$th(rowspan = 2, s)))
  bottom    <- lapply(ind_of(value_cols), function(i) htmltools::tags$th(i))
  container <- htmltools::tags$table(class = "display",
                                     htmltools::tags$thead(htmltools::tags$tr(top),
                                                           htmltools::tags$tr(bottom)))
  
  # cacher type (porté par RowGroup) + colonnes sd ; "-" pour les manquants
  hide_meta <- if (group_rows) intersect("type", meta_cols) else character(0)
  coldefs <- list(
    list(visible = FALSE, targets = pos(c(hide_meta, sd_cols))),
    list(defaultContent = "-", targets = "_all"))
  
  opts <- list(pageLength = 15, columnDefs = coldefs, dom = 'tp',
               rowGroup = if (group_rows && "type" %in% meta_cols) 
                 list(dataSrc = pos("type")))
  exts <- if (group_rows && "type" %in% meta_cols) "RowGroup" else character(0)
  
  dt <- DT::datatable(df, rownames = FALSE, container = container, options = opts, 
                      escape = FALSE, filter = "top",selection = 'single')
  
  # coloration de chaque case selon SON sd correspondant
  for (vc in value_cols) {
    sdc <- paste0("sd|", vc)
    if (sdc %in% sd_cols)
      dt <- dt %>% 
        DT::formatStyle(vc, valueColumns = sdc,
                        backgroundColor = DT::styleInterval(
                          c(-sensibility, sensibility),
                          c("#e23b3b", "white", "#e23b3b")),
                        color           = DT::styleInterval(
                          c(-sensibility, sensibility),
                          c("white", "inherit", "white")))
  }
  if (length(presence_cols))
    dt <- dt %>% DT::formatStyle(presence_cols, backgroundColor = "white")
  
  # formats numériques
  int_cols <- value_cols[ind_of(value_cols) %in% c("Nmod")]
  round_cols <- value_cols[ind_of(value_cols) %in% c("mean","median","sd","khi2","Vcramer")]
  pct_cols   <- value_cols[ind_of(value_cols) == "missing"]
  if (length(int_cols))   dt <- dt %>% DT::formatRound(int_cols, digits = 0)
  if (length(round_cols)) dt <- dt %>% DT::formatRound(round_cols, digits = 2)
  if (length(pct_cols))   dt <- dt %>% DT::formatPercentage(pct_cols, digits = 2)

  dt
}

presence_check_dt <- function(pres) {
  if (nrow(pres) == 0)
    return(DT::datatable(
      data.frame(NULL),
      rownames = FALSE, escape = FALSE, options = list(dom = "t")))
  
  long <- pres %>%
    dplyr::select(variable, dplyr::matches("^value\\|.*\\|presence$")) %>%
    tidyr::pivot_longer(
      cols = -variable,
      names_to = "wave",
      names_pattern = "^value\\|(.*)\\|presence$",   # capture la vague
      values_to = "present") %>%
    dplyr::mutate(
      absent = is.na(present) | present != 1,
      mark = ifelse(absent,
                    sprintf("%s <span style='color:#d73027;font-weight:bold'>\u2716</span>", variable),
                    sprintf("%s <span style='color:#1a9850;font-weight:bold'>\u2714</span>", variable)))
  
  wide <- long %>%
    dplyr::arrange(dplyr::desc(absent), variable) %>%      # absentes d'abord
    dplyr::group_by(wave) %>%
    dplyr::summarise(vars = paste(mark, collapse = ", "), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = wave, values_from = vars, names_sort = TRUE)
  
  DT::datatable(wide, rownames = FALSE, escape = FALSE,
                options = list(dom = "t", ordering = FALSE))
}

#### Comparison of distributions ####

plot_compa_distributions <- function(prepa_distrib, intvwr, variable, vg,
                                     type = c("auto", "categorical", "continuous"),
                                     trim = c(0.01, 0.99),   # NULL pour désactiver
                                     adjust = 1) {           # lissage densité
  type <- match.arg(type)
  title_intvwr <- paste("Distribution for", vg, "=", intvwr)
  
  # Construction des groupes
  base <- prepa_distrib %>%
    mutate(.group = !!sym(vg) == intvwr) %>%
    tidyr::replace_na(list(.group = FALSE)) %>%
    mutate(.group_lbl = factor(ifelse(.group, "Groupe ciblé", "Référence"),
                               levels = c("Référence", "Groupe ciblé")))
  
  # Détection auto
  if (type == "auto") {
    x <- base[[variable]]
    type <- if (is.numeric(x) && dplyr::n_distinct(x, na.rm = TRUE) > 15)
      "continuous" else "categorical"
  }
  
  cols <- c("Référence" = "grey80", "Groupe ciblé" = "#2c5f7a")
  
  # BRANCHE CATÉGORIELLE
  if (type == "categorical") {
    syn <- base %>%
      mutate(!!sym(variable) := as.character(!!sym(variable))) %>%
      count(.group, !!sym(variable)) %>%
      group_by(.group) %>% mutate(prop = n / sum(n)) %>% ungroup()
    
    return(
      ggplot() +
        geom_bar(data = filter(syn, !.group),
                 aes(x = !!sym(variable), y = prop),
                 stat = "identity", fill = "grey80", width = 0.8, alpha = 0.5) +
        geom_bar(data = filter(syn, .group),
                 aes(x = !!sym(variable), y = prop),
                 stat = "identity", fill = "#2c5f7a", width = 0.4) +
        geom_text(data = filter(syn, .group),
                  aes(x = !!sym(variable), y = prop,
                      label = scales::percent(prop, accuracy = 1)),
                  hjust = -0.2, size = 4, color = "#2c5f7a") +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1.05),
                           expand = c(0, 0)) +
        coord_flip() +
        theme_minimal(base_size = 15) +
        labs(x = variable, y = "Proportion", fill = NULL, title = title_intvwr) +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y = element_text(face = "bold"),
              plot.title.position = "plot",
              legend.position = "bottom")
    )
  }
  
  # BRANCHE CONTINUE
  syn <- base %>% filter(!is.na(!!sym(variable)))
  
  # Découpage des outliers sur quantiles communs (mêmes bornes pour les 2 groupes)
  if (!is.null(trim)) {
    bornes <- quantile(syn[[variable]], probs = trim, na.rm = TRUE)
    syn <- syn %>%
      filter(dplyr::between(!!sym(variable), bornes[1], bornes[2]))
  }
  
  medians <- syn %>%
    group_by(.group_lbl) %>%
    summarise(med = median(!!sym(variable)), .groups = "drop")
  
  ggplot(syn, aes(x = !!sym(variable),
                  fill = .group_lbl, color = .group_lbl)) +
    geom_density(alpha = 0.5, adjust = adjust, linewidth = 0.6) +
    geom_vline(data = medians,
               aes(xintercept = med, color = .group_lbl),
               linetype = "dashed", linewidth = 0.7, show.legend = FALSE) +
    scale_fill_manual(values = cols) +
    scale_color_manual(values = c("Référence" = "grey55",
                                  "Groupe ciblé" = "#2c5f7a")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_minimal(base_size = 15) +
    labs(x = variable, y = "Densité", fill = NULL, color = NULL,
         title = title_intvwr) +
    theme(panel.grid.minor = element_blank(),
          plot.title.position = "plot",
          legend.position = "bottom")
}


format_duree <- function(d) {
  print(d)
  signe <- ifelse(d < 0, "-", "")
  d <- abs(d*60)
  
  jours   <- as.integer(d %/% ddays(1))
  heures  <- as.integer((d %% ddays(1)) %/% dhours(1))
  minutes <- as.integer((d %% dhours(1)) %/% dminutes(1))
  
  # Coller les morceaux sous forme "2j 10h 20m"
  res <- paste0(signe,
                ifelse(jours > 0,paste0(jours, "j"),
                       ifelse(heures > 0, paste0(heures, "h "),
                              ifelse(minutes > 0, paste0(minutes, "m"),
                                     paste0(d,"s"))
                       )))
  
  res[res=="NANA"] <- ""
  res[res=="NANANANA"] <- ""
  
  trimws(res)  # pour enlever les espaces en trop
}
