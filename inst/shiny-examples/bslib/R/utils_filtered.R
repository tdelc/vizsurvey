# ============================================================================
# utils_filtered.R
# Couche "données filtrées". Pas d'UI -> pas un module, juste une fonction qui
# assemble des reactives à partir de `data` (mod_source) et `sel` (mod_filters).
# Appelée UNE fois dans app_server, le résultat est passé aux onglets.
# ============================================================================

# Helper pur : filtre wave + filter Factorise le motif dupliqué partout.
filter_wave_zone <- function(df, config, var_wave, var_filter) {
  out <- dplyr::filter(df, !!rlang::sym(config$var_wave) %in% var_wave)
  if (length(config$var_filter) && var_filter %in% dplyr::pull(out[, config$var_filter])) {
    out <- dplyr::filter(out, !!rlang::sym(config$var_filter) == var_filter)
  }
  out
}

# Retourne une liste de reactives prêts à consommer dans les onglets.
#   data : sortie de mod_source_server()  (df, df_stats, df_stats_group, config, timer, path_survey)
#   sel  : sortie de mod_filters_server() (wave, filter, wave_compare)
survey_filtered <- function(data, sel) {
  
  cfg <- reactive(data$config())

  df <- reactive({
    req(data$df(), data$config()$var_wave)
    df <- filter_wave_zone(data$df(), data$config(), sel$wave(), sel$filter())
    df
  })

  df_stats_wave <- reactive({
    df <- data$df_stats_wave()
    if (is.null(df)) return(NULL)
    df %>% dplyr::filter(
      filter == sel$filter()
    )
  })

  df_stats_intvwr <- reactive({
    req(data$df_stats_intvwr())
    data$df_stats_intvwr() %>% 
      dplyr::filter(!!rlang::sym(data$config()$var_wave) == sel$wave(),
                    filter == sel$filter())
  })
  
  df_var_ranking <- reactive({
    req(df_stats_intvwr())
    
    out_cha <- df_stats_intvwr() %>%
      filter(stat == "chi2") %>% mutate(median=NA) %>% 
      select(!!sym(cfg()$var_intvwr),Nrow,Nval,variable,chi2=value,median,standard)
    
    out_num <- df_stats_intvwr() %>%
      filter(stat == "median") %>% mutate(chi2=NA) %>% 
      select(!!sym(cfg()$var_intvwr),Nrow,Nval,variable,chi2,median=value,standard)
    
    out <- out_cha %>% tibble::add_row(out_num) %>% 
      mutate(chi2 = round(chi2,1),
             median = round(median,1),
             standard = round(standard,1)) %>%
      select(!!sym(cfg()$var_intvwr),Nrow,Nval,variable,chi2,median,standard) %>%
      arrange(desc(abs(standard)))
    
    out <- out %>%
      dplyr::filter(Nrow >= sel$threshold_Nrow(),
                    Nval >= sel$threshold_Nval(),
                    abs(standard) >= sel$threshold_intvwr())
    
    return(out)
  })

  df_timer_intv <- reactive({
    tmr <- data$timer()
    req(isTRUE(tmr$ready), tmr$df_timer_intv)
    
    vec_intv <- df() %>% pull(!!sym(data$timer()$cfg$var_intv))
          
    out <- tmr$df_timer_intv %>%
      dplyr::filter(!!rlang::sym(data$config()$var_wave) == sel$wave()) %>% 
      dplyr::filter(!!rlang::sym(data$timer()$cfg$var_intv) %in% vec_intv)
    out
  })

  # Détail timer (parquet lazy) — dépend du wave + path, pas du filtre df
  df_timer_detail <- reactive({
    path <- data$path_survey()
    name_file <- paste0("timers_", sel$wave(), ".parquet")
    req(file.exists(file.path(path, name_file)))
    arrow::open_dataset(file.path(path, name_file))
  })

  list(
    df                = df,
    df_stats_wave     = df_stats_wave,
    df_stats_intvwr   = df_stats_intvwr,
    df_var_ranking    = df_var_ranking,
    df_timer_intv     = df_timer_intv,
    df_timer_detail   = df_timer_detail,
    threshold_Nrow    = sel$threshold_Nrow,
    threshold_Nval    = sel$threshold_Nval,
    threshold_intvwr  = sel$threshold_intvwr
  )
}
