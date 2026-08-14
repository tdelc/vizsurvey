# ============================================================================
# mod_summary.R  — onglet "Summary" (premier onglet, sélectionné au démarrage)
#
# Deux rôles :
#   1. présenter l'enquête chargée (volumétrie, vagues, rôle des variables)
#   2. donner trois points d'entrée chiffrés vers les onglets d'analyse
#
# Consomme : data (df, config, path_survey, timer), filt (df_var_ranking,
#            df_stats_wave, seuils), sel (wave, wave_level, filter), opts,
#            main_session (pour changer d'onglet depuis les boutons)
# ============================================================================

mod_summary_ui <- function(id, i18n) {
  ns <- NS(id)
  nav_panel(
    title = tagList(bs_icon("house-door-fill"), i18n$t("Summary")),
    value = "tab_summary",
    
    uiOutput(ns("no_data_msg")),
    uiOutput(ns("boxes")),
    
    layout_columns(
      col_widths = c(5, 7),
      card(
        id = ns("card_survey"),
        full_screen = TRUE,
        icon_header("info-circle", i18n$t("Survey"), help = ns("card_survey")),
        uiOutput(ns("survey_table"))
      ),
      card(
        id = ns("card_waves"),
        full_screen = TRUE,
        icon_header("bar-chart-line", i18n$t("Interviews per wave"),
                    help = ns("card_waves")),
        plotOutput(ns("plot_waves"), height = "280px")
      )
    ),
    
    h2(class = "mt-4", i18n$t("Where to start")),
    div(class = "small text-muted mb-2", textOutput(ns("scope"))),
    
    layout_columns(
      col_widths = c(4, 4, 4),
      card(
        id = ns("card_intvwr"),
        icon_header("person-vcard-fill", i18n$t("Interviewer"),
                    help = ns("card_intvwr")),
        uiOutput(ns("teaser_intvwr")),
        card_footer(actionButton(ns("go_intvwr"), i18n$t("Open the tab"),
                                 class = "btn-sm btn-primary"))
      ),
      card(
        id = ns("card_wave"),
        icon_header("water", i18n$t("Wave"), help = ns("card_wave")),
        uiOutput(ns("teaser_wave")),
        card_footer(actionButton(ns("go_wave"), i18n$t("Open the tab"),
                                 class = "btn-sm btn-primary"))
      ),
      card(
        id = ns("card_cross"),
        icon_header("crosshair", i18n$t("Cross"), help = ns("card_cross")),
        uiOutput(ns("teaser_cross")),
        card_footer(actionButton(ns("go_cross"), i18n$t("Open the tab"),
                                 class = "btn-sm btn-primary"))
      )
    )
  )
}

mod_summary_server <- function(id, filt, data, sel, opts, main_session, lang, i18n_s) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) { lang(); i18n_s$t(x) }
    cfg <- reactive(data$config())
    
    vars_wave   <- reactive(vars_levels(cfg(), "wave"))
    vars_filter <- reactive(vars_levels(cfg(), "filter"))
    
    # Une variable "fictive" (créée par prepa_survey faute de config) ne compte
    # pas : elle ne contient qu'une seule modalité "All".
    n_modalities <- function(var) {
      req(data$df())
      length(unique(as.character(data$df()[[var]])))
    }
    
    fmt <- function(x) format(x, big.mark = " ", trim = TRUE)
    
    # ===================== Volumétrie ========================================
    output$no_data_msg <- renderUI({
      if (!is.null(data$df())) return(NULL)
      div(class = "alert alert-warning", tr("No survey loaded yet."))
    })
    
    output$boxes <- renderUI({
      req(data$df())
      
      n_vd <- length(intersect(cfg()$vars_discretes, names(data$df())))
      n_vc <- length(intersect(cfg()$vars_continuous, names(data$df())))
      n_itw <- n_modalities(cfg()$var_intvwr)
      keys1 <- keys_vars(data$df(), vars_wave(), cfg()$var_wave, level = 1)
      
      sub <- function(txt) p(class = "small mb-0 opacity-75", txt)
      
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box(
          title = tr("Interviews"), value = fmt(nrow(data$df())),
          showcase = bs_icon("list-ol"), theme = "primary",
          sub(paste(fmt(ncol(data$df())), tr("columns")))
        ),
        value_box(
          title = tr("Variables"), value = fmt(n_vd + n_vc),
          showcase = bs_icon("columns-gap"), theme = "secondary",
          sub(paste0(fmt(n_vd), " ", tr("categorical"), " / ",
                     fmt(n_vc), " ", tr("continuous")))
        ),
        value_box(
          title = tr("Interviewers"),
          value = if (n_itw > 1) fmt(n_itw) else "—",
          showcase = bs_icon("person-vcard"), theme = "info",
          sub(if (n_itw > 1) paste(fmt(round(nrow(data$df()) / n_itw)),
                                   tr("interviews on average"))
              else tr("no interviewer variable"))
        ),
        value_box(
          title = tr("Waves"),
          value = if (!identical(keys1, "All")) fmt(length(keys1)) else "—",
          showcase = bs_icon("water"), theme = "success",
          sub(if (length(keys1) > 1) paste(dplyr::first(keys1), "→", dplyr::last(keys1))
              else tr("no wave variable"))
        )
      )
    })
    
    # ===================== Identité de l'enquête =============================
    output$survey_table <- renderUI({
      req(data$df())
      
      f <- file.path(data$path_survey(), paste0(opts$data_rds_pattern, ".rds"))
      prepared <- if (file.exists(f))
        format(file.info(f)$mtime, "%Y-%m-%d %H:%M") else "—"
      
      n_filter <- n_modalities(cfg()$var_filter)
      
      rows <- list(
        c(tr("Name"), paste(cfg()$name_survey, collapse = " ")),
        c(tr("Path"), data$path_survey_short()),
        c(tr("Prepared on"), prepared),
        c(tr("Wave"), paste(vars_wave(), collapse = " / ")),
        c(tr("Filter"), if (n_filter > 1) paste0(paste(vars_filter(), collapse = " / "),
                                                 " (", n_filter, ")") else "—"),
        c(tr("Interviewer"), if (n_modalities(cfg()$var_intvwr) > 1)
          cfg()$var_intvwr else "—"),
        c(tr("Timers"), if (isTRUE(data$timer()$ready)) tr("Yes") else tr("No"))
      )
      
      tags$table(
        class = "table table-sm mb-0",
        tags$tbody(lapply(rows, function(r) tags$tr(
          tags$th(scope = "row", class = "text-muted fw-normal", r[1]),
          tags$td(r[2])
        )))
      )
    })
    
    # ===================== Entretiens par vague ==============================
    output$plot_waves <- renderPlot({
      req(data$df())
      df <- data$df()
      keys <- as.character(df[[cfg()$var_wave]])
      
      d <- dplyr::tibble(lvl1 = key_level1(keys),
                         lvl2 = if (length(vars_wave()) > 1) key_level2(keys) else NA_character_)
      
      if (length(unique(d$lvl1)) <= 1 && all(is.na(d$lvl2))) {
        validate(tr("No wave variable"))
      }
      
      d <- d %>% dplyr::count(lvl1, lvl2)
      
      p <- ggplot(d, aes(x = lvl1, y = n)) +
        labs(x = NULL, y = NULL, fill = if (length(vars_wave()) > 1) vars_wave()[2] else NULL) +
        scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
        theme_minimal(base_size = 14) +
        theme(panel.grid.major.x = element_blank(),
              legend.position = "bottom")
      
      if (all(is.na(d$lvl2))) {
        p + geom_col(fill = "#2c5f7a", width = 0.7)
      } else {
        p + aes(fill = lvl2) + geom_col(width = 0.7) + scale_fill_viridis_d()
      }
    })
    
    # ===================== Points d'entrée ===================================
    output$scope <- renderText({
      paste0(tr("For the selected wave and filter"), " : ",
             sel$wave(), " / ", sel$filter())
    })
    
    # Écarts enquêteur·rice x variable au-dessus des seuils (objet de l'onglet
    # Croisé, réutilisé tel quel : les chiffres affichés ici sont les siens)
    # NULL = pas d'analyse enquêteur·rice pour cette enquête (une seule
    # personne, ou pas de variable enquêteur·rice) ; 0 ligne = rien au-dessus
    # des seuils, ce qui est une information différente.
    ranking <- reactive({
      if (is.null(data$df_stats_intvwr())) return(NULL)
      filt$df_var_ranking()
    })
    
    # Variables ayant varié entre les vagues. Même règle que l'onglet Vague
    # (coefficient de variation d'un indicateur comparé au seuil), appliquée
    # ici à toutes les vagues du niveau sélectionné.
    wave_moved <- reactive({
      req(filt$df_stats_wave(), data$df())
      keys <- keys_vars(data$df(), vars_wave(), cfg()$var_wave, level = sel$wave_level())
      
      d <- filt$df_stats_wave() %>%
        dplyr::filter(!!sym(cfg()$var_wave) %in% keys,
                      stat %in% c("missing", "median", "Nmod")) %>%
        dplyr::group_by(variable, stat) %>%
        dplyr::summarise(Nrow = sum(Nrow, na.rm = TRUE),
                         Nval = sum(Nval, na.rm = TRUE),
                         cv = sd(value, na.rm = TRUE) / mean(value, na.rm = TRUE),
                         .groups = "drop") %>%
        dplyr::mutate(cv = tidyr::replace_na(cv, 0)) %>%
        dplyr::filter(Nrow >= filt$threshold_Nrow(),
                      Nval >= filt$threshold_Nval(),
                      abs(cv) > filt$threshold_wave())
      
      # summarise() sur 0 ligne évalue max() sur un vecteur vide (warning)
      if (nrow(d) == 0) return(dplyr::tibble(variable = character(), cv = numeric()))
      
      d %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(cv = max(abs(cv)), .groups = "drop") %>%
        dplyr::arrange(dplyr::desc(cv))
    })
    
    # Bloc "grand chiffre + libellé + top 5"
    teaser <- function(n, label, items) {
      tagList(
        div(class = "d-flex align-items-baseline gap-2",
            span(class = "display-5 fw-semibold text-primary", fmt(n)),
            span(class = "text-muted", label)),
        if (length(items))
          tags$ul(class = "small mt-2 mb-0 ps-3", lapply(items, tags$li))
        else div(class = "small text-muted mt-2", tr("Nothing above the thresholds"))
      )
    }
    
    output$teaser_intvwr <- renderUI({
      d <- ranking()
      if (is.null(d))
        return(div(class = "text-muted", tr("no interviewer variable")))
      if (nrow(d) == 0)
        return(teaser(0, tr("interviewers with at least one deviation"), NULL))
      
      top <- d %>%
        dplyr::count(!!sym(cfg()$var_intvwr), name = "n") %>%
        dplyr::arrange(dplyr::desc(n)) %>% utils::head(5)
      
      teaser(dplyr::n_distinct(dplyr::pull(d, !!sym(cfg()$var_intvwr))),
             tr("interviewers with at least one deviation"),
             paste0(dplyr::pull(top, 1), " — ", top$n, " ", tr("variables")))
    })
    
    output$teaser_wave <- renderUI({
      d <- wave_moved()
      teaser(nrow(d), tr("variables that changed between waves"),
             utils::head(d$variable, 5))
    })
    
    output$teaser_cross <- renderUI({
      d <- ranking()
      if (is.null(d))
        return(div(class = "text-muted", tr("no interviewer variable")))
      if (nrow(d) == 0)
        return(teaser(0, tr("interviewer x variable deviations"), NULL))
      
      top <- d %>% dplyr::count(variable, name = "n") %>%
        dplyr::arrange(dplyr::desc(n)) %>% utils::head(5)
      
      teaser(nrow(d), tr("interviewer x variable deviations"),
             paste0(top$variable, " — ", top$n, " ", tr("interviewers")))
    })
    
    # ===================== Navigation ========================================
    go <- function(input_id, target) {
      observeEvent(input[[input_id]], {
        nav_select("main_nav", target, session = main_session)
      })
    }
    
    go("go_intvwr", "tab_intvwr")
    go("go_wave",   "tab_wave")
    go("go_cross",  "tab_intvwr_variable")
  })
}