# ============================================================================
# mod_source.R
# Navigation dossier/enquête + import utilisateur·ice + chargement RDS/timer.
# Source UNIQUE de la profondeur (corrige le bug depth_folder global vs values_ini).
#
# CONTRAT (retour) : liste de reactives
#   $df, $df_stats, $df_stats_group  : data.frames de l'enquête
#   $config                          : liste config (vw, vz, vg, vc, vd, date, ...)
#   $timer                           : list(ready, df_timer_enq/_ssn/_sctn, error)
#   $path_survey                     : chemin du dossier enquête courant (pur)
# ============================================================================

mod_source_ui <- function(id, i18n) {
  ns <- NS(id)
  list(
    shiny.i18n::usei18n(i18n),
    accordion_panel(
      title = i18n$t("Survey loading"),
      value = "loading",
      icon = bs_icon("database-fill"),
      radioButtons(ns("path_folder"), i18n$t("List of directories"), choices = "Loading..."),
      radioButtons(ns("path_survey"), i18n$t("List of databases"),   choices = "Loading...")
    )
  )
}

mod_source_server <- function(id, opts, lang, i18n_s) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) { lang(); i18n_s$t(x) }
    
    # --- État interne : UNE seule profondeur, UN seul dossier racine ---------
    rv <- reactiveValues(
      depth = opts$depth_folder,
      root  = opts$path_data_folder,
      pattern = opts$data_rds_pattern,
      df_user = NULL
    )

    # ===================== Navigation dossier/enquête ========================
    folders <- reactive({
      if (rv$depth == 3L) list.files(rv$root) else basename(rv$root)
    })

    observeEvent(folders(), {
      freezeReactiveValue(input, "path_folder")
      updateRadioButtons(session, "path_folder", inline = TRUE, choices = folders())
    })

    surveys <- reactive({
      req(input$path_folder)
      vs <- switch(as.character(rv$depth),
        "1" = folders(),
        "2" = list.dirs(rv$root, full.names = TRUE, recursive = FALSE),
        "3" = list.dirs(file.path(rv$root, input$path_folder),
                        full.names = TRUE, recursive = FALSE)
      )
      sort(basename(vs))
    })

    observeEvent(surveys(), {
      freezeReactiveValue(input, "path_survey")
      updateRadioButtons(session, "path_survey", inline = TRUE,
                         choices = surveys(), selected = surveys()[1])
    })

    # path_survey PUR : calcule et retourne, aucun effet de bord
    path_survey <- reactive({
      switch(as.character(rv$depth),
        "1" = rv$root,
        "2" = file.path(rv$root, input$path_survey),
        "3" = file.path(rv$root, input$path_folder, input$path_survey)
      )
    })
    
    # path_survey short
    path_survey_short <- reactive({
      switch(as.character(rv$depth),
             "1" = "",
             "2" = file.path(input$path_survey),
             "3" = file.path(input$path_folder, input$path_survey)
      )
    })

    # ===================== Chargement du .rds global =========================
    global_obj <- reactive({
      path <- path_survey()
      f <- file.path(path, paste0(rv$pattern, ".rds"))
      req(file.exists(f))
      readRDS(f)
    })

    timer <- reactive({
      path <- path_survey()
      tf <- find_timer_rds(path)
      if (is.null(tf)) return(list(ready = FALSE, error = tr("Data unavailable.")))
      tryCatch({
        t <- readr::read_rds(tf)
        list(ready = TRUE, cfg = t$cfg, df_timer_intv = t$df_timer_intv,
             df_timer_ssn = t$df_timer_ssn, df_timer_sctn = t$df_timer_sctn)
      }, error = function(e) list(ready = FALSE, error = conditionMessage(e)))
    })
    
    # ===================== Chargement du dictionnaire ========================
    df_dict <- reactive({
      req(opts$path_dict)
      readRDS(opts$path_dict)
    })
    
    
    # ===================== Chargement de la nomenclature ====================
    df_nomen <- reactive({
      req(opts$path_nomen)
      df <- tibble(data.table::fread(opts$path_nomen))
      df$LABEL <- pull(df[,toupper(lang())])
      df
    })
    

    # ===================== Contrat de sortie =================================
    list(
      df                = reactive(global_obj()[["df"]]),
      df_stats_wave     = reactive(global_obj()[["df_stats_wave"]]),
      df_stats_intvwr   = reactive(global_obj()[["df_stats_intvwr"]]),
      df_dict           = df_dict,
      df_nomen          = df_nomen,
      config            = reactive(global_obj()[["configs"]]),
      timer             = timer,
      path_survey       = path_survey,
      path_survey_short = path_survey_short
    )
  })
}
