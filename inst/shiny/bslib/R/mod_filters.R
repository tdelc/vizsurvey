# ============================================================================
# mod_filters.R
# Contrôles vague / zone / vagues de comparaison. Remplace verif_init() et le
# test "Loading..." par freezeReactiveValue + req() sur la donnée réelle.
#
# CONTRAT (retour) : liste de reactives  $wave, $wave_level, $filter
#
# Multi-niveaux : quand var_wave (ou var_filter) contient deux variables, un
# second bouton radio en cascade est affiché (facultatif, "All" par défaut).
# La clé retournée est soit celle du niveau 1 ("2024"), soit la clé complète
# ("2024 / T1") : les deux existent dans les stats préparées.
# ============================================================================

mod_filters_ui <- function(id, i18n) {
  ns <- NS(id)

  list(
    bslib::accordion_panel(
      title = i18n$t("Database filtering"),
      value = "filtering",
      icon = bs_icon("funnel-fill"),
      radioButtons(ns("config_wave"), i18n$t("Wave"), choices = i18n$t("Loading...")),
      uiOutput(ns("ui_wave2")),
      radioButtons(ns("config_filter"), i18n$t("Filter"), choices = i18n$t("Loading...")),
      uiOutput(ns("ui_filter2")),
      div(class = "small text-muted mt-2", textOutput(ns("info_nb_enq")))
    ),
    br(),
    bslib::nav_item(
      actionButton(ns("button_parms"),
                   tagList(bs_icon("gear"), i18n$t("Detection Thresholds")),
                   class = "btn-m"))
  )
}

mod_filters_server <- function(id, data, lang, i18n_s) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) { lang(); i18n_s$t(x) }

    cfg         <- reactive(data$config())
    vars_wave   <- reactive(vars_levels(cfg(), "wave"))
    vars_filter <- reactive(vars_levels(cfg(), "filter"))

    # Peuple les inputs quand la config/df arrive ; freeze évite l'état transitoire
    observeEvent(data$df(), {
      modal <- keys_vars(data$df(), vars_wave(), cfg()$var_wave, level = 1)

      freezeReactiveValue(input, "config_wave")
      updateRadioButtons(session, "config_wave", label = vars_wave()[1],
                         inline = TRUE,
                         choices = modal, selected = dplyr::last(modal))

      freezeReactiveValue(input, "wave_compare")
      updateCheckboxGroupInput(session, "wave_compare", label = cfg()$var_wave, inline = TRUE,
                               choices = modal, selected = modal[max(1, length(modal) - 1)])

      freezeReactiveValue(input, "config_filter")
      if (length(cfg()$var_filter)) {
        filter_mod <- keys_vars(data$df(), vars_filter(), cfg()$var_filter, level = 1)
        updateRadioButtons(session, "config_filter", label = vars_filter()[1],
                           inline = TRUE,
                           choices = unique(c("All", filter_mod)), selected = "All")
      } else {
        updateRadioButtons(session, "config_filter", label = "Filter", inline = TRUE,
                           choices = "All")
      }
    })

    # --- Second niveau (facultatif), en cascade du premier -------------------
    # Les choix sont les clés complètes ("2024 / T1"), les libellés le niveau 2.
    output$ui_wave2 <- renderUI({
      req(data$df(), input$config_wave)
      if (length(vars_wave()) < 2) return(NULL)

      keys <- keys_vars(data$df(), vars_wave(), cfg()$var_wave, level = 2)
      keys <- keys[key_level1(keys) == input$config_wave]
      if (length(keys) == 0) return(NULL)

      radioButtons(ns("config_wave2"), vars_wave()[2], inline = TRUE,
                   choices = c(All = "All", setNames(keys, key_level2(keys))),
                   selected = "All")
    })

    output$ui_filter2 <- renderUI({
      req(data$df(), input$config_filter)
      if (length(vars_filter()) < 2 || input$config_filter == "All") return(NULL)

      keys <- keys_vars(data$df(), vars_filter(), cfg()$var_filter, level = 2)
      keys <- keys[key_level1(keys) == input$config_filter]
      if (length(keys) == 0) return(NULL)

      radioButtons(ns("config_filter2"), vars_filter()[2], inline = TRUE,
                   choices = c(All = "All", setNames(keys, key_level2(keys))),
                   selected = "All")
    })

    # Clé courante : niveau 1 seul, ou clé complète si le niveau 2 est choisi.
    # Le test sur key_level1 évite l'état transitoire quand le niveau 1 change.
    wave_key <- reactive({
      req(input$config_wave)
      key2 <- input$config_wave2
      if (length(vars_wave()) < 2 || is.null(key2) || key2 == "All") return(input$config_wave)
      if (key_level1(key2) != input$config_wave) return(input$config_wave)
      key2
    })

    filter_key <- reactive({
      req(input$config_filter)
      key2 <- input$config_filter2
      if (input$config_filter == "All") return("All")
      if (length(vars_filter()) < 2 || is.null(key2) || key2 == "All") return(input$config_filter)
      if (key_level1(key2) != input$config_filter) return(input$config_filter)
      key2
    })

    observeEvent(input$button_parms, {
      showModal(modalDialog(
        h3(tr("Analysis configuration")),
        fluidRow(
          numericInput(ns("threshold_Nrow"), 
                       tr("Minimal number of rows to include interviewer of variable anomalies detection"), 
                       value = threshold_Nrow()),
          numericInput(ns("threshold_Nval"), 
                       tr("Minimum number of valid values to include the variable in the analysis"), 
                       value = threshold_Nval()),
          numericInput(ns("threshold_wave"), 
                       tr("Minimum rate of change to detect wave anomalies"), 
                       value = threshold_wave(), min = 0, max = 2, step = 0.1),
          numericInput(ns("threshold_intvwr"), 
                       tr("chi² distance / minimum median deviation to detect investigator anomalies"),
                       value = threshold_intvwr(), min = 0, max = 10, step = 1)
        ),
        size = "l",
        footer = tagList(
          modalButton("Ok")
        )
      ))
    })
    
    threshold_Nrow <- reactive({
      if (is.null(input$threshold_Nrow)) 30 else input$threshold_Nrow})
    
    threshold_Nval <- reactive({
      if (is.null(input$threshold_Nval)) 30 else input$threshold_Nval})
    
    threshold_wave <- reactive({
      if (is.null(input$threshold_wave)) 0.5 else input$threshold_wave})
    
    threshold_intvwr <- reactive({
      if (is.null(input$threshold_intvwr)) 5 else input$threshold_intvwr})

    list(
      wave             = reactive({ wave_key() }),
      wave_first       = reactive(input$config_wave),
      wave_level       = reactive({ if (identical(wave_key(), input$config_wave)) 1 else 2 }),
      filter           = reactive({ filter_key() }),
      threshold_Nrow   = reactive(threshold_Nrow()),
      threshold_Nval   = reactive(threshold_Nval()),
      threshold_wave   = reactive(threshold_wave()),
      threshold_intvwr = reactive(threshold_intvwr())
    )
  })
}
