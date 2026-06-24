# ============================================================================
# mod_filters.R
# Contrôles vague / zone / vagues de comparaison. Remplace verif_init() et le
# test "Loading..." par freezeReactiveValue + req() sur la donnée réelle.
#
# CONTRAT (retour) : liste de reactives  $wave, $zone, $wave_compare
# ============================================================================

mod_filters_ui <- function(id, i18n) {
  ns <- NS(id)
  
  list(
    bslib::accordion_panel(
      title = i18n$t("Database filtering"),
      value = "filtering",
      icon = bs_icon("funnel-fill"),
      radioButtons(ns("config_wave"), i18n$t("Wave"), choices = i18n$t("Loading...")),
      radioButtons(ns("config_filter"), i18n$t("Filter"), choices = i18n$t("Loading...")),
      div(class = "small text-muted mt-2", textOutput(ns("info_nb_enq")))
    ),
    br(),
    bslib::nav_item(
      actionButton(ns("button_parms"),
                   tagList(bs_icon("gear"), i18n$t("Detection Thresholds")),
                   class = "btn-m"))
  )
}

mod_filters_server <- function(id, data, i18n_s) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) i18n_s$t(x)

    # Peuple les inputs quand la config/df arrive ; freeze évite l'état transitoire
    observeEvent(data$df(), {
      cfg <- data$config()
      modal <- sort(dplyr::pull(unique(data$df()[, cfg$var_wave])))

      freezeReactiveValue(input, "config_wave")
      updateRadioButtons(session, "config_wave", label = cfg$var_wave, inline = TRUE,
                         choices = modal, selected = dplyr::last(modal))

      freezeReactiveValue(input, "wave_compare")
      updateCheckboxGroupInput(session, "wave_compare", label = cfg$var_wave, inline = TRUE,
                               choices = modal, selected = modal[max(1, length(modal) - 1)])

      freezeReactiveValue(input, "config_filter")
      if (length(cfg$var_filter)) {
        filter_mod <- sort(dplyr::pull(unique(data$df()[, cfg$var_filter])))
        updateRadioButtons(session, "config_filter", label = cfg$var_filter, inline = TRUE,
                           choices = unique(c("All", filter_mod)), selected = "All")
      } else {
        updateRadioButtons(session, "config_filter", label = "Filter", inline = TRUE,
                           choices = "All")
      }
    })
    
    observeEvent(input$button_parms, {
      showModal(modalDialog(
        h3(tr("Analysis configuration")),
        fluidRow(
          numericInput(ns("threshold_Nrow"), 
                       tr("Minimal number of rows to include interviewer of variable anomalies detection"), 
                       value = 30),
          numericInput(ns("threshold_Nval"), 
                       tr("Minimum number of valid values to include the variable in the analysis"), 
                       value = 30),
          numericInput(ns("threshold_wave"), 
                       tr("Minimum rate of change to detect wave anomalies"), 
                       value = 0.5, min = 0, max = 2, step = 0.1),
          numericInput(ns("threshold_intvwr"), 
                       tr("chi² distance / minimum median deviation to detect investigator anomalies"),
                       value = 5, min = 0, max = 10, step = 1)
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
      wave             = reactive({ req(input$config_wave); input$config_wave }),
      filter           = reactive({ req(input$config_filter); input$config_filter }),
      threshold_Nrow   = reactive(threshold_Nrow()),
      threshold_Nval   = reactive(threshold_Nval()),
      threshold_wave   = reactive(threshold_wave()),
      threshold_intvwr = reactive(threshold_intvwr())
    )
  })
}
