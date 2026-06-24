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
    bslib::accordion_panel(
      title = i18n$t("Analysis configuration"),
      value = "config",
      icon = bs_icon("gear"),
      bslib::tooltip( 
        numericInput(ns("threshold_Nrow"), i18n$t("N row min"), value = 30),
        i18n$t("Number of rows to include a interviewer of a variable"), 
        id = "tip", 
        placement = "right" 
      ),
      numericInput(ns("threshold_Nval"), i18n$t("N val min"), value = 30),
      numericInput(ns("threshold_wave"), i18n$t("Δ min (wave)"), value = 0.5, min = 0, max = 2, step = 0.1),
      numericInput(ns("threshold_intvwr"), i18n$t("Δ min (intvwr)"), value = 5, min = 0, max = 10, step = 1)
    )
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

    list(
      wave             = reactive({ req(input$config_wave); input$config_wave }),
      filter           = reactive({ req(input$config_filter); input$config_filter }),
      threshold_Nrow   = reactive(input$threshold_Nrow),
      threshold_Nval   = reactive(input$threshold_Nval),
      threshold_wave   = reactive(input$threshold_wave),
      threshold_intvwr = reactive(input$threshold_intvwr)
    )
  })
}
