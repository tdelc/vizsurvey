# ============================================================================
# mod_data_explorer.R  — onglets "Data" + "Dictionnaire"
# Consomme : data (df, config)
# Le dictionnaire (df_dict) devrait venir de data, pas d'un global implicite.
# ============================================================================

mod_data_explorer_ui <- function(id, i18n) {
  ns <- NS(id)
  bslib::nav_panel(
    title = tagList(bs_icon("database"), i18n$t("Data")),
    value = "tab_data",
    selectInput(ns("variables"), i18n$t("Variables selection"), 
                choices = character(0), multiple = TRUE, width = "100%"),
    bslib::card(
      full_screen = TRUE,
      min_height = "800px",
      icon_header("table", i18n$t("Data"), help = ns("card_data")),
      DT::DTOutput(ns("table"))
    )
  )
}

mod_data_explorer_server <- function(id, data, i18n_s) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) i18n_s$t(x)

    observeEvent(data$df(), {
      cfg <- data$config()
      keep <- unique(c(cfg$vw, cfg$vg, colnames(data$df())[1:5]))
      freezeReactiveValue(input, "variables")
      updateSelectInput(session, "variables",
                        choices = colnames(data$df()), selected = keep)
    })

    output$table <- DT::renderDT({
      req(data$df())
      out <- data$df()[, intersect(input$variables, names(data$df())), drop = FALSE] %>%
        dplyr::mutate(dplyr::across(dplyr::everything(),
                                    ~ ifelse(is.na(.x), "NA", as.character(.x))))
      DT::datatable(out, filter = "top", rownames = FALSE,
                    options = list(pageLength = 20))
    })
  })
}
