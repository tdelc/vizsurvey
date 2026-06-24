# ============================================================================
# mod_dict.R  — onglets "Dictionnaire"
# Consomme : dict
# ============================================================================

mod_dict_server <- function(id, dict, nav_id = "main_nav", main_session, i18n_s) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) i18n_s$t(x)
    inserted <- reactiveVal(FALSE)
    
    output$dict_table <- DT::renderDT({
      req(dict)
      DT::datatable(dict, escape = FALSE, filter = "top",
                    rownames = FALSE, options = list(pageLength = 20))
    })
    
    observeEvent(dict, ignoreNULL = FALSE, {
      d <- dict
      available <- !is.null(d) && (!is.data.frame(d) || nrow(d) > 0)
      
      if (available && !inserted()) {
        nav_insert(
          nav_id, target = "tab_data", position = "after", select = FALSE,
          nav = nav_panel(
            title = tagList(bs_icon("book-half"), tr("Dictionnary")),
            value = "tab_dict",
            card(full_screen = TRUE, min_height = "800px",
                 icon_header("table", tr("Dictionnary")),
                 DT::DTOutput(ns("dict_table")))
          ),
          session = main_session
        )
        inserted(TRUE)
      } else if (!available && inserted()) {
        nav_remove(nav_id, target = "tab_dict", session = main_session)
        inserted(FALSE)
      }
    })
  })
}
