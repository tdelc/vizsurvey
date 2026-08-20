mod_dict_server <- function(id, data, nav_id = "main_nav", main_session, lang, i18n_s) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) { lang(); i18n_s$t(x) }
    inserted <- reactiveVal(FALSE)
    
    dict <- reactive({
      req(path_dict)
      readRDS(path_dict)
    })
    
    output$dict_table <- DT::renderDT({
      req(data$df_dict())
      DT::datatable(data$df_dict(), escape = FALSE, filter = "top",
                    rownames = FALSE, options = list(pageLength = 20))
    })
    
    observeEvent(data$df_dict(), ignoreNULL = FALSE, {
      d <- data$df_dict()
      available <- !is.null(d) && (!is.data.frame(d) || nrow(d) > 0)
      
      if (available && !inserted()) {
        nav_insert(
          nav_id, target = "tab_data", position = "after", select = FALSE,
          nav = nav_panel(
            title = tagList(bs_icon("book-half"), tr("Dictionnary")),
            value = "tab_dict",
            card(full_screen = TRUE, min_height = "800px",
                 icon_header("table", tr("Dictionnary"), help = ns("card_dict")),
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
