# ============================================================================
# mod_archive.R  — onglet "Archive / Historique"
# Consomme : sel (wave/zone pour pré-remplir), r_focus (intvwr/variable), opts.
# Fix : état en mémoire (reactiveVal) + écriture disque ; plus de relecture
#       systématique du CSV qui pouvait écraser l'état courant.
# ============================================================================

mod_archive_ui <- function(id, i18n) {
  ns <- NS(id)
  list(
    bslib::nav_panel(
      title = tagList(bs_icon("file-earmark-zip"), i18n$t("Archive")),
      value = "tab_archive",
      bslib::card(
        full_screen = TRUE,
        min_height = "800px",
        icon_header("table", i18n$t("Archive"), help = ns("card_archive")),
        DT::DTOutput(ns("archive"))
      )
    ),
    bslib::nav_spacer(),
    bslib::nav_item(actionButton(ns("add_button"),i18n$t("Add history"), class = "btn-sm"))
  )
}

mod_archive_server <- function(id, data, sel, r_focus, opts, i18n_s) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) i18n_s$t(x)
    
    archive <- reactiveVal(
      if (file.exists(opts$path_archive)) utils::read.csv(opts$path_archive)
      else tibble::tibble()
    )

    observeEvent(input$add_button, {
      showModal(modalDialog(
        h3(tr("Explanation")),
        fluidRow(
          textInput(ns("add_user"), tr("User"),value = opts$user),
          column(6,textInput(ns("add_path"), tr("Path"),value = data$path_survey_short())),
          column(6,textInput(ns("add_wave"), tr("Wave"),value = sel$wave()))
        ),
        fluidRow(
          column(6,textInput(ns("add_intvwr"), tr("Interviewer"),value = r_focus$intvwr)),
          column(6,textInput(ns("add_variable"), tr("Variable"),value = r_focus$variable))
        ),
        textAreaInput(ns("add_com"), tr("Comment"),height = 100,width = 400,
                      placeholder = 'explanation'),
        footer = tagList(
          modalButton(tr("Cancel")),
          actionButton(ns("add_submit"), tr("Send"))
        )
      ))
    })

    observeEvent(input$add_submit, {
      if (!is.null(input$add_com)) {
        record <- tibble(timestamp = as.character(lubridate::today()),
                         user = input$add_user,
                         path = input$add_path,
                         intvwr = input$add_intvwr,
                         variable = input$add_variable,
                         comment = input$add_com)
        
        if (!file.exists(opts$path_archive)){
          write.csv(record, file = opts$path_archive, row.names = FALSE)
        }else{
          df <- utils::read.csv(opts$path_archive) %>% add_row(record)
          write.csv(df, file = opts$path_archive, row.names = FALSE)
          archive(df)
        }
        removeModal()
      } else {
        showNotification(tr("Erreur"))
      }
    })

    output$archive <- DT::renderDT({
      DT::datatable(archive(), escape = FALSE, filter = "top",
                    rownames = FALSE, options = list(pageLength = 20))
    })
  })
}
