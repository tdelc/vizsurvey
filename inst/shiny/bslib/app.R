# ============================================================================
# app.R — assemblage. Compatible standalone ET packagé.
# ============================================================================

source("functions.R")

suppressPackageStartupMessages({
  library(bslib, quietly = T)
  library(bsicons, quietly = T)
  library(shinyjs, quietly = T)
  library(DT, quietly = T)
  library(plotly, quietly = T)
  library(dplyr, quietly = T)
  library(ggplot2, quietly = T)
  library(tidyr, quietly = T)
  library(rlang, quietly = T)
  library(magrittr, quietly = T)
  library(tibble, quietly = T)
  library(scales, quietly = T)
  library(purrr, quietly = T)
  library(lubridate, quietly = T)
  library(stringr, quietly = T)
  library(seriation, quietly = T)
  # library(arrow, quietly = T)
  library(readr, quietly = T)
  library(viridisLite, quietly = T)
})

# ---- Thème ----
vz_theme <- bslib::bs_theme(
  version = 5,
  bg = "#fafbfc",
  fg = "#1a1d29",
  primary = "#2c5f7a",
  secondary = "#6c7a89",
  success = "#5a9f7d",
  info = "#5b8db8",
  warning = "#d4a056",
  # base_font = font_google("Inter"),
  # heading_font = font_google("Inter"),
  # code_font = font_google("JetBrains Mono"),
  "card-border-color" = "#e5e8ec",
  "card-cap-bg"       = "#f4f6f8"
)

# Textes de l'aide contextuelle (help/help_<lang>.md), chargés une fois
HELP <- help_load("help")

i18n <- shiny.i18n::Translator$new(translation_json_path = "i18n/translation.json")
i18n$set_translation_language("fr")
i18n$use_js()

# --- Options d'instance -----------------------------------------------------
# Remplace les getShinyOption() dispersés : un seul objet de config passé aux
# modules, plus de variable globale lue ailleurs.
app_opts <- function() {
  list(
    path_data_folder = getShinyOption("path_data_folder", "data"),
    data_rds_pattern = getShinyOption("data_rds_pattern", "global"),
    depth_folder     = getShinyOption("depth_folder", 1L),
    path_archive     = getShinyOption("path_archive", "archive.csv"),
    path_dict        = getShinyOption("path_dict", NULL),
    path_nomen       = getShinyOption("path_nomen", NULL),
    user             = getShinyOption("user", Sys.getenv("USERNAME", "unknown")),
    seed             = getShinyOption("seed", 42L)  # iForest reproductible
  )
}

app_ui <- function() {
  bslib::page_navbar(
    id = "main_nav",
    title = "Vizsurvey",
    theme = vz_theme,
    fillable = FALSE,
    navbar_options = bslib::navbar_options(
      bg = "#2c5f7a",
      theme = "dark",
      underline = TRUE
    ),
    header = htmltools::tagList(
      tour_assets(),
      shinybusy::add_busy_spinner(color = "#d4a056"),
      shinybusy::add_busy_spinner(spin = "fading-circle", color = "#2c5f7a"),
      tags$head(tags$style(HTML("
      body { font-feature-settings: 'cv11', 'ss01'; }
      .navbar-brand { font-weight: 600; letter-spacing: -0.01em; }
      .card-header { font-weight: 500; }
      .accordion-button { font-weight: 500; font-size: 0.95rem; }
      .accordion-button:not(.collapsed) { background-color: #e8f0f5; color: #2c5f7a; }
      .accordion-button:focus { box-shadow: none; }
      .nav-pills .nav-link.active { background-color: #2c5f7a; }
      .nav-pills .nav-link { color: #2c5f7a; font-weight: 500; }
      .form-label { font-weight: 500; font-size: 0.875rem; color: #4a5568; }
      .form-check-label { font-size: 0.9rem; }
      .bslib-sidebar-layout > .sidebar { border-right: 1px solid #e5e8ec; }
      .card { transition: box-shadow 0.15s ease; }
      .card:hover { box-shadow: 0 2px 8px rgba(0,0,0,0.04); }
      h2 { font-weight: 600; color: #2c5f7a; letter-spacing: -0.01em; }
      .text-muted { color: #6c7a89 !important; }
      .navbar .btn {
        padding-top: 0.25rem;
        padding-bottom: 0.25rem;
        font-size: 0.85rem;
        line-height: 1.4;
      }
      .navbar .btn,
      .navbar .form-control {
        padding-top: 0.25rem;
        padding-bottom: 0.25rem;
        font-size: 0.85rem;
        line-height: 1.4;
      }
      .navbar .form-control      { height: auto; }
      .navbar .shiny-input-container { margin-bottom: 0; }
      .navbar .shiny-input-container .control-label { display: none; }
    ")))
    ),
    sidebar = bslib::sidebar(
      width = 320,
      bslib::accordion(
        open = c("loading", "filtering"),
        multiple = TRUE,
        !!!mod_source_ui("source", i18n),
        !!!mod_filters_ui("filters", i18n)
      )
    ),
    mod_summary_ui("summary", i18n),
    mod_intvwr_ui("intvwr", i18n),
    mod_wave_ui("wave", i18n),
    mod_intvwr_variable_ui("intvwr_variable", i18n),
    mod_data_explorer_ui("data", i18n),
    !!!mod_archive_ui("archive", i18n),
    mod_tour_ui("tour", i18n),
    bslib::nav_item(
      selectInput("selected_lang", NULL,
                  choices  = setNames(i18n$get_languages(), toupper(i18n$get_languages())),
                  selected = "fr",
                  selectize = FALSE,
                  width = "100px")
    )
  )
}

app_server <- function(input, output, session) {
  opts <- app_opts()

  i18n_s <- i18n$clone()
  lang <- reactiveVal("fr")
  
  observeEvent(input$selected_lang, {
    shiny.i18n::update_lang(input$selected_lang)
    i18n_s$set_translation_language(input$selected_lang)
    lang(input$selected_lang)
  }, ignoreInit = TRUE)

  # --- Aide contextuelle ----------------------------------------------------
  # Un seul observeur pour toutes les cards : help_button() pousse la clé de la
  # card dans input$help_show, le texte vient de help/help_<lang>.md
  observeEvent(input$help_show, {
    showModal(help_modal(input$help_show,
                         lang = input$selected_lang %||% "fr",
                         help = HELP,
                         close_label = i18n_s$t("Close")))
  })

  # État partagé inter-onglets (sélection courante groupe/variable)
  r_focus <- reactiveValues(intvwr = "", variable = "", intv = "")
  
  # --- Chaîne de données ----------------------------------------------------
  data <- mod_source_server("source", opts, lang, i18n_s) 
  sel  <- mod_filters_server("filters", data, lang, i18n_s)
  filt <- survey_filtered(data, sel)
  
  observeEvent(filt$df(),{
    r_focus$intvwr <- ""
    r_focus$variable <- ""
  })
  
  # --- Onglets --------------------------------------------------------------
  mod_summary_server("summary", filt, data, sel, opts, session, lang, i18n_s)
  mod_intvwr_server("intvwr", filt, data, r_focus, opts, lang, i18n_s)
  mod_wave_server("wave",  filt, data, sel, r_focus, lang, i18n_s)
  mod_intvwr_variable_server("intvwr_variable", filt, data, r_focus, opts, lang, i18n_s)
  mod_data_explorer_server("data", data, lang, i18n_s)
  mod_dict_server("dict", data, "main_nav",session, lang, i18n_s)
  mod_archive_server("archive", data, sel, r_focus, opts, lang, i18n_s)
  mod_tour_server("tour", data, filt, sel, session, i18n_s)
}

# Standalone
shinyApp(ui = app_ui(), server = app_server)
