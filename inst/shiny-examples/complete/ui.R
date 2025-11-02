library(shiny)
library(shiny.i18n)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinybusy)

library(plotly)
library(DT)
library(gt)

i18n <- Translator$new(translation_csvs_path = "i18n")

ui <- dashboardPage(
  title="Vizsurvey",
  header = dashboardHeader(title = "Vizsurvey",
                           leftUi = tagList(
                             dropdownBlock(
                               id = "mydropdown",
                               title = "Language",
                               icon = icon("flag"),
                               badgeStatus = "primary",
                               selectInput(
                                 inputId='lang',
                                 label=i18n$t('Change language'),
                                 choices = i18n$get_languages(),
                                 selected = i18n$get_key_translation()
                               )
                             )
                           )),
  #### Sidebar ####
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      shiny.i18n::usei18n(i18n),
      menuItem(i18n$t("Interviewer"), tabName = "tab_itw"),
      menuItem(i18n$t("Domain"), tabName = "tab_domain"),
      menuItem(i18n$t("Data"), tabName = "tab_data")
    )
  ),
  controlbar = dashboardControlbar(
    skin = "dark",width = 360,
    controlbarMenu(
      id = "menu",
      controlbarItem(
        shiny.i18n::usei18n(i18n),
        "Explications des onglets",
        uiOutput("texte_aide")
      )
    )
  ),
  body = dashboardBody(
    shiny.i18n::usei18n(i18n),
    tags$style(HTML("
      .content-wrapper { overflow: auto; }

      .UI_output iframe {
        width: 100% !important;
        height: 800px !important;
      }

      .leaflet-top{
        position: relative;
      }

      .custom-row {
        display: flex;
        align-items: center;
        justify-content: space-between;
      }

      .btn-group-toggle .btn-custom-class.active {
        /*background: lightgreen !important;*/
        background: #28a745 !important;
        color: #fff !important;
      }

      .btn-group-toggle .btn-blue-class.active {
        background: lightblue !important;
      }

      .flex-container {
        display: flex;
      }

      .rightAlign{float:right;}

      .box_cadre {
        flex: 1;
        border: 1px solid #3c8dbc;
        padding: 10px;
        border-radius: 2px;
      }")),
    add_busy_spinner(spin = "fading-circle"),
    #### Configure Zone and domain ####
    fluidRow(
      box(width = 5,title=i18n$t("Database loading"),
          status = "primary", solidHeader = TRUE,
          radioGroupButtons(
            inputId = "path_folder",label = i18n$t("List of directories"),
            status = "primary",justified = TRUE,
            choices = i18n$t("Loading...")),
          radioGroupButtons(
            inputId = "path_survey",label = i18n$t("List of databases"),
            status = "primary",justified = TRUE,
            choices = i18n$t("Loading..."))
      ),
      box(width = 7,title = i18n$t("Database filtering"),
          status = "primary",solidHeader = TRUE,
          fluidRow(
            column(width = 6,
                   radioGroupButtons("config_domain",label = i18n$t("Domain"),
                                     choices = i18n$t("Loading..."))),
            column(width = 6,
                   radioGroupButtons("config_zone",
                                     label = i18n$t("Zone"),
                                     choices = i18n$t("Loading...")
                   ))
          )
      )
    ),
    tabItems(
      #### Domain Tab ####
      tabItem(tabName = "tab_domain",
              fluidRow(
                box(width = 12,title=i18n$t("Parameters"),
                    status = "primary", solidHeader = TRUE,
                    column(width=4,
                           checkboxGroupButtons("domain_compare",label = i18n$t("Compare with"),
                                             choices = i18n$t("Loading..."))
                    ),
                    column(width=4,
                           sliderInput("domain_sensibility",
                                       label = i18n$t("Sensibility (sd)"),ticks = F,
                                       min = 0,max=1,value = 0.5)
                    ),
                    column(width = 4,
                           sliderInput("domain_Nval",
                                       label = i18n$t("N valid (not missing) minimum"),ticks = F,
                                       min = 0,max=100,value = 30)
                    )
                )
              ),
              radioGroupButtons(
                inputId = "domain_subtab_choice",label = NULL,
                status = "custom-class",justified = TRUE,size="lg",
                choices = c("Summary"  = "domain_summary",
                            "Graphics" = "domain_graphics")
                ),
              tabsetPanel(id = "domain_subtab",type ="hidden",
                          tabPanelBody("domain_graphics",
                                       uiOutput("domain_plots_outliers",fill=TRUE)
                          ),
                          tabPanelBody("domain_summary",
                                       fluidRow(
                                         box(width = 7,title=i18n$t("Changed Variables"),
                                             status = "primary", solidHeader = TRUE,
                                             gt_output("domain_tab_outliers")
                                         ),
                                         box(width = 5,title=i18n$t("Dis/Appeared Variables"),
                                             status = "primary", solidHeader = TRUE,
                                             gt_output("domain_tab_check")
                                         )
                                       )
                          )
              )
      ),
      #### Interviewer Tab ####
      tabItem(tabName = "tab_itw",
              fluidRow(
                box(width = 12,title=i18n$t("Parameters"),
                    status = "primary", solidHeader = TRUE,
                    column(width = 2,
                           sliderInput("itw_threshold",
                                       label = i18n$t("Sensibility"),ticks = F,
                                       min = 0,max=10,value = 5)
                    ),
                    column(width = 2,
                           sliderInput("itw_Nrow",
                                       label = i18n$t("N rows"),ticks = F,
                                       min = 0,max=50,value = 20)
                    ),
                    column(width = 2,
                           sliderInput("itw_Nval",
                                       label = i18n$t("N valid (not missing)"),ticks = F,
                                       min = 0,max=50,value = 20)
                    ),
                    column(width = 6,
                           radioGroupButtons(
                             inputId = "itw_choice_heatmap",
                             label = i18n$t("Choose of rows and columns"),
                             status = "custom-class",
                             choices=c("All" = "all",
                                       "Risky rows" = "enq",
                                       "Risky columns" = "var",
                                       "Risky cells" = "both"),
                             selected = "all")
                    )
                )
              ),
              radioGroupButtons(
                inputId = "itw_subtab_choice",label = NULL,
                status = "custom-class",justified = TRUE,size="lg",
                choices = c("Heatmap" = "itw_graphics",
                            "Row Synthesis"  = "itw_row",
                            "Column Synthesis"  = "itw_col"),
                selected = "itw_graphics"),
              tabsetPanel(id = "itw_subtab",type ="hidden",
                          tabPanelBody("itw_graphics",
                                       h2(i18n$t("Anomalies by Variables and Interviewer")),
                                       plotlyOutput("itw_heatmap",height = "800px"),
                                       br()
                          ),
                          tabPanelBody("itw_row",
                                       fluidRow(
                                         box(width = 6,title=i18n$t("Ranking of Interviewer"),
                                             status = "primary", solidHeader = TRUE,
                                             DTOutput("itw_ranking")
                                         ),
                                         box(width = 6,title=i18n$t("Listing of variables"),
                                             status = "primary", solidHeader = TRUE,
                                             DTOutput("itw_listing")
                                         )
                                       )
                          ),
                          tabPanelBody("itw_col",
                                       fluidRow(
                                         box(width = 6,title=i18n$t("Ranking of Interviewer"),
                                             status = "primary", solidHeader = TRUE,
                                             DTOutput("itw_var_ranking")
                                         ),
                                         box(width = 6,title=i18n$t("Listing of variables"),
                                             status = "primary", solidHeader = TRUE,
                                             DTOutput("itw_var_listing")
                                         )
                                       )
                          ),
                          fluidRow(
                            column(width = 6,
                                   fluidRow(
                                     column(width = 4,
                                            switchInput("itw_missing",label = i18n$t("Missings?"),
                                                        value = TRUE, onLabel = i18n$t("Yes"),
                                                        onStatus = "success",
                                                        offLabel = i18n$t("No"),labelWidth = "100%")
                                     )
                                   )
                            ),
                            column(width = 6,
                                   column(width = 4,
                                          actionBttn(
                                            inputId = "itw_all_distrib",
                                            label = i18n$t("Show Distributions"),
                                            style = "unite", color = "warning")
                                   )
                            )
                          ),
                          # Graphiques
                          fluidRow(
                            column(width = 6,
                                   box(width = 12,title=textOutput("itw_distrib_spe_text"),
                                       status = "primary", solidHeader = TRUE,
                                       plotOutput("itw_distrib_spe"),
                                       verbatimTextOutput("itw_summary_spe")
                                   )
                            ),
                            column(width = 6,
                                   box(width = 12,title=textOutput("itw_distrib_glo_text"),
                                       status = "primary", solidHeader = TRUE,
                                       plotOutput("itw_distrib_glo"),
                                       verbatimTextOutput("itw_summary_glo")
                                   )
                            )
                          ),
                          fluidRow(
                            column(width = 6,
                                   box(width = 12,title=textOutput("itw_corr_spe_text"),
                                       status = "primary", solidHeader = TRUE,
                                       plotOutput("itw_main_corr_spe",height = "200px"),
                                       br(),plotOutput("itw_corr_spe")
                                   )
                            ),
                            column(width = 6,
                                   box(width = 12,title=textOutput("itw_corr_glo_text"),
                                       status = "primary", solidHeader = TRUE,
                                       plotOutput("itw_main_corr_glo",height = "200px"),
                                       br(),plotOutput("itw_corr_glo")
                                   )
                            )
                          )
              )
      ),
      #### Micro données ####
      tabItem(tabName = "tab_data",
              fluidRow(
                box(width = 12,title=i18n$t("Variables to show"),
                    status = "info", solidHeader = TRUE,collapsible = TRUE,
                    checkboxGroupButtons("data_variables",label = i18n$t("Choice"),
                                         choices = character(0)
                    )
                ),
                box(width = 12,title=i18n$t("Micro Data"),
                  DTOutput("data_table")
                )
              )
      )
    )
  )
)


