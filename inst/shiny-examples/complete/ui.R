library(shiny)
library(shinydashboard)
library(plotly)
library(gt)

ui <- dashboardPage(
  title="Vizsurvey",
  header = dashboardHeader(title = "Vizsurvey"),
  #### Sidebar ####
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Interviewer", tabName = "tab_itw"),
      menuItem("Domain", tabName = "tab_domain"),
      menuItem("Data", tabName = "tab_data")
    )
  ),
  body = dashboardBody(
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
    #### Configure Zone and domain ####
    fluidRow(
      box(width = 5,title="Database loading",
          status = "primary", solidHeader = TRUE,
          radioButtons(
            inputId = "path_folder",label = "List of directories",
            choices = "Loading..."),
          radioButtons(
            inputId = "path_survey",label = "List of databases",
            choices = "Loading...")
      ),
      box(width = 7,title = "Database filtering",
          status = "primary",solidHeader = TRUE,
          fluidRow(
            column(width = 6,
                   radioButtons("config_domain",label = "Domain",
                                choices = "Loading...")),
            column(width = 6,
                   radioButtons("config_zone",label = "Zone",
                                choices = "Loading..."))
          )
      )
    ),
    tabItems(
      #### Domain Tab ####
      tabItem(tabName = "tab_domain",
              fluidRow(
                box(width = 12,title="Parameters",
                    status = "primary", solidHeader = TRUE,
                    column(width=4,
                           checkboxGroupInput("domain_compare",inline=T,
                                              label = "Compare with",
                                              choices = "Loading...")
                    ),
                    column(width=4,
                           sliderInput("domain_sensibility",
                                       label = "Sensibility (sd)",ticks = F,
                                       min = 0,max=1,value = 0.5)
                    ),
                    column(width = 4,
                           sliderInput("domain_Nval",
                                       label = "N valid (not missing) minimum",
                                       ticks = F,min = 0,max=100,value = 30)
                    )
                )
              ),
              navbarPage("Exploration method",
                         tabPanel("Summary",
                                  fluidRow(
                                    box(width = 7,title="Changed Variables",
                                        status = "primary", solidHeader = TRUE,
                                        gt_output("domain_tab_outliers")
                                    ),
                                    box(width = 5,title="Dis/Appeared Variables",
                                        status = "primary", solidHeader = TRUE,
                                        gt_output("domain_tab_check")
                                    )
                                  )),
                         tabPanel("Graphics",
                                  uiOutput("domain_plots_outliers",fill=TRUE)
                         )
              )
      ),
      #### Interviewer Tab ####
      tabItem(tabName = "tab_itw",
              fluidRow(
                box(width = 12,title="Parameters",
                    status = "primary", solidHeader = TRUE,
                    column(width = 2,
                           sliderInput("itw_threshold",
                                       label = "Sensibility",ticks = F,
                                       min = 0,max=10,value = 5)
                    ),
                    column(width = 2,
                           sliderInput("itw_Nrow",
                                       label = "N rows",ticks = F,
                                       min = 0,max=50,value = 20)
                    ),
                    column(width = 2,
                           sliderInput("itw_Nval",
                                       label = "N valid (not missing)",ticks = F,
                                       min = 0,max=50,value = 20)
                    ),
                    column(width = 6,
                           radioButtons(
                             inputId = "itw_choice_heatmap",inline=T,
                             label = "Choose of rows and columns",
                             choices=c("All" = "all",
                                       "Risky rows" = "enq",
                                       "Risky columns" = "var",
                                       "Risky cells" = "both"),
                             selected = "all")
                    )
                )
              ),
              navbarPage("Exploration method",
                         tabPanel("Heatmap",
                                  h2("Anomalies by Variables and Interviewer"),
                                  plotlyOutput("itw_heatmap",height = "800px"),
                                  br()),
                         tabPanel("Row Synthesis",
                                  fluidRow(
                                    box(width = 6,title="Ranking of Interviewer",
                                        status = "primary", solidHeader = TRUE,
                                        DTOutput("itw_ranking")
                                    ),
                                    box(width = 6,title="Listing of variables",
                                        status = "primary", solidHeader = TRUE,
                                        DTOutput("itw_listing")
                                    )
                                  )),
                         tabPanel("Column Synthesis",
                                  fluidRow(
                                    box(width = 6,title="Ranking of Interviewer",
                                        status = "primary", solidHeader = TRUE,
                                        DTOutput("itw_var_ranking")
                                    ),
                                    box(width = 6,title="Listing of variables",
                                        status = "primary", solidHeader = TRUE,
                                        DTOutput("itw_var_listing")
                                    )
                                  ))
              ),
              fluidRow(
                column(width = 6,
                       fluidRow(
                         column(width = 4,
                                checkboxInput("itw_missing",label = "Missings?",
                                            value = TRUE)
                         )
                       )
                ),
                column(width = 6,
                       column(width = 4,
                              actionButton(
                                inputId = "itw_all_distrib",
                                label = "Show Distributions")
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
      ),
      #### Micro données ####
      tabItem(tabName = "tab_data",
              fluidRow(
                box(width = 12,title="Variables to show",
                    status = "info", solidHeader = TRUE,collapsible = TRUE,
                    checkboxGroupInput("data_variables",label = "Choice",
                                       inline=T,choices = character(0)
                    )
                ),
                box(width = 12,title="Micro Data",
                    dataTableOutput("data_table")
                )
              )
      )
    )
  )
)


