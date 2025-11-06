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
      menuItem("Group", tabName = "tab_group"),
      menuItem("Wave", tabName = "tab_wave"),
      menuItem("Data", tabName = "tab_data")
    )
  ),
  body = dashboardBody(
    #### Configure Zone and Wave ####
    fluidRow(
      box(width = 5,title="Database loading",
          status = "primary", solidHeader = TRUE,
          column(width = 8,
            radioButtons(
              inputId = "path_folder",label = "List of directories",
              choices = "Loading..."),
            radioButtons(
              inputId = "path_survey",label = "List of databases",
              choices = "Loading...")
          ),
          column(width = 4,
            fileInput('load_df','Load a dataset (csv)',accept='.csv')
          )
      ),
      box(width = 7,title = "Database filtering",
          status = "primary",solidHeader = TRUE,
          fluidRow(
            column(width = 6,
                   radioButtons("config_wave",label = "Wave",
                                choices = "Loading...")),
            column(width = 6,
                   radioButtons("config_zone",label = "Zone",
                                choices = "Loading..."))
          )
      )
    ),
    tabItems(
      #### Wave Tab ####
      tabItem(tabName = "tab_wave",
              fluidRow(
                box(width = 12,title="Parameters",
                    status = "primary", solidHeader = TRUE,
                    column(width=4,
                           checkboxGroupInput("wave_compare",inline=T,
                                              label = "Compare with",
                                              choices = "Loading...")
                    ),
                    column(width=4,
                           sliderInput("wave_sensibility",
                                       label = "Sensibility (sd)",ticks = F,
                                       min = 0,max=1,value = 0.5)
                    ),
                    column(width = 4,
                           sliderInput("wave_Nval",
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
                                        gt_output("wave_tab_outliers")
                                    ),
                                    box(width = 5,title="Dis/Appeared Variables",
                                        status = "primary", solidHeader = TRUE,
                                        gt_output("wave_tab_check")
                                    )
                                  )),
                         tabPanel("Graphics",
                                  uiOutput("wave_plots_outliers",fill=TRUE)
                         )
              )
      ),
      #### Interviewer Tab ####
      tabItem(tabName = "tab_group",
              fluidRow(
                box(width = 12,title="Parameters",
                    status = "primary", solidHeader = TRUE,
                    column(width = 2,
                           sliderInput("group_threshold",
                                       label = "Sensibility",ticks = F,
                                       min = 0,max=10,value = 5)
                    ),
                    column(width = 2,
                           sliderInput("group_Nrow",
                                       label = "N rows",ticks = F,
                                       min = 0,max=50,value = 20)
                    ),
                    column(width = 2,
                           sliderInput("group_Nval",
                                       label = "N valid (not missing)",ticks = F,
                                       min = 0,max=50,value = 20)
                    ),
                    column(width = 6,
                           radioButtons(
                             inputId = "group_choice_heatmap",inline=T,
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
                                  h2("Anomalies by Variables and Group"),
                                  plotlyOutput("group_heatmap",height = "800px"),
                                  br()),
                         tabPanel("Row Synthesis",
                                  fluidRow(
                                    box(width = 6,title="Ranking of Group",
                                        status = "primary", solidHeader = TRUE,
                                        dataTableOutput("group_ranking")
                                    ),
                                    box(width = 6,title="Listing of variables",
                                        status = "primary", solidHeader = TRUE,
                                        dataTableOutput("group_listing")
                                    )
                                  )),
                         tabPanel("Column Synthesis",
                                  fluidRow(
                                    box(width = 6,title="Ranking of Group",
                                        status = "primary", solidHeader = TRUE,
                                        dataTableOutput("group_var_ranking")
                                    ),
                                    box(width = 6,title="Listing of variables",
                                        status = "primary", solidHeader = TRUE,
                                        dataTableOutput("group_var_listing")
                                    )
                                  ))
              ),
              fluidRow(
                column(width = 6,
                       fluidRow(
                         column(width = 4,
                                checkboxInput("group_missing",label = "Missings?",
                                            value = TRUE)))
                ),
                column(width = 6,
                       column(width = 4,
                              actionButton(
                                inputId = "group_all_distrib",
                                label = "Show Distributions"))
                )
              ),
              # Graphiques
              fluidRow(
                column(width = 6,
                       box(width = 12,title=textOutput("group_distrib_spe_text"),
                           status = "primary", solidHeader = TRUE,
                           plotOutput("group_distrib_spe"),
                           verbatimTextOutput("group_summary_spe")
                       )
                ),
                column(width = 6,
                       box(width = 12,title=textOutput("group_distrib_glo_text"),
                           status = "primary", solidHeader = TRUE,
                           plotOutput("group_distrib_glo"),
                           verbatimTextOutput("group_summary_glo")
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(width = 12,title=textOutput("group_corr_spe_text"),
                           status = "primary", solidHeader = TRUE,
                           plotOutput("group_main_corr_spe",height = "200px"),
                           br(),plotOutput("group_corr_spe")
                       )
                ),
                column(width = 6,
                       box(width = 12,title=textOutput("group_corr_glo_text"),
                           status = "primary", solidHeader = TRUE,
                           plotOutput("group_main_corr_glo",height = "200px"),
                           br(),plotOutput("group_corr_glo")
                       )
                )
              )
      ),
      #### Micro données ####
      tabItem(tabName = "tab_data",
              fluidRow(
                box(width = 12,title="Variables to show",
                    status = "primary", solidHeader = TRUE,
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


