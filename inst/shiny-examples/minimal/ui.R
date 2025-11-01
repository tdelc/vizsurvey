library(shiny)
library(plotly)

ui <- fluidPage(

  # Application title
  titlePanel("Illustation of vizsurvey"),
  sidebarLayout(
    sidebarPanel(width = 2,
                 fileInput('load_df','Load a dataset (csv)',accept='.csv'),
                 radioButtons("load_df_sample",
                              label = "Choose a sample dataset",
                              choices = c("eusilc","ses","apipop"),
                              width = "100%")
    ),
    mainPanel(width = 10,
              h3("Configuration of variable"),
              fluidRow(
                column(width = 5,
                       checkboxGroupInput("config_vd",label = "Discrete Variables",
                                          choices = character(0),width = "100%")
                ),
                column(width = 5,
                       checkboxGroupInput("config_vc",label = "Continuous Variables",
                                          choices = character(0),width = "100%")
                ),
                column(width = 2,
                       selectInput("config_itw", "Variable of the Interviewers", character(0))
                )
              ),
              fluidRow(
                column(width = 3,
                       sliderInput("itw_threshold",
                                   label = "Minimum threshold to show differences",
                                   min = 0,max=10,value = 5)
                ),
                column(width = 3,
                       sliderInput("itw_Nrow",
                                   label = "Minimum number of rows",
                                   min = 0,max=50,value = 20),

                ),
                column(width = 3,
                       sliderInput("itw_Nval",
                                   label = "Minimum number of valid rows",
                                   min = 0,max=50,value = 20)
                ),
                column(width = 3,
                       radioButtons(
                         inputId = "itw_choice_heatmap",
                         label = "Choice of rows and columns",
                         choices=c("All" = "all",
                                   "Rows at risk" = "row",
                                   "Columns at risk" = "col",
                                   "Data at risk" = "both"),
                         selected = "all")
                )
              ),
              fluidRow(
                h2("Heatmap of survey"),
                plotlyOutput("itw_heatmap",height = "800px"),
                fluidRow(
                  column(width = 6,
                         h3(textOutput("itw_distrib_spe_txt")),
                         plotOutput("itw_distrib_spe_plot"),
                         verbatimTextOutput("itw_summary_spe")
                  ),
                  column(width = 6,
                         h3(textOutput("itw_distrib_glo_text")),
                         plotOutput("itw_distrib_glo_plot"),
                         verbatimTextOutput("itw_summary_glo")
                  )
                )
              )
    )
  )
)
