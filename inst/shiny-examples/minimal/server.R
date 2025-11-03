library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(vizsurvey)
library(laeken)
library(plotly)

options(shiny.maxRequestSize=100*1024^2)

data(eusilc)
data(ses)
data(api)

values <- reactiveValues()

server <- function(input, output, session) {

  observeEvent(input$load_df_sample, {
    df_name <- input$load_df_sample
    values$df_name <- df_name
    values$DB_INI <- get(df_name)

    df_variables <- classify_variables(values$DB_INI)
    values$variables <- df_variables %>% pull(variable)

    values$prepa_vd <- df_variables %>% filter(type == "Modal") %>% pull(variable)
    values$prepa_vc <- df_variables %>% filter(type == "Continuous") %>% pull(variable)
    values$prepa_itw <- values$prepa_vd[1]

    if (values$df_name == "eusilc") values$prepa_itw <- "db040"
    else if (values$df_name == "ses") values$prepa_itw <- "IDunit"
    else if (values$df_name == "apipop") values$prepa_itw <- "cname"

    updateCheckboxGroupInput(session,"config_vd",inline = TRUE,
                             choices = values$variables,
                             selected = values$prepa_vd)

    updateCheckboxGroupInput(session,"config_vc",inline = TRUE,
                             choices = values$variables,
                             selected = values$prepa_vc)

    updateSelectInput(session,"config_itw",
                      choices = values$variables,
                      selected = values$prepa_itw)

    values$vd <- values$prepa_vd
    values$vc <- values$prepa_vc
    values$itw <- values$prepa_itw
  })

  observeEvent(input$load_df, {
    file <- input$load_df
    ext <- tools::file_ext(file$datapath)
    req(file)
    values$DB_INI <- tibble::as_tibble(data.table::fread(file$datapath))

    df_variables <- classify_variables(values$DB_INI)
    values$variables <- df_variables %>% pull(variable)

    values$prepa_vd <- df_variables %>% filter(type == "Modal") %>% pull(variable)
    values$prepa_vc <- df_variables %>% filter(type == "Continuous") %>% pull(variable)
    values$prepa_itw <- values$prepa_vd[1]

    updateCheckboxGroupButtons(session,"config_vd",size="xs",
                               choices = values$variables,
                               selected = values$prepa_vd,
                               status = "custom-class")

    updateCheckboxGroupButtons(session,"config_vc",size="xs",
                               choices = values$variables,
                               selected = values$prepa_vc,
                               status = "custom-class")

    updateSelectInput(session,"config_itw",
                      choices = values$variables,
                      selected = values$prepa_itw)

    values$vd <- values$prepa_vd
    values$vc <- values$prepa_vc
    values$itw <- values$prepa_itw
  })

  # Configuration des listes de variables
  observeEvent(input$config_vd,{values$vd <- input$config_vd})
  observeEvent(input$config_vc,{values$vc <- input$config_vc})
  observeEvent(input$config_itw,{values$itw <- input$config_itw})

  ##### Création des DB d'analyse ####

  ###### DB complète #####

  DB <- reactive({
    req(values$DB_INI,values$vd,values$vc)

    DB <- values$DB_INI
    vd <- intersect(values$vd, names(DB))
    vc <- intersect(values$vc, names(DB))
    itw <- intersect(values$itw, names(DB))
    vd <- setdiff(vd,itw)
    vc <- setdiff(vc,itw)

    DB <- DB %>%
      mutate_at(vars(vd), empty_as_na) %>%
      mutate_at(vars(vd), as.factor) %>%
      mutate_at(vars(vc), as.numeric) %>%
      mutate_at(vars(itw), as.character)

    return(DB)
  })

  ###### DB de Stats (enquêteurs) #####

  DB_STATS <- reactive({
    req(DB(),values$itw)

    db <- DB()

    # Préparation des variables à analyser
    vd <- intersect(values$vd, names(db))
    vc <- intersect(values$vc, names(db))

    db_stat <- prepa_stats(db,values$itw,vd,vc)

    return(db_stat)
  })

  ###### DB des anomalies (enquêteurs) #####

  DB_ANOMALIES <- reactive({
    req(DB_STATS())

    db_stat <- DB_STATS() %>%
      group_by(!!sym(values$itw),stat) %>%
      filter(abs(standard)>input$itw_threshold) %>%
      filter(Nrow > input$itw_Nrow) %>%
      filter(Nval > input$itw_Nval) %>%
      ungroup()

    return(db_stat)
  })

  output$itw_heatmap <- renderPlotly({
    req(DB_STATS())

    db_stat <- DB_STATS() %>%
      mutate(standard = case_when(
        Nrow < input$itw_Nrow ~ 0,
        !is.na(value_ref) & is.na(value) ~ standard,
        Nval < input$itw_Nval ~ 0,
        TRUE ~ standard))

    db_stat <- db_stat %>%
      filter(
        (type == "cha" & stat %in% c("missing","chi2")) |
          (type == "num" & stat %in% c("missing","median")))

    var_group <- colnames(db_stat)[1]

    if (input$itw_choice_heatmap %in% c("row","both"))
      db_stat <- db_stat %>%
      group_by(!!sym(var_group)) %>%
      filter(max(abs(standard),na.rm=TRUE)>input$itw_threshold) %>%
      ungroup()

    if (input$itw_choice_heatmap %in% c("col","both"))
      db_stat <- db_stat %>%
      group_by(variable) %>%
      filter(max(abs(standard),na.rm=TRUE)>input$itw_threshold) %>%
      ungroup()

    db_stat <- db_stat %>%
      mutate(!!sym(values$itw) := factor(!!sym(values$itw),
                                         levels = sort(unique(!!sym(values$itw)))))

    if (nrow(db_stat) == 0) return(NULL)

    var_group <- colnames(db_stat)[1]

    values$var_group <- var_group
    values$heatmap_vec_vars <- sort(unique(db_stat$variable))
    values$heatmap_vec_itw <- db_stat %>% pull(!!sym(var_group)) %>%
      unique() %>% as.character() %>% sort()

    p <- heat_map_group(db_stat,input$itw_threshold)

    ggplotly(p, tooltip = "text", source = "heatmap_source") %>%
      event_register("plotly_click")
  })

  click_heatmap <- reactive({
    req(DB_STATS())
    event_data("plotly_click", source = "heatmap_source")
  })

  observeEvent(click_heatmap(), {
    d <- event_data("plotly_click", source = "heatmap_source")

    click_var <- values$heatmap_vec_vars[d$x]
    click_itw <- values$heatmap_vec_itw[d$y]

    # Title

    output$itw_distrib_spe_txt <- renderText({
      paste("Distribution for", values$var_group, "=",click_itw)
    })

    output$itw_distrib_glo_text <- renderText({
      paste("Distribution for", values$var_group, "!=",click_itw)
    })

    # Summary

    output$itw_summary_spe <- renderPrint({
      data_enq <- DB() %>% filter(!!sym(values$var_group) == click_itw)
      summarytools::dfSummary(data_enq[,click_var],graph.col=FALSE)
    })

    output$itw_summary_glo <- renderPrint({
      data_glo <- DB() %>% filter(!!sym(values$var_group) != click_itw)
      summarytools::dfSummary(data_glo[,click_var],graph.col=FALSE)
    })

    if (click_var %in% values$vd){
      syn_spe <- DB() %>%
        filter(!!sym(values$var_group) == click_itw) %>%
        count(!!sym(click_var)) %>%
        mutate(prop = n/sum(n),
               !!sym(click_var):=as.character(!!sym(click_var)))

      syn_glo <- DB() %>%
        filter(!!sym(values$var_group) != click_itw) %>%
        count(!!sym(click_var)) %>%
        mutate(prop = n/sum(n),
               !!sym(click_var):=as.character(!!sym(click_var)))

      output$itw_distrib_spe_plot <- renderPlot({
        ggplot(syn_spe)+
          aes(x=!!sym(click_var),y=prop)+
          geom_bar(fill="yellow3",stat="identity") +
          scale_y_continuous(labels = scales::percent) +
          coord_flip()+
          theme_minimal()
      })

      output$itw_distrib_glo_plot <- renderPlot({
        ggplot(syn_glo)+
          aes(x=!!sym(click_var),y=prop)+
          geom_bar(fill="orange3",stat="identity") +
          scale_y_continuous(labels = scales::percent) +
          coord_flip()+
          theme_minimal()
      })
    }

    if (click_var %in% values$vc){

      output$itw_distrib_spe_plot <- renderPlot({
        DB() %>%
          filter(!!sym(values$var_group) == click_itw) %>%
          ggplot()+
          aes(x=!!sym(click_var))+
          geom_histogram(fill="yellow3",color="black") +
          theme_minimal()
      })

      output$itw_distrib_glo_plot <- renderPlot({
        DB() %>%
          filter(!!sym(values$var_group) != click_itw) %>%
          ggplot()+
          aes(x=!!sym(click_var))+
          geom_histogram(fill="orange3",color="black") +
          theme_minimal()
      })
    }
  })
}
