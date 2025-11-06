source("functions.R")

link_data_folder <- getShinyOption("link_data_folder", "data")
data_rds_pattern <- getShinyOption("data_rds_pattern", "global")
depth_folder     <- getShinyOption("depth_folder", 1)

library(DT)
library(summarytools)
library(corrplot)

server <- function(input, output, session) {

  values_ini <- reactiveValues(
    link_data_folder = link_data_folder,
    data_rds_pattern = data_rds_pattern,
    depth_folder     = depth_folder
    )
  values_dis <- reactiveValues()
  values_grp <- reactiveValues()


  #### Database Loading ####

  observeEvent(input$load_df, ignoreInit = T, {
    file <- input$load_df
    ext <- tools::file_ext(file$datapath)
    req(file)
    values_ini$df_user <- tibble::as_tibble(data.table::fread(file$datapath))

    vec_vars <- classify_df(values_ini$df_user)$variable

    showModal(modalDialog(
      title = paste("Import of data.frame",basename(file$datapath)),
      size = "l",easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("load_df_fi",label="Prepare survey")
      ),
      h3(paste0("Choose of survey parameters")),
      selectInput("load_df_var_wave",label = "Wave Variable", choices = c("No",vec_vars)),
      selectInput("load_df_var_zone",label = "Zone Variable", choices = c("No",vec_vars)),
      selectInput("load_df_var_group",label = "Group Variable", choices = c("No",vec_vars))
    ))
  })

  observeEvent(input$load_df_fi, ignoreInit = T,{
    # Survey preparation
    temporary_dir <- tempdir()
    unlink(file.path(temporary_dir, "DATA"),recursive = T)
    dir.create(file.path(temporary_dir, "DATA"))
    link_folder <- file.path(temporary_dir, "DATA")
    readr::write_csv(values_ini$df_user,
                     file=file.path(link_folder,"data_from_r.csv"),
                     col_names = T)

    vars <- classify_df(values_ini$df_user)

    if (input$load_df_var_wave == "No") var_wave <- NULL
    else var_wave <- input$load_df_var_wave

    if (input$load_df_var_zone == "No") var_zone <- NULL
    else var_zone <- input$load_df_var_zone

    if (input$load_df_var_group == "No") var_group <- NULL
    else var_group <- input$load_df_var_group

    create_config(
      folder_path = link_folder,
      file_name   = "config.txt",
      var_wave    = var_wave,
      var_zone    = var_zone,
      var_group   = var_group
    )

    prepa_survey(link_folder)

    values_ini$link_data_folder <- link_folder
    values_ini$data_rds_pattern <- "global"
    values_ini$depth_folder <- 1

    removeModal()
  })

  vec_path_folder <- reactive({
    if (values_ini$depth_folder == 3){
      list.files(values_ini$link_data_folder)
    }else{
      basename(values_ini$link_data_folder)
    }
  })

  observeEvent(vec_path_folder(),{
    path_folder <- vec_path_folder()
    updateRadioButtons(session,"path_folder",inline=T,choices = path_folder)
  })

  observeEvent(input$path_folder, {

    if (values_ini$depth_folder == 1){
      vec_path_survey <- vec_path_folder()
    }

    if (values_ini$depth_folder == 2){
      vec_path_survey <- list.dirs(values_ini$link_data_folder,
                               full.names = TRUE,recursive = FALSE)
    }

    if (values_ini$depth_folder == 3){
      path_folder <- file.path(values_ini$link_data_folder,input$path_folder)
      vec_path_survey <- list.dirs(path_folder,full.names = T,recursive = F)
      values_ini$path_folder <- input$path_folder
    }

    vec_path_survey <- basename(vec_path_survey)

    updateRadioButtons(session,"path_survey",inline = T,
                            choices = sort(vec_path_survey),
                            selected = sort(vec_path_survey)[1])
  })

  observeEvent(input$path_survey, {
    if (depth_folder == 1){
      values_ini$path_survey <- values_ini$link_data_folder
    }
    if (depth_folder == 2){
      values_ini$path_survey <- file.path(values_ini$link_data_folder,
                                          input$path_survey)
    }
    if (depth_folder == 3){
      values_ini$path_survey <- file.path(values_ini$link_data_folder,
                                          input$path_folder,
                                          input$path_survey)
    }
  })

  ##### Load RDS #####

  observeEvent(values_ini$path_survey,{
    path <- values_ini$path_survey
    name_file <- paste0(data_rds_pattern,".rds")
    req(file.exists(file.path(path,name_file)))

    global <- readRDS(file.path(path,name_file))

    values_ini$df             <- global[['df']]
    values_ini$df_stats       <- global[['df_stats']]
    values_ini$df_stats_group <- global[['df_stats_group']]
    values_ini$config         <- global[['configs']]
  })

  ##### Update inputs #####

  observeEvent(values_ini$df,{

    # Data configuration
    modality <- sort(pull(unique(values_ini$df[,values_ini$config$vw])))
    updateRadioButtons(session,"config_wave",inline=T,
                       label=values_ini$config$vw,
                       choices = modality,
                       selected = modality[length(modality)])

    updateCheckboxGroupInput(session,"wave_compare",inline=T,
                             label=values_ini$config$vw,
                             choices = modality,
                             selected = modality[length(modality)-1])

    # Micro-Data
    wave_data <- unique(c(values_ini$config$vw,values_ini$config$vg))

    updateCheckboxGroupInput(session,"data_variables",inline = T,
                             choices = colnames(values_ini$df),
                             selected = wave_data)

    if (length(values_ini$config$vz)){
      modality <- sort(pull(unique(values_ini$df[,values_ini$config$vz])))

      updateRadioButtons(session,"config_zone",inline=T,
                         label=values_ini$config$vz,
                         choices = unique(c("All",modality)),
                         selected = c("All",modality)[1])
    }else{
      updateRadioButtons(session,"config_zone",inline=T,
                         label=values_ini$config$vz,
                         choices = "All")
    }
  })

  verif_init <- reactive({
    if (input$config_wave == "Loading...") return(NULL)
    if (input$config_zone == "Loading...") return(NULL)

    return(TRUE)
  })

  #### reactive df creation ####

  observeEvent(input$config_wave,{
    values_dis$plot_distrib <- NULL
    values_dis$syn_distrib <- NULL
    values_dis$gt_distrib <- NULL
    values_grp$id_group <- NULL
    values_grp$variable <- NULL
  })

  ##### df zone and wave #####

  df_sub <- reactive({
    req(values_ini$df,values_ini$config$vw,verif_init())

    df_sub <- values_ini$df %>%
      filter(!!sym(values_ini$config$vw) %in% input$config_wave)

    if (length(values_ini$config$vz) &&
        input$config_zone %in% pull(df_sub[,values_ini$config$vz]))
      df_sub <- df_sub %>% filter(!!sym(values_ini$config$vz) == input$config_zone)

    return(df_sub)
  })

  ##### df_stats zone and wave #####

  df_stats_sub <- reactive({
    req(verif_init())

    vec_wave <- c(input$config_wave,input$wave_compare)

    df_stats <- values_ini$df_stats
    if (is.null(df_stats)) return(NULL)
    df_stats %>%
      filter(!!sym(values_ini$config$vw) %in% vec_wave,
             zone == input$config_zone)
  })

  ##### df_stats_group zone and wave #####
  df_stats_group_sub <- reactive({
    req(values_ini$df_stats_group,verif_init())

    values_ini$df_stats_group %>%
      filter(!!sym(values_ini$config$vw) == input$config_wave,
             zone == input$config_zone)
  })


  #### wave outliers ####

  ##### reactive df outliers #####

  df_stats_outliers <- reactive({

    req(df_stats_sub())

    db_longer <- df_stats_sub() %>%
      select(variable,!!sym(values_ini$config$vw),Nrow,Nval,type,
             stat,value,standard,value_ref) %>%
      filter(!stat %in% c("chi2")) %>%
      group_by(variable,stat) %>%
      mutate(value = case_when(
        sum(Nval,na.rm = TRUE) < input$wave_Nval ~ 0,
        TRUE ~ value
      )) %>%
      ungroup() %>%
      select(-standard,-Nrow,-Nval,-value_ref) %>%
      # group_by(variable,stat) %>%
      group_by(variable,stat) %>%
      # arrange(!!sym(values_ini$config$vw)) %>%
      # mutate(sd = (value-lag(value))/lag(value),
      mutate(sd = sd(value,na.rm=T)/mean(value,na.rm=T),
             sd = tidyr::replace_na(sd,0)) %>%
      ungroup() %>%
      arrange(variable,stat)

    db_longer %>%
      arrange(!!sym(values_ini$config$vw),stat) %>%
      pivot_wider(
        names_from = c(!!sym(values_ini$config$vw),stat),
        names_sep = "|",
        values_from = c(value,sd)
      )
  })

  prepa_wave_tab_outliers <- reactive({
    df_stats_outliers() %>%
      filter(if_any(starts_with("sd|"), ~ abs(.x) > input$wave_sensibility),
             if_all(matches("^value\\|.*\\|presence$"), ~ .x == 1)
             ) %>%
      select(-matches("presence$"))
  })

  output$wave_tab_outliers <- render_gt({
    tidy_to_gt(prepa_wave_tab_outliers(),input$wave_sensibility)
  })

  output$wave_tab_check <- render_gt({

    df_stats_outliers <- df_stats_outliers() %>%
      select(variable, type, matches("^value.*presence$"),
             matches("^sd.*presence$")) %>%
      filter(if_any(starts_with("sd|"), ~ abs(.x) > 0))

    tidy_to_gt(df_stats_outliers,2)
  })

  ##### Outliers graphics #####

  vars_outliers <- reactive({
    sort(unique(prepa_wave_tab_outliers()$variable))
  })

  output$wave_plots_outliers <- renderUI({

    req(vars_outliers())

    plot_output_list <- vars_outliers() %>% map(~{
      plot_name_wave <- paste0(.x, "_plot_wave")
      plot_name_evol <- paste0(.x, "_plot_evol")
      list(
        fluidRow(
          column(10, h4(.x))
        ),
        fluidRow(
          column(6, plotOutput(plot_name_wave, height = "400px")),
          column(6, plotOutput(plot_name_evol, height = "400px"))
        ), tags$br()
      )
    })

    tagList(
      fluidRow(
        column(6,h3("Distribution")),
        column(6,h3("Comparaison"))
      ),
      do.call(tagList, unlist(plot_output_list, recursive = FALSE))
    )
  })

  observeEvent(vars_outliers(), {

    vec_wave <- c(input$config_wave,input$wave_compare)

    df_zone_compa <- values_ini$df %>%
      filter(!!sym(values_ini$config$vw) %in% vec_wave)

    if (length(values_ini$config$vz) &&
        input$config_zone %in% pull(df_sub()[,values_ini$config$vz])){
      df_zone_compa <- df_zone_compa %>%
        filter(!!sym(values_ini$config$vz) == input$config_zone)
    }

    lapply(vars_outliers(), function(i) {
      local({
        my_i <- i
        plot_wave_name <- paste(my_i, "_plot_wave", sep = "")
        plot_evol_name <- paste(my_i, "_plot_evol", sep = "")

        if (my_i %in% values_ini$config$vd){
          output[[plot_wave_name]] <- renderPlot({

            syn <- df_sub() %>%
              count(!!sym(my_i)) %>%
              mutate(prop = n/sum(n),
                     !!sym(my_i):=as.character(!!sym(my_i)))

            ggplot(syn)+
              aes(x=!!sym(my_i),fill=!!sym(my_i),y=prop)+
              geom_bar(stat="identity") +
              scale_y_continuous(labels = scales::percent) +
              coord_flip() +
              theme_minimal(base_size = 15)
          })

          output[[plot_evol_name]] <- renderPlot({
            ggplot(df_zone_compa)+
              aes(x=!!sym(values_ini$config$vw),fill=!!sym(my_i))+
              geom_bar(position="fill") +
              scale_y_continuous(labels = scales::percent) +
              coord_flip() +
              theme_minimal(base_size = 15)
          })
        }

        if (my_i %in% values_ini$config$vc){
          output[[plot_wave_name]] <- renderPlot({
            ggplot(df_sub())+
              aes(x=!!sym(my_i))+
              geom_histogram(color="grey",fill="black") +
              theme_minimal(base_size = 15)
          })

          output[[plot_evol_name]] <- renderPlot({
            ggplot(df_zone_compa)+
              aes(x=!!sym(my_i))+
              geom_histogram(color="grey",fill="black") +
              theme_minimal(base_size = 15)+
              facet_wrap(vars(!!sym(values_ini$config$vw)),ncol=1)
          })
        }
      })
    })
  })

  #### Group Outliers ####

  observeEvent(input$group_subtab_choice, {
    updateTabsetPanel(session, "group_subtab",
                      selected = input$group_subtab_choice)
  })

  ##### Reactive df outliers #####

  df_stats_group_outliers <- reactive({
    req(df_stats_group_sub())

    df_stats_group_sub() %>%
      filter(stat != "presence",!type == "txt") %>%
      group_by(!!sym(values_ini$config$vg),stat) %>%
      filter(abs(standard)>input$group_threshold,
             Nrow > input$group_Nrow,
             Nval > input$group_Nval) %>%
      ungroup()
  })

  ##### Table and Ranking #####

  ###### Synthesis by row ######

  prepa_group_ranking <- reactive({

    req(df_stats_group_sub())

    df_count <- df_stats_group_sub() %>%
      filter(stat %in% c("chi2","median"),
             Nrow >= input$group_Nrow,
             Nval >= input$group_Nval,
             standard >= input$group_threshold) %>%
      group_by(!!sym(values_ini$config$vg)) %>%
      count(name="N_outliers")

    df_prepa <- df_stats_group_sub() %>%
      filter(stat %in% c("chi2","median"),Nrow > input$group_Nrow) %>%
      select(!!sym(values_ini$config$vg),!!sym(values_ini$config$vw),
             zone,variable,standard,Nrow) %>%
      pivot_wider(
        id_cols = c(!!sym(values_ini$config$vg),
                    !!sym(values_ini$config$vw),
                    zone,Nrow),
        names_from = variable,
        values_from = standard
      )

    out <- df_prepa %>%
      group_by(!!sym(values_ini$config$vw),zone) %>%
      mutate(score = score_isoforest(across(where(is.numeric))),
             score = round(score*100,1)) %>%
      ungroup() %>%
      left_join(df_count) %>%
      arrange(desc(score))

    return(out)
  })

  output$group_ranking <- renderDataTable({

    df <- prepa_group_ranking() %>%
      select(!!sym(values_ini$config$vg),Nrow,N_outliers,score)

    dt <- datatable(df, filter='top', selection = 'single',escape   = FALSE,
                    options = list(pageLength = 15,dom = 'tp'),
                    rownames = F)
    dt
  })

  prepa_group_listing <- reactive({
    req(input$group_ranking_rows_selected)
    s <- input$group_ranking_rows_selected
    id_group <- pull(prepa_group_ranking()[s,values_ini$config$vg])
    if(length(s)){
      out <- prepa_group_ranking() %>%
        select(-Nrow,-N_outliers,-score,-!!sym(values_ini$config$vw),-zone) %>%
        filter(!!sym(values_ini$config$vg) == id_group) %>%
        pivot_longer(cols = where(is.numeric),names_to = "variable",
                     values_to = "Difference") %>%
        mutate(Difference = round(Difference,1)) %>%
        arrange(desc(abs(Difference)))
    }else{
      out <- NULL
    }
    return(out)
  })

  output$group_listing <- renderDataTable({
    df <- prepa_group_listing()

    dt <- datatable(df, filter='top', selection = 'single',
                    escape   = FALSE,
                    options = list(pageLength = 15,dom = 'tp'),
                    rownames = F)

    dt
  })

  observeEvent(input$group_listing_rows_selected, {
    req(input$group_listing_rows_selected)
    s <- input$group_listing_rows_selected

    values_grp$variable <- pull(prepa_group_listing()[s,2])
    values_grp$id_group <- pull(prepa_group_listing()[s,1])
  })

  ###### Synthesis by column ######

  prepa_var_ranking <- reactive({

    req(df_stats_group_sub())

    df_count <- df_stats_group_sub() %>%
      filter(stat %in% c("chi2","median"),
             Nrow >= input$group_Nrow,
             Nval >= input$group_Nval,
             standard >= input$group_threshold) %>%
      group_by(variable) %>%
      count(name="N_outliers")

    df_prepa <- df_stats_group_sub() %>%
      filter(stat %in% c("chi2","median"),Nrow > input$group_Nrow) %>%
      select(!!sym(values_ini$config$vg),!!sym(values_ini$config$vw),
             zone,variable,standard) %>%
      pivot_wider(
        id_cols = c(!!sym(values_ini$config$vw),variable,zone),
        names_from = !!sym(values_ini$config$vg),
        values_from = standard
      )

    out <- df_prepa %>%
      group_by(!!sym(values_ini$config$vw),zone) %>%
      mutate(score = score_isoforest(across(where(is.numeric))),
             score = round(score*100,1)) %>%
      ungroup() %>%
      left_join(df_count) %>%
      arrange(desc(score))

    return(out)
  })

  output$group_var_ranking <- renderDataTable({

    df <- prepa_var_ranking() %>%
      select(variable,N_outliers,score)

    dt <- datatable(df, filter='top', selection = 'single',escape   = FALSE,
                    options = list(pageLength = 15,dom = 'tp'),
                    rownames = F)
    dt
  })

  prepa_var_listing <- reactive({
    req(input$group_var_ranking_rows_selected)
    s <- input$group_var_ranking_rows_selected
    id_variable <- pull(prepa_var_ranking()[s,"variable"])
    if(length(s)){
      out <- prepa_var_ranking() %>%
        filter(variable == id_variable) %>%
        select(-N_outliers,-score,-!!sym(values_ini$config$vw),-zone) %>%
        pivot_longer(cols = where(is.numeric),
                     names_to = values_ini$config$vg,
                     values_to = "Difference") %>%
        mutate(Difference = round(Difference,1)) %>%
        arrange(desc(abs(Difference)))
    }else{
      out <- NULL
    }
    return(out)
  })

  output$group_var_listing <- renderDataTable({
    df <- prepa_var_listing()

    dt <- datatable(df, filter='top', selection = 'single',
                    escape   = FALSE,
                    options = list(pageLength = 15,dom = 'tp'),
                    rownames = F)

    dt
  })

  observeEvent(input$group_var_listing_rows_selected, {
    req(input$group_var_listing_rows_selected)
    s <- input$group_var_listing_rows_selected

    values_grp$variable <- pull(prepa_var_listing()[s,1])
    values_grp$id_group <- pull(prepa_var_listing()[s,2])
  })

  ##### Prepare Heatmap ####

  prepa_heatmap <- reactive({
    req(verif_init(),df_stats_group_sub())
    df_stats <- df_stats_group_sub() %>%
      mutate(standard = case_when(
        Nrow < input$group_Nrow ~ 0,
        !is.na(value_ref) & is.na(value) ~ standard,
        Nval < input$group_Nval ~ 0,
        TRUE ~ standard))

    df_stats <- df_stats %>%
      filter(
        (type == "cha" & stat %in% c("missing","chi2")) |
          (type == "num" & stat %in% c("missing","median")))

    if (input$group_choice_heatmap %in% c("enq","both"))
      df_stats <- df_stats %>%
      group_by(!!sym(values_ini$config$vg)) %>%
      filter(max(abs(standard),na.rm=TRUE)>input$group_threshold) %>%
      ungroup()

    if (input$group_choice_heatmap %in% c("var","both"))
      df_stats <- df_stats %>%
      group_by(variable) %>%
      filter(max(abs(standard),na.rm=TRUE)>input$group_threshold) %>%
      ungroup()

    df_stats <- df_stats %>%
      mutate(!!sym(values_ini$config$vg) := factor(
        !!sym(values_ini$config$vg),levels = sort(
          unique(!!sym(values_ini$config$vg)))))

    if (nrow(df_stats) == 0) return(NULL)

    values_grp$heatmap_group <- df_stats %>%
      pull(!!sym(values_ini$config$vg)) %>%
      unique() %>% as.character() %>% sort()

    values_grp$heatmap_var <- sort(unique(df_stats$variable))

    return(df_stats)
  })

  output$group_heatmap <- renderPlotly({

    req(prepa_heatmap())

    p <- heatmap_group(prepa_heatmap(),input$group_threshold)

    ggplotly(p, tooltip = "text", source = "heatmap_source") %>%
      event_register("plotly_click")
  })

  ##### Reaction to heatmap #####

  df_sub_details <- reactive({
    req(df_sub(),values_grp$variable)
    df_sub <- df_sub()
    if (!input$group_missing){
      df_sub <- df_sub %>% filter(!is.na(!!sym(values_grp$variable)))
    }
    return(df_sub)
  })

  observeEvent(event_data("plotly_click", source="heatmap_source"), {
    d <- event_data("plotly_click", source="heatmap_source")
    req(d)
    values_grp$variable <- values_grp$heatmap_var[d$x]
    values_grp$id_group   <- values_grp$heatmap_group[d$y]
  })

  ###### Reactive text in box #####

  output$group_distrib_spe_text <- renderText({
    req(values_grp$id_group)
    paste("Distribution for", values_ini$config$vg, "=",values_grp$id_group)
  })

  output$group_distrib_glo_text <- renderText({
    req(values_grp$id_group)
    paste("Distribution for", values_ini$config$vg, "!=",values_grp$id_group)
  })

  output$group_corr_spe_text <- renderText({
    req(values_grp$id_group)
    paste("Correlations for", values_ini$config$vg, "=",values_grp$id_group)
  })

  output$group_corr_glo_text <- renderText({
    req(values_grp$id_group)
    paste("Correlations for", values_ini$config$vg, "!=",values_grp$id_group)
  })

  ###### Reactive Distributions #####

  output$group_distrib_spe <- renderPlot({

    validate(
      need(values_grp$id_group, 'Choose a group.'),
      need(values_grp$variable, 'Choose a variable.')
    )

    syn_spe <- df_sub_details() %>%
      filter(!!sym(values_ini$config$vg) == values_grp$id_group)

    if (values_grp$variable %in% values_ini$config$vc){
      p <- ggplot(syn_spe)+
        aes(x=!!sym(values_grp$variable))+
        geom_histogram(color="purple3",fill="#ee82ee") +
        theme_minimal()

    } else if (values_grp$variable %in% values_ini$config$vd){

      syn_spe <- syn_spe %>%
        count(!!sym(values_grp$variable)) %>%
        mutate(prop = n/sum(n),
               !!sym(values_grp$variable):=as.character(!!sym(values_grp$variable)))

      p <- ggplot(syn_spe)+
        aes(x=!!sym(values_grp$variable),y=prop)+
        geom_bar(fill="yellow3",stat="identity") +
        scale_y_continuous(labels = scales::percent) +
        coord_flip()+
        theme_minimal()
    }
    p + theme_minimal(base_size = 15)
  })

  output$group_distrib_glo <- renderPlot({

    validate(
      need(values_grp$id_group, 'Choose a group.'),
      need(values_grp$variable, 'Choose a variable.')
    )

    syn_glo <- df_sub_details() %>%
      filter(!!sym(values_ini$config$vg) != values_grp$id_group)

    if (values_grp$variable %in% values_ini$config$vc){
      p <- ggplot(syn_glo)+
        aes(x=!!sym(values_grp$variable))+
        geom_histogram(color="#00708C",fill="mediumturquoise")
    } else if (values_grp$variable %in% values_ini$config$vd){
      syn_glo <- syn_glo %>%
        dplyr::count(!!sym(values_grp$variable)) %>%
        mutate(prop = n/sum(n),
               !!sym(values_grp$variable):=as.character(!!sym(values_grp$variable)))

      p <- syn_glo %>%
        ggplot()+
        aes(x=!!sym(values_grp$variable),y=prop)+
        geom_bar(fill="orange3",stat="identity") +
        scale_y_continuous(labels = scales::percent) +
        coord_flip()
    }
    p + theme_minimal(base_size = 15)
  })

  ###### Reactive Summary ######

  output$group_summary_spe <- renderPrint({
    req(df_sub_details())
    data_group <- df_sub_details() %>%
      filter(!!sym(values_ini$config$vg) == values_grp$id_group)
    dfSummary(data_group[,values_grp$variable],graph.col=FALSE)
  })

  output$group_summary_glo <- renderPrint({
    req(df_sub_details())
    data_group <- df_sub_details() %>%
      filter(!!sym(values_ini$config$vg) != values_grp$id_group)
    dfSummary(data_group[,values_grp$variable],graph.col=FALSE)
  })

  ###### Reactive Correlations ######

  prepa_corr <- reactive({
    req(values_ini$config$vg,df_sub_details())

    id <- values_ini$config$vg
    df <- df_sub_details()
    df <- df %>%
      select(-all_of(id)) %>%
      mutate(across(where(is.character), as.factor),
             across(where(lubridate::is.Date), as.factor),
             across(where(is.factor), ~ as.numeric(.x))) %>%
      select(where(is.numeric)) %>%
      dplyr::bind_cols(df[all_of(id)])

    df <- df %>% select(where(~ sum(!is.na(.x)) >= input$group_Nrow))
    df <- df %>% select(where(~ sum(!is.na(.x)) >= input$group_Nval))

    vars_corr <- df %>%
      select(-!!sym(values_ini$config$vg)) %>%
      cor(y = .[[values_grp$variable]],
          use = "pairwise.complete.obs", method= "pearson") %>%
      as.table %>% as.data.frame %>%
      mutate(Freq_abs = abs(Freq)) %>%
      dplyr::slice_max(Freq_abs,n=10,with_ties = FALSE) %>%
      pull(Var1) %>% as.character()

    df %>% select(!!sym(values_ini$config$vg),any_of(vars_corr))
  })

  output$group_corr_spe <- renderPlot({
    M <- prepa_corr() %>%
      filter(!!sym(values_ini$config$vg) == values_grp$id_group) %>%
      select(-!!sym(values_ini$config$vg)) %>%
      cor(use = "pairwise.complete.obs",method="spearman")

    output$group_main_corr_spe <- renderPlot(
      group_main_corr(M,values_grp$variable)  +
        labs(title = paste("10 First Correlations with",
                           values_grp$variable))
    )

    corrplot(M,type="upper", tl.col="grey20", tl.srt=45)
  })

  output$group_corr_glo <- renderPlot({
    M <- prepa_corr() %>%
      filter(!!sym(values_ini$config$vg) != values_grp$id_group) %>%
      select(-!!sym(values_ini$config$vg)) %>%
      cor(use = "pairwise.complete.obs",method="spearman")

    output$group_main_corr_glo <- renderPlot(
      group_main_corr(M,values_grp$variable)  +
        labs(title = paste("10 First Correlations with",
                           values_grp$variable))
      )
    corrplot(M,type="upper", tl.col="grey20", tl.srt=45)
  })

  ###### All distributions ######

  observeEvent(input$group_all_distrib,{

    data_glo <- df_sub_details() %>%
      select(!!sym(values_ini$config$vg),!!sym(values_grp$variable))

    sub_var <- data_glo %>% pull(!!sym(values_grp$variable))
    fl_discrete <- length(sub_var) < 15 | length(unique(sub_var)) < 15

    if (fl_discrete){

      showModal(modalDialog(
        title = paste("Analysis of distribution of variable ",values_grp$variable),
        size = "l",easyClose = TRUE,
        h3(paste0("Group position")),
        radioButtons(
          inputId = "config_modalities_group",label = "Modality",inline = T,
          choices = "Loading..."),
        renderPlot(values_dis$plot_distrib),
        h3(paste0("List of distributions")),
        render_gt(values_dis$gt_distrib)
      ))

      values_dis$syn_distrib <- data_glo %>%
        group_by(!!sym(values_ini$config$vg),!!sym(values_grp$variable)) %>%
        count() %>%
        group_by(!!sym(values_ini$config$vg)) %>%
        mutate(N = sum(n),prop = n/N) %>%
        ungroup() %>%
        select(!!sym(values_ini$config$vg),!!sym(values_grp$variable),N,prop) %>%
        arrange(!!sym(values_grp$variable)) %>%
        pivot_wider(names_from=!!sym(values_grp$variable),values_from = prop) %>%
        arrange(desc(N))

      modalities <- sort(colnames(values_dis$syn_distrib[,-c(1:2)]))

      updateRadioButtons(session,"config_modalities_group",inline = T,
                         choices = modalities,selected = modalities[1])
    }
  })

  observeEvent(values_dis$syn_distrib,{
    values_dis$gt_distrib <- values_dis$syn_distrib %>%
      gt() %>%
      fmt_percent(columns = -c(1,2),decimals = 1,drop_trailing_zeros = TRUE) %>%
      data_color(
        rows = !!sym(values_ini$config$vg) == values_grp$id_group,
        direction = "row",palette = c("red")
      )
  })

  observeEvent(input$config_modalities_group,{

    syn_distrib <- df_sub() %>%
      group_by(!!sym(values_ini$config$vg),!!sym(values_grp$variable)) %>%
      dplyr::count() %>%
      group_by(!!sym(values_ini$config$vg)) %>%
      mutate(N = sum(n),prop = n/N) %>%
      ungroup() %>%
      tidyr::complete(!!sym(values_ini$config$vg),!!sym(values_grp$variable),
               fill=list(n=0,prop=0))

    if (input$config_modalities_group == "NA"){
      syn_distrib <- syn_distrib %>%
        filter(is.na(!!sym(values_grp$variable)))
    }else{
      syn_distrib <- syn_distrib %>%
        filter(as.character(!!sym(values_grp$variable)) == input$config_modalities_group)
    }

    try({
      group_value <- syn_distrib %>%
        filter(!!sym(values_ini$config$vg) == values_grp$id_group) %>%
        pull(prop)

      values_dis$plot_distrib <-  syn_distrib %>%
        ggplot() +
        aes(x = prop) +
        geom_density(fill = "sienna2",alpha = 0.65) +
        theme_minimal(base_size = 15) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                           limits = c(0,1)) +
        labs(x = paste("Proportion of",input$config_modalities_group),
             y = "Density") +
        geom_vline(xintercept = group_value, color = "red",
                   linetype = "dashed", linewidth = 1.2) +
        annotate("text", x = group_value, y = 0,
                 label = paste0("Group : ", round(group_value*100, 1),"%"),
                 vjust = -0.5, hjust = 1.1, color = "red")
    },silent = TRUE)

  })

  #### Data ####

  output$data_table <- renderDataTable({

    # out <- values_ini$df[, colnames(values_ini$df) %in% input$data_variables]
    out <- values_ini$df[, intersect(input$data_variables, names(values_ini$df)), drop = FALSE]

    # na_as_empty <- function(vec){
    #   ifelse(is.na(vec),"NA",vec)
    # }
    # out <- out %>%
    #   mutate_all(as.character) %>%
    #   mutate_all(na_as_empty)

    out <- out %>%
      mutate(across(everything(), as.character),
             across(everything(), ~ ifelse(is.na(.x), "NA", .x)))

    datatable(out,
              filter = 'top',
              options = list(pageLength = 20),
              rownames = FALSE)
  })

  #### Aide à l'utilisation ####

  output$texte_aide <- renderUI({
    aide <- switch(input$sidebar,  # "tabs" = l'id de ton onglet principal
                   "tab_heatmap" = HTML("
      <p><strong>🧠 Objectif</strong></p>
      Contrôler la plausibilité des réponses encodées par les enquêteurs.

        Le heatmap permet en un seul coup d’œil de repérer les résultats surprenants ou anormaux dans les questionnaires remplis par les enquêteur·rices.</p>

        <p><strong>🧩 Comment ça fonctionne ?</strong><br>
        Chaque ligne du heatmap représente un·e enquêteur·rice.<br>
        Chaque colonne représente une variable du questionnaire.<br>
        La couleur indique l’écart par rapport à la distribution attendue.</p>

        <p><strong>🌡️ Calcul</strong><br>
        - Catégorielles : test du Chi²<br>
        - Continues : comparaison des médianes<br>
        - Valeurs manquantes : % de NA par enquêteur</p>

        Un clic sur une cellule ouvre la comparaison détaillée de la distribution des réponses de l’enquêteur ciblé avec celle de ses pairs pertinents (même région/province). <br>

        Des filtres automatiques proposent : <br>

- seulement les enquêteurs à contrôler (valeur khi² sous un certain seuil, ou ayant réalisé au moins X enquêtes), <br>
- seulement les variables sensibles, <br>
- ou les couples enquêteur-variable jugés à risque.

Il est possible d’ajouter des flags directement depuis la heatmap pour tracer la validation des cas suspects. "
                                        ),
                   "tab_enqueteur" = HTML("
      <p><strong>🧠 Objectif </strong></p>
      Identification rapide des enquêteurs « à risque », c'est à dire avec des performances atypiques ou productivité anormale.<br>

<p>Une ligne par enquêteur présentant un ensemble d’indicateurs globaux : nombre d’entretiens codés (médiane, maximum), vitesses médianes sur des items clés et sur l’ensemble du questionnaire, etc.</p>

<p>il est possible de trier selon chaque indicateur pour faire émerger les situations à risque.</p>
    "),
                   "tab_data" = HTML("
                    <p><strong>🧠 Objectifs</strong></p>
      Table détaillée des observations, filtrable à la volée, permettant la vérification ponctuelle des réponses individuelles. </p>
    "),
                   "tab_variable" = HTML("
      <p><strong>🧠 Objectifs</strong></p>
      Idéal pour les vérifications de routing des enquêtes<br> <br>

      <p><strong>👁 Explications des onglets</strong></p>
      <p>📝 Synthèse</p>
      La synthèse reprend les variables ayant changé entre l'an passé et cette année. Il est possible de faire varier le taux de variation à partir duquel la variable est indiqué comme à risque.<br>

      <p>📊 Détails</p>
      Le détail reprend, pour chaque variable avec une anomalie, deux graphiques :<br>

      - Distribution : pour chaque variable, la répartition des réponses à la dernière période disponible.<br>
      -Évolution temporelle : courbes longitudinales permettant de déceler rapidement toute dérive ou rupture de tendance
    "),
                   "tab_detail" = HTML("
      <p><strong>🧠 Objectifs</strong></p>
      Espace libre «bac à sable» où l’utilisateur·rice compose ses propres graphiques : choix des variables, et des filtres (région, période, etc.) pour explorer plus finement les données.
    "),
                   HTML("<p>Aucune aide disponible pour cet onglet.</p>")
    )
    aide
  })

}

