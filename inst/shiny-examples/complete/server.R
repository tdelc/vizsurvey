source("functions.R")

link_data_folder <- getShinyOption("link_data_folder", "data")
data_rds_pattern <- getShinyOption("data_rds_pattern", "global")
is_double_folder <- getShinyOption("is_double_folder", FALSE)

library(summarytools)
library(corrplot)

i18n <- Translator$new(translation_csvs_path = "i18n")
i18n$set_translation_language(i18n$get_key_translation())

server <- function(input, output, session) {

  # Quand la langue change :
  observeEvent(input$lang, ignoreInit = TRUE, {
    i18n$set_translation_language(input$lang)
    shiny.i18n::update_lang(input$lang)

    updateRadioGroupButtons(session,"itw_choice_heatmap",
                            status = "custom-class",
                            choiceValues = c("all","enq","var","both"),
                            choiceNames = i18n$t(c("All","Risky rows",
                                                   "Risky columns",
                                                   "Risky data")),
                            selected = "all")
  })

  values_ini <- reactiveValues(vec_path_folder = list.files(link_data_folder))
  values_dis <- reactiveValues()
  values_itw <- reactiveValues()

  #### Database Loading ####

  observeEvent(values_ini$vec_path_folder,{
    if (is_double_folder)
      path_folder <- sort(values_ini$vec_path_folder)
    else
      path_folder <- "-"

    updateRadioGroupButtons(session,"path_folder",status = "custom-class",
                            choices = path_folder)
  })

  observeEvent(input$path_folder, {

    if (is_double_folder){
      path_folder <- file.path(link_data_folder,input$path_folder)
      vec_path_survey <- list.dirs(path_folder,full.names = T,recursive = F)

      values_ini$path_folder <- input$path_folder

    }else{
      vec_path_survey <- list.dirs(link_data_folder,
                                   full.names = TRUE,recursive = FALSE)

      values_ini$path_folder <- "-"
    }
    vec_path_survey <- basename(vec_path_survey)

    if (length(vec_path_survey) == 0) vec_path_survey <- "-"
    size_path_survey <- ifelse(length(vec_path_survey) > 10,"xs","normal")
    updateRadioGroupButtons(session,"path_survey",status = "custom-class",
                            size = size_path_survey,
                            choices = sort(vec_path_survey),
                            selected = sort(vec_path_survey)[1])
  })

  observeEvent(input$path_survey, {
    if (is_double_folder)
      values_ini$path_survey <- file.path(values_ini$path_folder,input$path_survey)
    else
      values_ini$path_survey <- input$path_survey
  })


  ##### Load RDS #####

  observeEvent(values_ini$path_survey,{
    path <- file.path(link_data_folder,values_ini$path_survey)
    name_file <- paste0(data_rds_pattern,".rds")
    req(file.exists(file.path(path,name_file)))

    global <- readRDS(file.path(path,name_file))

    values_ini$df           <- global[['df']]
    values_ini$df_stats     <- global[['df_stats']]
    values_ini$df_stats_itw <- global[['df_stats_itw']]
    values_ini$config       <- global[['configs']]

    print(values_ini$df)
    print(values_ini$df_stats)
    print(values_ini$df_stats_itw)
    print(values_ini$config)
  })

  ##### Update inputs #####

  observeEvent(values_ini$df,{

    # Data configuration
    modality <- sort(pull(unique(values_ini$df[,values_ini$config$vt])))
    updateRadioGroupButtons(session,"config_domain",size="xs",
                            label=values_ini$config$vt,
                            choices = modality,
                            selected = modality[length(modality)],
                            status = "custom-class")

    updateCheckboxGroupButtons(session,"domain_compare",size="xs",
                               label=values_ini$config$vt,
                               choices = modality,
                               selected = modality[length(modality)-1],
                               status = "custom-class")

    # Micro-Data
    domain_data <- unique(c(values_ini$config$vt,values_ini$config$id_itw))

    updateCheckboxGroupButtons(session,"data_variables",size="xs",
                               choices = colnames(values_ini$df),
                               selected = domain_data,
                               status = "custom-class")

    if (length(values_ini$config$vg)){
      modality <- sort(pull(unique(values_ini$df[,values_ini$config$vg])))

      updateRadioGroupButtons(session,"config_zone",
                              label=values_ini$config$vg,
                              choices = unique(c(i18n$t("All"),modality)),
                              size="xs",
                              selected = c(i18n$t("All"),modality)[1],
                              status = "custom-class")
    }else{
      updateRadioGroupButtons(session,"config_zone",
                              label=values_ini$config$vg,
                              choices = i18n$t("All"),
                              size="xs",
                              status = "custom-class")
    }
  })

  verif_init <- reactive({
    if (input$config_domain == i18n$t("Loading...")) return(NULL)
    if (input$config_zone == i18n$t("Loading...")) return(NULL)

    return(TRUE)
  })

  #### reactive df creation ####

  observeEvent(input$config_domain,{
    values_dis$plot_distrib <- NULL
    values_dis$syn_distrib <- NULL
    values_dis$gt_distrib <- NULL
    values_itw$id_itw <- NULL
    values_itw$variable <- NULL
  })

  ##### df zone and domain #####

  df_sub <- reactive({
    req(values_ini$df,values_ini$config$vt,verif_init())

    df_sub <- values_ini$df %>%
      filter(!!sym(values_ini$config$vt) %in% input$config_domain)

    if (length(values_ini$config$vg) &&
        input$config_zone %in% pull(df_sub[,values_ini$config$vg]))
      df_sub <- df_sub %>% filter(!!sym(values_ini$config$vg) == input$config_zone)

    return(df_sub)
  })

  ##### df_stats zone and domain #####

  df_stats_sub <- reactive({
    req(verif_init())

    vec_domain <- c(input$config_domain,input$domain_compare)

    df_stats <- values_ini$df_stats
    if (is.null(df_stats)) return(NULL)
    df_stats %>%
      filter(!!sym(values_ini$config$vt) %in% vec_domain,
             group == input$config_zone)
  })

  ##### df_stats_itw zone and domain #####
  df_stats_itw_sub <- reactive({
    req(values_ini$df_stats_itw,verif_init())

    values_ini$df_stats_itw %>%
      filter(!!sym(values_ini$config$vt) == input$config_domain,
             group == input$config_zone)
    # collect()
  })


  #### Domain outliers ####

  observeEvent(input$domain_subtab_choice, {
    updateTabsetPanel(session, "domain_subtab",
                      selected = input$domain_subtab_choice)
  })

  ##### reactive df outliers #####

  df_stats_outliers <- reactive({

    req(df_stats_sub())

    db_longer <- df_stats_sub() %>%
      select(variable,!!sym(values_ini$config$vt),Nrow,Nval,type,
             stat,value,standard,value_ref) %>%
      filter(!stat %in% c("chi2")) %>%
      group_by(variable,stat) %>%
      mutate(value = case_when(
        sum(Nval,na.rm = TRUE) < input$domain_Nval ~ 0,
        TRUE ~ value
      )) %>%
      ungroup() %>%
      select(-standard,-Nrow,-Nval,-value_ref) %>%
      # group_by(variable,stat) %>%
      group_by(variable,stat) %>%
      # arrange(!!sym(values_ini$config$vt)) %>%
      # mutate(sd = (value-lag(value))/lag(value),
      mutate(sd = sd(value,na.rm=T)/mean(value,na.rm=T),
             sd = tidyr::replace_na(sd,0)) %>%
      ungroup() %>%
      arrange(variable,stat)

    db_longer %>%
      arrange(!!sym(values_ini$config$vt),stat) %>%
      pivot_wider(
        names_from = c(!!sym(values_ini$config$vt),stat),
        names_sep = "|",
        values_from = c(value,sd)
      )
  })

  prepa_domain_tab_outliers <- reactive({
    df_stats_outliers() %>%
      filter(if_any(starts_with("sd|"), ~ abs(.x) > input$domain_sensibility),
             if_all(matches("^value\\|.*\\|presence$"), ~ .x == 1)
             ) %>%
      select(-matches("presence$"))
  })

  output$domain_tab_outliers <- render_gt({
    tidy_to_gt(prepa_domain_tab_outliers(),input$domain_sensibility)
  })

  output$domain_tab_check <- render_gt({

    df_stats_outliers <- df_stats_outliers() %>%
      select(variable, type, matches("^value.*presence$"),
             matches("^sd.*presence$")) %>%
      filter(if_any(starts_with("sd|"), ~ abs(.x) > 0))

    tidy_to_gt(df_stats_outliers,2)
  })

  ##### Outliers graphics #####

  vars_outliers <- reactive({
    sort(unique(prepa_domain_tab_outliers()$variable))
  })

  output$domain_plots_outliers <- renderUI({

    req(vars_outliers())

    plot_output_list <- multi_plots(vars_outliers(),NULL,NULL,df_sub())

    tagList(
      fluidRow(
        column(6,h3(i18n$t("Distribution"))),
        column(6,h3(i18n$t("Comparaison")))
      ),
      do.call(tagList, unlist(plot_output_list, recursive = FALSE))
    )
  })

  observeEvent(vars_outliers(), {

    vec_domain <- c(input$config_domain,input$domain_compare)

    df_zone_compa <- values_ini$df %>%
      filter(!!sym(values_ini$config$vt) %in% vec_domain)

    if (length(values_ini$config$vg) &&
        input$config_zone %in% pull(df_sub()[,values_ini$config$vg])){
      df_zone_compa <- df_zone_compa %>%
        filter(!!sym(values_ini$config$vg) == input$config_zone)
    }

    lapply(vars_outliers(), function(i) {
      local({
        my_i <- i
        plot_domain_name <- paste(my_i, "_plot_domain", sep = "")
        plot_evol_name <- paste(my_i, "_plot_evol", sep = "")

        if (my_i %in% values_ini$config$vd){
          output[[plot_domain_name]] <- renderPlot({

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
              aes(x=!!sym(values_ini$config$vt),fill=!!sym(my_i))+
              geom_bar(position="fill") +
              scale_y_continuous(labels = scales::percent) +
              coord_flip() +
              theme_minimal(base_size = 15)
          })
        }

        if (my_i %in% values_ini$config$vc){
          output[[plot_domain_name]] <- renderPlot({
            ggplot(df_sub())+
              aes(x=!!sym(my_i))+
              geom_histogram(color="purple3",fill="#ee82ee") +
              theme_minimal(base_size = 15)
          })

          output[[plot_evol_name]] <- renderPlot({
            ggplot(df_zone_compa)+
              aes(x=!!sym(my_i))+
              geom_histogram(color="purple3",fill="#ee82ee") +
              theme_minimal(base_size = 15)+
              facet_wrap(vars(!!sym(values_ini$config$vt)),ncol=1)
          })
        }
      })
    })
  })

  #### Itw Outliers ####

  observeEvent(input$itw_subtab_choice, {
    updateTabsetPanel(session, "itw_subtab",
                      selected = input$itw_subtab_choice)
  })

  ##### Reactive df outliers #####

  df_stats_itw_outliers <- reactive({
    req(df_stats_itw_sub())

    df_stats_itw_sub() %>%
      filter(stat != "presence",!type == "txt") %>%
      group_by(!!sym(values_ini$config$id_itw),stat) %>%
      filter(abs(standard)>input$itw_threshold,
             Nrow > input$itw_Nrow,
             Nval > input$itw_Nval) %>%
      ungroup()
  })

  ##### Table and Ranking #####

  ###### Synthesis by row ######

  prepa_itw_ranking <- reactive({

    req(df_stats_itw_sub())

    df_count <- df_stats_itw_sub() %>%
      filter(stat == "chi2",
             Nrow >= input$itw_Nrow,
             Nval >= input$itw_Nval,
             standard >= input$itw_threshold) %>%
      group_by(!!sym(values_ini$config$id_itw)) %>%
      count(name="N_outliers")

    df_prepa <- df_stats_itw_sub() %>%
      filter(stat == "chi2",Nrow > input$itw_Nrow) %>%
      select(!!sym(values_ini$config$id_itw),!!sym(values_ini$config$vt),
             group,variable,standard,Nrow) %>%
      pivot_wider(
        id_cols = c(!!sym(values_ini$config$id_itw),
                    !!sym(values_ini$config$vt),
                    group,Nrow),
        names_from = variable,
        values_from = standard
      )

    out <- df_prepa %>%
      group_by(!!sym(values_ini$config$vt),group) %>%
      mutate(score = score_isoforest(across(where(is.numeric))),
             score = round(score*100,1)) %>%
      ungroup() %>%
      left_join(df_count) %>%
      arrange(desc(score))

    return(out)
  })

  output$itw_ranking <- renderDT({

    df <- prepa_itw_ranking() %>%
      select(!!sym(values_ini$config$id_itw),Nrow,N_outliers,score)

    dt <- datatable(df, filter='top', selection = 'single',escape   = FALSE,
                    options = list(pageLength = 15,dom = 'tp'),
                    rownames = F)
    dt
  })

  prepa_itw_listing <- reactive({
    req(input$itw_ranking_rows_selected)
    s <- input$itw_ranking_rows_selected
    id_itw <- pull(prepa_itw_ranking()[s,values_ini$config$id_itw])
    if(length(s)){
      out <- prepa_itw_ranking() %>%
        select(-Nrow,-N_outliers,-score,-!!sym(values_ini$config$vt),-group) %>%
        filter(!!sym(values_ini$config$id_itw) == id_itw) %>%
        pivot_longer(cols = where(is.numeric),names_to = "variable",
                     values_to = "chi2") %>%
        mutate(chi2 = round(chi2,1)) %>%
        arrange(desc(abs(chi2)))
    }else{
      out <- NULL
    }
    return(out)
  })

  output$itw_listing <- renderDT({
    df <- prepa_itw_listing()

    dt <- datatable(df, filter='top', selection = 'single',
                    escape   = FALSE,
                    options = list(pageLength = 15,dom = 'tp'),
                    rownames = F)

    dt
  })

  observeEvent(input$itw_listing_rows_selected, {
    req(input$itw_listing_rows_selected)
    s <- input$itw_listing_rows_selected

    values_itw$variable <- pull(prepa_itw_listing()[s,2])
    values_itw$id_itw <- pull(prepa_itw_listing()[s,1])
  })

  ###### Synthesis by column ######

  prepa_var_ranking <- reactive({

    req(df_stats_itw_sub())

    df_count <- df_stats_itw_sub() %>%
      filter(stat == "chi2",
             Nrow >= input$itw_Nrow,
             Nval >= input$itw_Nval,
             standard >= input$itw_threshold) %>%
      group_by(variable) %>%
      count(name="N_outliers")

    df_prepa <- df_stats_itw_sub() %>%
      filter(stat == "chi2",Nrow > input$itw_Nrow) %>%
      select(!!sym(values_ini$config$id_itw),!!sym(values_ini$config$vt),
             group,variable,standard) %>%
      pivot_wider(
        id_cols = c(!!sym(values_ini$config$vt),variable,group),
        names_from = !!sym(values_ini$config$id_itw),
        values_from = standard
      )

    out <- df_prepa %>%
      group_by(!!sym(values_ini$config$vt),group) %>%
      mutate(score = score_isoforest(across(where(is.numeric))),
             score = round(score*100,1)) %>%
      ungroup() %>%
      left_join(df_count) %>%
      arrange(desc(score))

    return(out)
  })

  output$itw_var_ranking <- renderDT({

    df <- prepa_var_ranking() %>%
      select(variable,N_outliers,score)

    dt <- datatable(df, filter='top', selection = 'single',escape   = FALSE,
                    options = list(pageLength = 15,dom = 'tp'),
                    rownames = F)
    dt
  })

  prepa_var_listing <- reactive({
    req(input$itw_var_ranking_rows_selected)
    s <- input$itw_var_ranking_rows_selected
    id_variable <- pull(prepa_var_ranking()[s,"variable"])
    if(length(s)){
      out <- prepa_var_ranking() %>%
        filter(variable == id_variable) %>%
        select(-N_outliers,-score,-!!sym(values_ini$config$vt),-group) %>%
        pivot_longer(cols = where(is.numeric),
                     names_to = values_ini$config$id_itw,
                     values_to = "chi2") %>%
        mutate(chi2 = round(chi2,1)) %>%
        arrange(desc(abs(chi2)))
    }else{
      out <- NULL
    }
    return(out)
  })

  output$itw_var_listing <- renderDT({
    df <- prepa_var_listing()

    dt <- datatable(df, filter='top', selection = 'single',
                    escape   = FALSE,
                    options = list(pageLength = 15,dom = 'tp'),
                    rownames = F)

    dt
  })

  observeEvent(input$itw_var_listing_rows_selected, {
    req(input$itw_var_listing_rows_selected)
    s <- input$itw_var_listing_rows_selected

    values_itw$variable <- pull(prepa_var_listing()[s,1])
    values_itw$id_itw <- pull(prepa_var_listing()[s,2])
  })

  ##### Prepare Heatmap ####

  prepa_heatmap <- reactive({
    req(verif_init(),df_stats_itw_sub())
    df_stats <- df_stats_itw_sub() %>%
      mutate(standard = case_when(
        Nrow < input$itw_Nrow ~ 0,
        !is.na(value_ref) & is.na(value) ~ standard,
        Nval < input$itw_Nval ~ 0,
        TRUE ~ standard))

    df_stats <- df_stats %>%
      filter(
        (type == "cha" & stat %in% c("missing","chi2")) |
          (type == "num" & stat %in% c("missing","median")))

    if (input$itw_choice_heatmap %in% c("enq","both"))
      df_stats <- df_stats %>%
      group_by(!!sym(values_ini$config$id_itw)) %>%
      filter(max(abs(standard),na.rm=TRUE)>input$itw_threshold) %>%
      ungroup()

    if (input$itw_choice_heatmap %in% c("var","both"))
      df_stats <- df_stats %>%
      group_by(variable) %>%
      filter(max(abs(standard),na.rm=TRUE)>input$itw_threshold) %>%
      ungroup()

    df_stats <- df_stats %>%
      mutate(!!sym(values_ini$config$id_itw) := factor(
        !!sym(values_ini$config$id_itw),levels = sort(
          unique(!!sym(values_ini$config$id_itw)))))

    if (nrow(df_stats) == 0) return(NULL)

    values_itw$heatmap_itw <- df_stats %>%
      pull(!!sym(values_ini$config$id_itw)) %>%
      unique() %>% as.character() %>% sort()

    values_itw$heatmap_var <- sort(unique(df_stats$variable))

    return(df_stats)
  })

  output$itw_heatmap <- renderPlotly({

    req(prepa_heatmap())

    p <- heat_map_itw(prepa_heatmap(),input$itw_threshold)

    ggplotly(p, tooltip = "text", source = "heatmap_source") %>%
      event_register("plotly_click")
  })

  ##### Reaction to heatmap #####

  df_sub_details <- reactive({
    req(df_sub(),values_itw$variable)
    df_sub <- df_sub()
    if (!input$itw_missing){
      df_sub <- df_sub %>% filter(!is.na(!!sym(values_itw$variable)))
    }
    return(df_sub)
  })

  observeEvent(event_data("plotly_click", source="heatmap_source"), {
    d <- event_data("plotly_click", source="heatmap_source")
    req(d)
    values_itw$variable <- values_itw$heatmap_var[d$x]
    values_itw$id_itw   <- values_itw$heatmap_itw[d$y]
  })

  ###### Reactive text in box #####

  output$itw_distrib_spe_text <- renderText({
    req(values_itw$id_itw)
    paste(i18n$t("Distribution for"), values_ini$config$id_itw, "=",values_itw$id_itw)
  })

  output$itw_distrib_glo_text <- renderText({
    req(values_itw$id_itw)
    paste(i18n$t("Distribution for"), values_ini$config$id_itw, "!=",values_itw$id_itw)
  })

  output$itw_corr_spe_text <- renderText({
    req(values_itw$id_itw)
    paste(i18n$t("Correlations for"), values_ini$config$id_itw, "=",values_itw$id_itw)
  })

  output$itw_corr_glo_text <- renderText({
    req(values_itw$id_itw)
    paste(i18n$t("Correlations for"), values_ini$config$id_itw, "!=",values_itw$id_itw)
  })

  ###### Reactive Distributions #####

  output$itw_distrib_spe <- renderPlot({

    validate(
      need(values_itw$id_itw, i18n$t('Choose a interviewer.')),
      need(values_itw$variable, i18n$t('Choose a variable.'))
    )

    syn_spe <- df_sub_details() %>%
      filter(!!sym(values_ini$config$id_itw) == values_itw$id_itw)

    if (values_itw$variable %in% values_ini$config$vc){
      p <- ggplot(syn_spe)+
        aes(x=!!sym(values_itw$variable))+
        geom_histogram(color="purple3",fill="#ee82ee") +
        theme_minimal()

    } else if (values_itw$variable %in% values_ini$config$vd){

      syn_spe <- syn_spe %>%
        count(!!sym(values_itw$variable)) %>%
        mutate(prop = n/sum(n),
               !!sym(values_itw$variable):=as.character(!!sym(values_itw$variable)))

      p <- ggplot(syn_spe)+
        aes(x=!!sym(values_itw$variable),y=prop)+
        geom_bar(fill="yellow3",stat="identity") +
        scale_y_continuous(labels = scales::percent) +
        coord_flip()+
        theme_minimal()
    }
    p + theme_minimal(base_size = 15)
  })

  output$itw_distrib_glo <- renderPlot({

    validate(
      need(values_itw$id_itw, i18n$t('Choose a interviewer.')),
      need(values_itw$variable, i18n$t('Choose a variable.'))
    )

    syn_glo <- df_sub_details() %>%
      filter(!!sym(values_ini$config$id_itw) != values_itw$id_itw)

    if (values_itw$variable %in% values_ini$config$vc){
      p <- ggplot(syn_glo)+
        aes(x=!!sym(values_itw$variable))+
        geom_histogram(color="#00708C",fill="mediumturquoise")
    } else if (values_itw$variable %in% values_ini$config$vd){
      syn_glo <- syn_glo %>%
        dplyr::count(!!sym(values_itw$variable)) %>%
        mutate(prop = n/sum(n),
               !!sym(values_itw$variable):=as.character(!!sym(values_itw$variable)))

      p <- syn_glo %>%
        ggplot()+
        aes(x=!!sym(values_itw$variable),y=prop)+
        geom_bar(fill="orange3",stat="identity") +
        scale_y_continuous(labels = scales::percent) +
        coord_flip()
    }
    p + theme_minimal(base_size = 15)
  })

  ###### Reactive Summary ######

  output$itw_summary_spe <- renderPrint({
    req(values_itw$id_itw,values_itw$variable)
    data_itw <- df_sub_details() %>%
      filter(!!sym(values_ini$config$id_itw) == values_itw$id_itw)
    dfSummary(data_itw[,values_itw$variable],graph.col=FALSE)
  })

  output$itw_summary_glo <- renderPrint({
    req(values_itw$id_itw,values_itw$variable)
    data_itw <- df_sub_details() %>%
      filter(!!sym(values_ini$config$id_itw) != values_itw$id_itw)
    dfSummary(data_itw[,values_itw$variable],graph.col=FALSE)
  })

  ###### Reactive Correlations ######

  prepa_corr <- reactive({
    req(values_ini$config$id_itw,values_itw$id_itw,values_itw$variable)

    id <- values_ini$config$id_itw
    df <- df_sub_details()
    df <- df %>%
      select(-all_of(id)) %>%
      mutate(across(where(is.character), as.factor),
             across(where(lubridate::is.Date), as.factor),
             across(where(is.factor), ~ as.numeric(.x))) %>%
      select(where(is.numeric)) %>%
      dplyr::bind_cols(df[all_of(id)])

    df <- df %>% select(where(~ sum(!is.na(.x)) >= input$itw_Nrow))
    df <- df %>% select(where(~ sum(!is.na(.x)) >= input$itw_Nval))

    vars_corr <- df %>%
      select(-!!sym(values_ini$config$id_itw)) %>%
      cor(y = .[[values_itw$variable]],
          use = "pairwise.complete.obs", method= "pearson") %>%
      as.table %>% as.data.frame %>%
      mutate(Freq_abs = abs(Freq)) %>%
      dplyr::slice_max(Freq_abs,n=10,with_ties = FALSE) %>%
      pull(Var1) %>% as.character()

    df %>% select(!!sym(values_ini$config$id_itw),any_of(vars_corr))
  })

  output$itw_corr_spe <- renderPlot({
    M <- prepa_corr() %>%
      filter(!!sym(values_ini$config$id_itw) == values_itw$id_itw) %>%
      select(-!!sym(values_ini$config$id_itw)) %>%
      cor(use = "pairwise.complete.obs",method="spearman")

    output$itw_main_corr_spe <- renderPlot(
      itw_main_corr(M,values_itw$variable)  +
        labs(title = paste(i18n$t("10 First Correlations with"),
                           values_itw$variable))
    )

    corrplot(M,type="upper", tl.col="grey20", tl.srt=45)
  })

  output$itw_corr_glo <- renderPlot({
    M <- prepa_corr() %>%
      filter(!!sym(values_ini$config$id_itw) != values_itw$id_itw) %>%
      select(-!!sym(values_ini$config$id_itw)) %>%
      cor(use = "pairwise.complete.obs",method="spearman")

    output$itw_main_corr_glo <- renderPlot(
      itw_main_corr(M,values_itw$variable)  +
        labs(title = paste(i18n$t("10 First Correlations with"),
                           values_itw$variable))
      )
    corrplot(M,type="upper", tl.col="grey20", tl.srt=45)
  })

  ###### All distributions ######

  observeEvent(input$itw_all_distrib,{

    data_glo <- df_sub_details() %>%
      select(!!sym(values_ini$config$id_itw),!!sym(values_itw$variable))

    sub_var <- data_glo %>% pull(!!sym(values_itw$variable))
    fl_discrete <- length(sub_var) < 15 | length(unique(sub_var)) < 15

    if (fl_discrete){

      showModal(modalDialog(
        title = paste(i18n$t("Analysis of distribution of variable "),values_itw$variable),
        size = "l",easyClose = TRUE,
        h3(paste0(i18n$t("Interviewer position"))),
        radioGroupButtons(
          inputId = "config_modalities_itw",label = i18n$t("Modality"),
          status = "primary",justified = TRUE,
          choices = i18n$t("Loading...")),
        renderPlot(values_dis$plot_distrib),
        h3(paste0(i18n$t("List of distributions"))),
        render_gt(values_dis$gt_distrib)
      ))

      values_dis$syn_distrib <- data_glo %>%
        group_by(!!sym(values_ini$config$id_itw),!!sym(values_itw$variable)) %>%
        count() %>%
        group_by(!!sym(values_ini$config$id_itw)) %>%
        mutate(N = sum(n),prop = n/N) %>%
        ungroup() %>%
        select(!!sym(values_ini$config$id_itw),!!sym(values_itw$variable),N,prop) %>%
        arrange(!!sym(values_itw$variable)) %>%
        pivot_wider(names_from=!!sym(values_itw$variable),values_from = prop) %>%
        arrange(desc(N))

      modalities <- sort(colnames(values_dis$syn_distrib[,-c(1:2)]))

      updateRadioGroupButtons(session,"config_modalities_itw",size="xs",
                              choices = modalities,selected = modalities[1],
                              status = "custom-class")
    }
  })

  observeEvent(values_dis$syn_distrib,{
    values_dis$gt_distrib <- values_dis$syn_distrib %>%
      gt() %>%
      fmt_percent(columns = -c(1,2),decimals = 1,drop_trailing_zeros = TRUE) %>%
      data_color(
        rows = !!sym(values_ini$config$id_itw) == values_itw$id_itw,
        direction = "row",palette = c("red")
      )
  })

  observeEvent(input$config_modalities_itw,{

    syn_distrib <- df_sub() %>%
      group_by(!!sym(values_ini$config$id_itw),!!sym(values_itw$variable)) %>%
      dplyr::count() %>%
      group_by(!!sym(values_ini$config$id_itw)) %>%
      mutate(N = sum(n),prop = n/N) %>%
      ungroup() %>%
      tidyr::complete(!!sym(values_ini$config$id_itw),!!sym(values_itw$variable),
               fill=list(n=0,prop=0))

    if (input$config_modalities_itw == "NA"){
      syn_distrib <- syn_distrib %>%
        filter(is.na(!!sym(values_itw$variable)))
    }else{
      syn_distrib <- syn_distrib %>%
        filter(as.character(!!sym(values_itw$variable)) == input$config_modalities_itw)
    }

    try({
      itw_value <- syn_distrib %>%
        filter(!!sym(values_ini$config$id_itw) == values_itw$id_itw) %>%
        pull(prop)

      values_dis$plot_distrib <-  syn_distrib %>%
        ggplot() +
        aes(x = prop) +
        geom_density(fill = "sienna2",alpha = 0.65) +
        theme_minimal(base_size = 15) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                           limits = c(0,1)) +
        labs(x = paste(i18n$t("Proportion of"),input$config_modalities_itw),
             y = i18n$t("Density")) +
        geom_vline(xintercept = itw_value, color = "red",
                   linetype = "dashed", linewidth = 1.2) +
        annotate("text", x = itw_value, y = 0,
                 label = paste0("Itw : ", round(itw_value*100, 1),"%"),
                 vjust = -0.5, hjust = 1.1, color = "red")
    },silent = TRUE)

  })

  #### Data ####

  output$data_table <- renderDT({

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

