mod_intvwr_variable_ui <- function(id, i18n) {
  ns <- NS(id)
  bslib::nav_panel(
    title = tagList(bs_icon("crosshair"), i18n$t("Cross")),
    value = "tab_intvwr_variable",
    uiOutput(ns("no_data_msg")),
    
    bslib::navset_card_pill(
      placement = "above",
      
      bslib::nav_panel(
        title = tagList(bs_icon("list-ol"), i18n$t("Chi² table")),
        card(icon_header("trophy-fill", i18n$t("Ranking of Interviewer"),
                         help = ns("card_cross")),
             DTOutput(ns("cross_ranking")))
      ),
      
      bslib::nav_panel(
        title = tagList(bs_icon("grid-3x3-gap-fill"), i18n$t("Heatmap")),
        h2(class = "d-flex align-items-center gap-2",
           i18n$t("Anomalies by Variables and Interviewers"),
           help_button(ns("heatmap"))),
        radioButtons(ns("heatmap_seriation"), i18n$t("Choice classification of heatmap"),
                     inline = TRUE, choices = c("None")),
        radioButtons(ns("heatmap_choice"), i18n$t("Choice of rows and columns"),
                     inline = TRUE,
                     choiceNames = list(i18n$t("All"), i18n$t("Risky rows"),
                                     i18n$t("Risky columns"), i18n$t("Risky cells")),
                     choiceValues = c("all", "enq", "var", "both"),
                     selected = "both"),
        plotlyOutput(ns("heatmap"), height = "800px")
      ),
      
      bslib::nav_panel(
        title = tagList(bs_icon("list-ol"), i18n$t("Interviewer Synthesis")),
        layout_columns(
          col_widths = c(6, 6),
          card(icon_header("trophy-fill", i18n$t("Ranking of Interviewers"),
                           help = ns("card_intvwr_ranking")),
               DTOutput(ns("intvwr_ranking"))),
          card(icon_header("list-ul", i18n$t("Listing of variables"),
                           help = ns("card_variable_listing")),
               DTOutput(ns("variable_listing")))
        )
      ),
      
      bslib::nav_panel(
        title = tagList(bs_icon("layout-three-columns"), i18n$t("Variable Synthesis")),
        layout_columns(
          col_widths = c(6, 6),
          card(icon_header("trophy-fill", i18n$t("Ranking of Variables"),
                           help = ns("card_variable_ranking")),
               DTOutput(ns("variable_ranking"))),
          card(icon_header("list-ul", i18n$t("Listing of Interviewers"),
                           help = ns("card_intvwr_listing")),
               DTOutput(ns("intvwr_listing")))
        )
      )
    ),
    uiOutput(ns("dict")),
    card(
      full_screen = TRUE,
      icon_header("list-task", i18n$t("Comparison of distributions"),
                  help = ns("card_distrib")),
      layout_columns(
        col_widths = c(6, 6),
        plotOutput(ns("distrib")),
        # verbatimTextOutput(ns("summary"))
        DT::DTOutput(ns("summary"))
      )
    ),
    card(
      full_screen = TRUE,
      icon_header("list-task", i18n$t("Distribution of modalities"),
                  help = ns("card_distrib_mods")),
      uiOutput(ns("distrib_ui"))
    )
  )
}

mod_intvwr_variable_server <- function(id, filt, data, r_focus, opts, lang, i18n_s) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) { lang(); i18n_s$t(x) }
    cfg <- reactive(data$config())
    
    values_heatmap <- reactiveValues(
      intvwr = NULL,
      variable = NULL
    )
    
    ##### Reactive df stats #####
    
    df_stats_intvwr <- reactive({
      req(filt$df_stats_intvwr())
      filt$df_stats_intvwr() %>% filter(stat != "presence",type != "txt")
    })
    
    df_stats_intvwr_filter <- reactive({
      req(filt$df_stats_intvwr())
      df_stats_intvwr() %>%
        dplyr::filter(Nrow >= filt$threshold_Nrow(),
                      Nval >= filt$threshold_Nval(),
                      abs(standard) >= filt$threshold_intvwr())
    })
    
    ##### Prepa DB #####
    
    prepa_intvwr_ranking <- reactive({
      req(df_stats_intvwr_filter())
      prepa_ranking(df_stats_intvwr_filter,cfg()$var_intvwr,"variable",filt)
    })

    prepa_variable_ranking <- reactive({
      req(df_stats_intvwr_filter())
      prepa_ranking(df_stats_intvwr_filter,"variable",cfg()$var_intvwr,filt)
    })
    
    prepa_cross_ranking <- reactive({
      filt$df_var_ranking()
    })
    
    ##### DT ranking #####
    
    output$intvwr_ranking <- renderDataTable({
      df <- prepa_intvwr_ranking() %>%
        select(!!sym(cfg()$var_intvwr),Nrow,N_outliers,score)
      
      datatable(df, filter='top', selection = 'single',escape   = FALSE,
                options = list(pageLength = 15,dom = 'tp'),
                rownames = F)
    })
    
    output$variable_ranking <- renderDataTable({
      df <- prepa_variable_ranking() %>%
        select(variable,N_outliers,score)
      
      datatable(df, filter='top', selection = 'single',escape   = FALSE,
                      options = list(pageLength = 15,dom = 'tp'),
                      rownames = F)
    })
    
    output$cross_ranking <- renderDT({
      df <- prepa_cross_ranking() %>% 
        select(!!sym(cfg()$var_intvwr),Nrow,variable,chi2,standard)
      datatable(df, filter='top', selection = 'single',escape   = FALSE,
                options = list(pageLength = 15,dom = 'tp'),
                rownames = F)
    })
    
    ##### Prepa Listing #####
    
    prepa_variable_listing <- reactive({
      req(input$intvwr_ranking_rows_selected)
      s <- input$intvwr_ranking_rows_selected
      req(length(s))
      id_intvwr <- pull(prepa_intvwr_ranking()[s,cfg()$var_intvwr])
      
      prepa_listing(prepa_intvwr_ranking,cfg()$var_intvwr,id_intvwr,"variable")
    })
    
    prepa_intvwr_listing <- reactive({
      req(input$variable_ranking_rows_selected)
      s <- input$variable_ranking_rows_selected
      req(length(s))
      id_variable <- pull(prepa_variable_ranking()[s,"variable"])
      
      prepa_listing(prepa_variable_ranking,"variable",id_variable,cfg()$var_intvwr)
    })
    
    ##### DT Listing ######
    
    output$intvwr_listing <- renderDataTable({
      df <- prepa_intvwr_listing()
      
      datatable(df, filter='top', selection = 'single',
                escape   = FALSE,
                options = list(pageLength = 15,dom = 'tp'),
                rownames = F)
    })
    
    output$variable_listing <- renderDataTable({
      df <- prepa_variable_listing()
      
      datatable(df, filter='top', selection = 'single',
                escape   = FALSE,
                options = list(pageLength = 15,dom = 'tp'),
                rownames = F)
    })
    
    ##### Heatmap #####

    observe({
      method <- c("None","ARSA","GW","GW_average","GW_complete","GW_single","GW_ward",
                  "HC","HC_average","HC_complete","HC_single","HC_ward",
                  "OLO","OLO_average","OLO_complete","OLO_single","OLO_ward",
                  "isomap","QAP_2SUM","QAP_BAR","QAP_Inertia","QAP_LS",
                  "R2E","Reverse","Sammon_mapping","Spectral","Spectral_norm",
                  "SPIN_NH","TSP","VAT")
      
      updateRadioButtons(session,"heatmap_seriation",
                         choices = method,selected = "None")
    })
    
    prepa_heatmap <- reactive({
      req(df_stats_intvwr())
      df_stats <- df_stats_intvwr() %>%
        mutate(standard = case_when(
          Nrow < filt$threshold_Nrow() ~ 0,
          !is.na(value_ref) & is.na(value) ~ standard,
          Nval < filt$threshold_Nval() ~ 0,
          TRUE ~ standard))
      
      df_stats <- df_stats %>%
        filter(
          (type == "cha" & stat %in% c("missing","chi2")) |
            (type == "num" & stat %in% c("missing","median")))
      
      if (input$heatmap_choice %in% c("enq","both"))
        df_stats <- df_stats %>%
        group_by(!!sym(cfg()$var_intvwr)) %>%
        filter(max(abs(standard),na.rm=TRUE) >= filt$threshold_intvwr()) %>%
        ungroup()
      
      if (input$heatmap_choice %in% c("var","both"))
        df_stats <- df_stats %>%
        group_by(variable) %>%
        filter(max(abs(standard),na.rm=TRUE) >= filt$threshold_intvwr()) %>%
        ungroup()
      
      if (nrow(df_stats) == 0) return(NULL)
      
      # Seriation
      if (input$heatmap_seriation == "None"){
        df_stats <- df_stats %>%
          mutate(!!sym(cfg()$var_intvwr) := factor(
            !!sym(cfg()$var_intvwr),levels = sort(
              unique(!!sym(cfg()$var_intvwr)))))
        
        values_heatmap$intvwr <- df_stats %>%
          pull(!!sym(cfg()$var_intvwr)) %>%
          unique() %>% as.character() %>% sort()
        
        values_heatmap$variable <- df_stats %>%
          pull(variable) %>%
          unique() %>% as.character() %>% sort()
        
      }else{
        
        df_stats_wide <- df_stats %>% 
          group_by(variable,!!sym(cfg()$var_intvwr)) %>%
          summarise(standard = max(standard,na.rm=T)) %>% 
          select(variable,!!sym(cfg()$var_intvwr),standard) %>% 
          pivot_wider(names_from = variable,values_from = standard) %>% 
          tibble::column_to_rownames(cfg()$var_intvwr)
        
        df_stats_wide[is.na(df_stats_wide)] <- 0
        
        o <- seriation::seriate(df_stats_wide, method = "Heatmap", 
                                seriation_method = input$heatmap_seriation)
        order_intvwr   <- names(seriation::get_order(o, 1))
        order_variable <- names(seriation::get_order(o, 2))
        
        df_stats <- df_stats %>%
          mutate(
            !!sym(cfg()$var_intvwr) := factor(
              !!sym(cfg()$var_intvwr),levels = order_intvwr),
            variable = factor(variable, levels = order_variable)
          )

        values_heatmap$intvwr   <- order_intvwr
        values_heatmap$variable <- order_variable
      }

      return(df_stats)
    })
    
    heatmap_ready <- reactiveVal(FALSE)
    
    output$heatmap <- renderPlotly({
      req(prepa_heatmap())
      p <- heatmap_group(prepa_heatmap(),filt$threshold_intvwr())
      heatmap_ready(TRUE)
      ggplotly(p, tooltip = "text", source = "heatmap_source") %>%
        event_register("plotly_click")
    })
    
    ##### selected_intvwr and variable ######
    
    bind_selection <- function(input_id, df, field, id_col = 1) {
      observeEvent(input[[input_id]], {
        r_focus[[field]] <- dplyr::pull(df()[input[[input_id]], id_col])
      })
    }
    
    bind_selection("cross_ranking_rows_selected", prepa_cross_ranking, "intvwr")
    bind_selection("cross_ranking_rows_selected", prepa_cross_ranking, "variable", id_col = "variable")

    bind_selection("intvwr_ranking_rows_selected", prepa_intvwr_ranking, "intvwr")
    bind_selection("variable_listing_rows_selected", prepa_variable_listing, "variable")
    
    bind_selection("variable_ranking_rows_selected", prepa_variable_ranking, "variable")
    bind_selection("intvwr_listing_rows_selected", prepa_intvwr_listing, "intvwr")
    
    observe({
      req(heatmap_ready())
      d <- event_data("plotly_click", source = "heatmap_source")
      req(d)
      r_focus$intvwr   <- values_heatmap$intvwr[d$y]
      r_focus$variable <- values_heatmap$variable[d$x]
    })
    
    selected_intvwr   <- reactive(r_focus$intvwr)
    selected_variable <- reactive(r_focus$variable)
    
    ###### Dictionnary of variable ######
    
    prepa_dict <- reactive({
      req(data$df_dict(),selected_variable())
      
      df <- data$df_dict()
      
      colnames(df) <- toupper(colnames(df))
      
      if (!"VARIABLE" %in% colnames(df)) return(NULL)
      
      df <- df %>% filter(str_detect(toupper(VARIABLE),selected_variable()))
      
      if ("TX_LANG" %in% colnames(df)){
        df <- df %>% filter(TX_LANG == toupper(lang()))
      }  
      
      if (nrow(df) == 0) return(NULL) else return(df)
    })
    
    output$dict_dt <- renderDataTable({
      req(prepa_dict())
      
      datatable(prepa_dict(), filter = "top", selection = 'none', 
                escape   = FALSE,
                options = list(pageLength = 15,dom = 'tp'),
                rownames = F)
    })
    
    output$dict <- renderUI({
      req(prepa_dict())
      card(
        full_screen = TRUE,
        icon_header("book-half", tr("Data dictionary"),
                    help = ns("card_dict")),
        DTOutput(ns("dict_dt"))
      )
    })
    
    ##### Distribution of of specific variable ######
    
    output$distrib <- renderPlot({
      validate(
        need(selected_intvwr(), tr('Choose a interviewer.')),
        need(selected_variable(), tr('Choose a variable.'))
      )
      
      df <- filt$df()
      
      levels <- df %>% pull(!!sym(selected_variable())) %>% unique()
      
      if(length(levels) > 15){
        if (is.factor(pull(df[,selected_variable()]))){
          validate(tr("Too much modalities for this categorical variable"))
        }else{
          df <- df %>% mutate(!!sym(selected_variable()) := 
                                cut_safe(!!sym(selected_variable())))
        }
      }
      
      plot_compa_distributions(df, 
                               selected_intvwr(), selected_variable(), 
                               cfg()$var_intvwr, type = "categorical")
    })
    
    output$summary <- DT::renderDT({
      req(filt$df(),selected_variable(),selected_intvwr())
      
      df   <- filt$df()
      grp  <- group_vs_others(df[[cfg()$var_intvwr]], selected_intvwr(),cfg()$var_intvwr)
      type <- if (selected_variable() %in% cfg()$vars_discretes)
      "categorical" else "auto"
      
      describe_variable_dt(df[[selected_variable()]], by = grp, type = type)
    })
    
    df_mods <- reactive({
      df <- filt$df()
      
      if (selected_variable() %in% cfg()$vars_continuous) {
        df <- df %>% mutate(!!sym(selected_variable()) := 
                                cut_safe(!!sym(selected_variable())))
      }
      return(df)
    })

    dist_mods <- reactive({
      req(selected_variable(),selected_intvwr())
      df_mods() %>% 
        count_auto(cfg()$var_intvwr, selected_variable()) %>% 
        pull(!!sym(selected_variable())) %>% unique()
    })
    
    output$distrib_ui <- renderUI({
      req(dist_mods())
      
      if (length(dist_mods()) >= 15) {
        validate(tr("Too much modality for this variable"))
      }
      
      mods <- dist_mods()
      fluidRow(lapply(seq_along(mods), function(i) {
        column(4, plotOutput(ns(paste0("mods_dist_", i)), height = "220px"))
      }))
    })
    
    dist_mods_prop <- reactive({
      req(selected_variable(),selected_intvwr())
      df_mods() %>%
        count_auto(cfg()$var_intvwr, selected_variable()) %>% 
        group_by(!!sym(cfg()$var_intvwr)) %>% mutate(prop = n / sum(n)) %>% 
        ungroup()
    })
    
    observeEvent(dist_mods(), {
      req(dist_mods())
      mods <- dist_mods(); df <- dist_mods_prop()
      lapply(seq_along(mods), function(i) local({
        my_mod <- mods[[i]]
        
        if (is.na(my_mod)){
          sub_df <- df %>% filter(is.na(!!sym(selected_variable())))
        }else{
          sub_df <- df %>% filter(!!sym(selected_variable()) == my_mod)
        }
        
        enq_value <- sub_df %>% 
          filter(!!sym(cfg()$var_intvwr) == selected_intvwr()) %>% pull(prop)
        output[[paste0("mods_dist_", i)]] <- renderPlot({
          p <- ggplot(sub_df, aes(x = prop)) +
            geom_density(fill = "steelblue",alpha = 0.65) +
            scale_x_continuous(labels = scales::percent, limits = c(0, 1), 
                               expand = c(0, 0))+
            labs(x = paste("Proportion of",my_mod), y = "% interviewer") +
            theme_minimal(base_size = 12) +
            geom_vline(xintercept = enq_value, color = "red", 
                       linetype = "dashed", linewidth = 1.2) +
            annotate("text", x = enq_value, y = 0, 
                     label = paste0("Enq : ", round(100*enq_value),"%"),
                     vjust = -0.5, hjust = 1.1, color = "red")
          
          p
        })
      }))
    })
  })
}
