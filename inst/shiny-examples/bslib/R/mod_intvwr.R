# ============================================================================
# mod_interviewer.R  — onglet "Interviewer"
# Consomme : filt (df, df_timer_intv), data (config, timer),
#            r_focus (état partagé inter-onglets : $intvwr, $variable, écrit ici)
#
# Fixes intégrés :
#   - IDs d'output dynamiques INDEXÉS (plus de paste0(prefix, valeur_modalité))
#   - set.seed() avant le scoring iForest (reproductibilité du RANK)
#   - DT::renderDT partout
# ============================================================================

mod_intvwr_ui <- function(id, i18n) {
  ns <- NS(id)
  nav_panel(
    title = tagList(bs_icon("person-vcard-fill"), i18n$t("Interviewer")),
    value = "tab_intvwr",
    uiOutput(ns("no_data_msg")),
    card(
      card_body(
        checkboxGroupInput(ns("indics"), i18n$t("Indicators"),
                           choices = i18n$t("Loading..."), inline = TRUE)
      )
    ),
    card(
      full_screen = TRUE,
      min_height = "650px",
      icon_header("table", i18n$t("Synthesis")),
      DTOutput(ns("table"))
    ),
    uiOutput(ns("intvwr_title")),
    
    navset_card_pill(
      id = ns("nav_details"),
      placement = "above",
      nav_panel(
        title = tagList(bs_icon("bar-chart-line"), i18n$t("Details per indicator")),
        uiOutput(ns("distrib_ui"))
      ),
      nav_panel(
        value = "nav_detail_intvw",
        title = tagList(bs_icon("clipboard-data-fill"), i18n$t("Detail per interview")),
        card(
          full_screen = TRUE,
          min_height = "800px",
          icon_header("list-task", i18n$t("Table of interviews")),
          DTOutput(ns("intv_table"))
        ),
        
        uiOutput(ns("intvw_title")),
        card(
          full_screen = TRUE,
          icon_header("hourglass-split", i18n$t("Sessions of interview")),
          DTOutput(ns("ssn_table"))),
        card(
          full_screen = TRUE,
          min_height = "650px",
          icon_header("layout-text-sidebar-reverse", i18n$t("Detail of the interview")),
          DTOutput(ns("details")))
      ),
      nav_panel(
        title = tagList(bs_icon("clipboard-data-fill"), i18n$t("Detail per variable")),
        layout_columns(
          col_widths = c(6, 6),
          card(
            full_screen = TRUE,
            icon_header("list-task", i18n$t("Table of variables")),
            DTOutput(ns("var_table"))
          ),
          card(
            full_screen = TRUE,
            icon_header("list-task", i18n$t("Comparison of distributions")),
            plotOutput(ns("var_distrib")),
            verbatimTextOutput(ns("var_summary"))
          )
        ),
        card(
          full_screen = TRUE,
          icon_header("list-task", i18n$t("Distribution of modalities")),
          uiOutput(ns("var_distrib_ui"))
        )
      ),
      nav_panel(
        title = tagList(bs_icon("clipboard-data-fill"), i18n$t("Details on interviewer")),
        layout_columns(
          col_widths = c(6, 6),
          card(
            full_screen = TRUE,
            icon_header("list-task", i18n$t("Localisation of interviews")),
            DTOutput(ns("intvwr_geo"))
          ),
          card(
            full_screen = TRUE,
            icon_header("list-task", i18n$t("Number of interviews across wave")),
            DTOutput(ns("intvwr_stat"))
          )
        )
      )
    )
  )
}

mod_intvwr_server <- function(id, filt, data, r_focus, opts, i18n_s) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) i18n_s$t(x)
    cfg <- reactive(data$config())
    
    output$no_data_msg <- renderUI({
      tmr <- data$timer()
      if (isTRUE(tmr$ready)) return(NULL)
      nav_remove("nav_details", target = "nav_detail_intvw")
      div(class = "alert alert-warning",
          if (!is.null(tmr$error)) tmr$error else tr("Données indisponibles."))
    })

    # -- Tableau de scoring (assemblage audit + homogénéité + chi2) ------------
    prepa <- reactive({
      req(filt$df())
      tmr <- data$timer()
      
      df_count <- filt$df() %>% 
        count(!!sym(cfg()$var_intvwr), name = "NB_INTV") %>% 
        mutate(!!sym(cfg()$var_intvwr) := as.character(!!sym(cfg()$var_intvwr)))
      
      df_homogenity <- filt$df() %>% 
        index_homogeneity(cfg()$var_intvwr,0) %>% 
        mutate(INDEX_H = round(mean*100,1)) %>% 
        select(!!sym(cfg()$var_intvwr),INDEX_H) %>% 
        mutate(!!sym(cfg()$var_intvwr) := as.character(!!sym(cfg()$var_intvwr)))
      
      if (isTRUE(tmr$ready)){
        df_audit <- filt$df_timer_intv() %>% 
          build_df_timer_intvwr(data$timer()$cfg) %>% 
          select(-any_of(cfg()$var_wave)) %>%
          mutate(across(starts_with('PC_'), ~round(.x*100,1))) %>% 
          mutate(across(starts_with('DURATION_'), ~round(.x,1))) %>% 
          mutate(!!sym(cfg()$var_intvwr) := as.character(!!sym(cfg()$var_intvwr))) %>% 
          select(-NB_INTV)
      }else{
        df_audit <- tibble(!!sym(cfg()$var_intvwr) := character())
      }
      
      df_chi2 <- filt$df_stats_intvwr() %>% 
        filter(stat == "chi2") %>% 
        group_by(!!sym(cfg()$var_intvwr)) %>% 
        summarise(MAX_CHI2=round(max(standard,na.rm=T),1)) %>% 
        mutate(!!sym(cfg()$var_intvwr) := as.character(!!sym(cfg()$var_intvwr)))
      
      df_audit %>% 
        dplyr::full_join(df_count,by = cfg()$var_intvwr) %>% 
        dplyr::full_join(df_homogenity,by = cfg()$var_intvwr) %>% 
        dplyr::full_join(df_chi2,by = cfg()$var_intvwr) %>% 
        filter(NB_INTV >= filt$threshold_Nrow())
    })

    observe({
      req(cfg()$var_intvwr)
      indics <- prepa() %>%
        dplyr::select(-!!rlang::sym(cfg()$var_intvwr), -dplyr::starts_with("NR_"),
                      -dplyr::starts_with("DT_")) %>% colnames()
      freezeReactiveValue(input, "indics")
      updateCheckboxGroupInput(session, "indics", inline = TRUE,
                               choices = indics, selected = indics)
    })

    scored <- reactive({
      df <- prepa()
      if (!is.null(input$indics))
        df <- df %>% dplyr::select(!!rlang::sym(cfg()$var_intvwr), 
                                   # !!!rlang::syms(input$indics))
                                   any_of(input$indics))
      set.seed(opts$seed)
      df %>%
        dplyr::mutate(score = score_isoforest(dplyr::across(where(is.numeric)))) %>%
        dplyr::arrange(-score) %>%
        dplyr::mutate(RANK = dplyr::row_number()) %>%
        dplyr::select(-score) %>%
        dplyr::relocate(dplyr::any_of(c(cfg()$var_intvwr, "RANK","NB_INTV")))
    })

    output$table <- DT::renderDT({

      var_rank <- "RANK"
      var_itwvr <- cfg()$var_intvwr
      
      var_quanti <- scored() %>%
        select(any_of(c("NB_INTV","DURATION_MEDIAN","DURATION_MIN"))) %>%
        select(where(is.numeric)) %>% names()
      
      var_outliers <- scored() %>%
        select(-any_of(c(var_itwvr,var_rank,var_quanti))) %>%
        select(where(is.numeric)) %>% names()
      
      dt <- df_to_formated_dt(scored(),var_rank,var_quanti,var_outliers)
      
      dt
    })

    # --- Sélection -> état partagé r_focus -----------------------------------
    selected_intvwr <- reactive({
      req(input$table_rows_selected)
      v <- dplyr::pull(scored()[input$table_rows_selected, 1])
      r_focus$intvwr <- v
      v
    })

    # outputs dynamiques par INDICE
    dist_cols <- reactive({
      scored() %>%
        dplyr::select(-RANK, -dplyr::starts_with("NR_"), -dplyr::starts_with("DT_")) %>%
        colnames()
    })

    output$distrib_ui <- renderUI({
      req(dist_cols())
      cols <- dist_cols()
      fluidRow(lapply(seq_along(cols), function(i) {
        column(4, plotOutput(ns(paste0("dist_", i)), height = "220px"))
      }))
    })

    observeEvent(dist_cols(), {
      cols <- dist_cols()
      df <- scored()
      lapply(seq_along(cols), function(i) local({
        my_col <- cols[[i]]
        sub <- df[is.finite(df[[my_col]]), , drop = FALSE]
        output[[paste0("dist_", i)]] <- renderPlot({
          p <- ggplot(sub, aes(x = .data[[my_col]])) +
            geom_histogram(fill = "steelblue", color = "white", bins = 20) +
            theme_minimal(base_size = 12)
          
          try({
            enq_value <- sub %>% 
              filter(!!sym(cfg()$var_intvwr) == selected_intvwr()) %>% 
              pull(!!sym(cols[[i]]))
            
            p <- p +
              geom_vline(xintercept = enq_value, color = "red", 
                         linetype = "dashed", linewidth = 1.2) +
              annotate("text", x = enq_value, y = 0, 
                       label = paste0("Enq : ", enq_value,"%"),
                       vjust = -0.5, hjust = 1.1, color = "red")
            
          },silent = T)
          
          p
        })
      }))
    })

    # --- Drill-down ménage -> session -> détail ----------
    output$intvwr_title <- renderUI(h3(paste(tr("Interviewer:"), selected_intvwr())))
    
    prepa_intv_table <- reactive({
      
      df <- filt$df_timer_intv() %>%
        filter(!!sym(cfg()$var_intvwr) == selected_intvwr()) %>%
        select(!!sym(cfg()$var_intv), MIN_TM_INTV, 
               starts_with('DURATION_'), starts_with('FL_')) %>%
        mutate(across(starts_with('DURATION_'), ~format_duree(as.numeric(.x,1))),
               across(ends_with('_TM_INTV'), ~stringr::str_replace(.x,"T"," ")),
               across(ends_with('_TM_INTV'), ~stringr::str_replace(.x,"Z","")))

      df %>%  mutate(across(starts_with("FL_"),~ifelse(.x,"\U0001f534","")))
    })
    
    output$intv_table <- DT::renderDT({ 
      datatable(prepa_intv_table(), filter = "top", selection = "single", 
                rownames = FALSE, options = list(pageLength = 10, dom = "tp"))
    })
    
    selected_intv <- reactive({
      req(input$intv_table_rows_selected)
      dplyr::pull(prepa_intv_table()[input$intv_table_rows_selected, 1])
    })
    
    output$intvw_title <- renderUI({ 
      req(selected_intv(), selected_intvwr())
      h3(paste(tr("Interview:"), selected_intv(), 
               tr("- Interviewer:"), selected_intvwr()))
    })
    
    output$ssn_table <- DT::renderDT({ 
      
      df <- data$timer()$df_timer_ssn %>%
        filter(!!sym(cfg()$var_intvwr) == selected_intvwr(),
               !!sym(cfg()$var_intv) == selected_intv()) %>%
        select(!!sym(data$timer()$cfg$var_session), MIN_TM_SSN, 
               starts_with('DURATION_'), starts_with('FL_'), CHECK_SCTN) %>%
        mutate(across(starts_with('DURATION_'), ~format_duree(as.numeric(.x,1))),
               across(ends_with('_TM_SSN'), ~stringr::str_replace(.x,"T"," ")),
               across(ends_with('_TM_SSN'), ~stringr::str_replace(.x,"Z",""))) %>%  
        mutate(across(starts_with("FL_"),~ifelse(.x,"\U0001f534","")))
      
      datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = "t"))
    })
    
    output$details   <- DT::renderDT({ 
      req(selected_intvwr(), selected_intv())
      
      sel <- selected_intv()
      
      df <- filt$df_timer_detail() %>% 
        filter(!!sym(cfg()$var_intv) == sel) %>% 
        collect()
      
      datatable(df, rownames = FALSE, filter = "top", 
                options = list(pageLength = 15, dom = "t"))
    })
    
    # --- Variables atypiques (chi2) + distribution comparée ------------------
    
    prepa_var <- reactive({
      filt$df_var_ranking() %>% 
        filter(!!sym(cfg()$var_intvwr) == selected_intvwr())
    })
    
    output$var_table <- renderDataTable({
      
      df <- prepa_var()
      
      datatable(df, filter='top', selection = 'single',escape = FALSE,
                      options = list(pageLength = 15,dom = 'tp'),
                      rownames = F)
    })
    
    # --- Sélection variable -> état partagé r_focus -----------------------------------
    selected_variable <- reactive({
      req(input$var_table_rows_selected)
      v <- dplyr::pull(prepa_var()[input$var_table_rows_selected, "variable"])
      if (!v %in% colnames(filt$df())) v <- NULL
      r_focus$variable <- v
      v
    })
    
    output$var_distrib <- renderPlot({
      
      req(prepa_var(),selected_variable())
      
      validate(
        need(selected_intvwr(), tr('Choose a group.')),
        need(selected_variable(), tr('Choose a variable.'))
      )
      
      plot_compa_distributions(filt$df(), 
                               selected_intvwr(), selected_variable(), 
                               cfg()$var_intvwr, type = "auto")
      
    })
    
    output$var_summary <- renderPrint({
      req(filt$df(), selected_variable())
      
      df <- filt$df() %>%
        mutate(.group = !!sym(cfg()$var_intvwr) == selected_intvwr()) %>%
        replace_na(list(.group = FALSE)) %>%
        mutate(.group = ifelse(.group,
                               paste(cfg()$var_intvwr, "=",selected_intvwr()),
                               paste(cfg()$var_intvwr, "!=",selected_intvwr())))
      
      if (selected_variable() %in% cfg()$vars_discretes){
        df <- df %>% 
          mutate(!!sym(selected_variable()) := as.character(!!sym(selected_variable())))
      }
      
      df %>% group_by(.group) %>%
        select(!!sym(selected_variable())) %>%
        dfSummary(graph.col=FALSE,valid.col=FALSE)
    })
    
    output$var_all_distrib_ui <- renderUI({
      req(filt$df(),selected_variable())
      
      validate(
        need(selected_intvwr(), tr('Choose a group.')),
        need(selected_variable(), tr('Choose a variable.'))
      )
      
      modalities <- filt$df() %>% 
        count(var = !!sym(selected_variable())) %>% pull(var)
      
      plot_list <- lapply(modalities, function(col) {
        column(width = 4, plotOutput(paste0("mods_dist_", col), height = "220px"))
      })
      tagList(fluidRow(plot_list))
    })
    
    dist_mods <- reactive({
      req(filt$df(),selected_variable())
      filt$df() %>% 
        count_auto(cfg()$var_intvwr, selected_variable()) %>% 
        pull(!!sym(selected_variable())) %>% unique()
    })
    
    output$var_distrib_ui <- renderUI({
      req(dist_mods())
      mods <- dist_mods()
      
      if (length(dist_mods()) >= 15) {
        validate(tr("Too much modality for this variable"))
      }
      
      if (selected_variable() %in% cfg()$vars_continuous) {
        validate(tr("Continuous variable"))
      }
      
      fluidRow(lapply(seq_along(mods), function(i) {
        column(4, plotOutput(ns(paste0("mods_dist_", i)), height = "220px"))
      }))
    })
    
    dist_mods_prop <- reactive({
      filt$df() %>%
        count_auto(cfg()$var_intvwr, selected_variable()) %>% 
        group_by(!!sym(cfg()$var_intvwr)) %>% mutate(prop = n / sum(n)) %>% 
        ungroup()
    })
    
    observeEvent(dist_mods(), {
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
            scale_x_continuous(labels = scales::percent, expand = c(0, 0))+
            coord_cartesian(xlim = c(0, 1)) +
            labs(x = paste(tr("Proportion of"),my_mod), y = tr("% interviewer")) +
            theme_minimal(base_size = 12) +
            geom_vline(xintercept = enq_value, color = "red", 
                       linetype = "dashed", linewidth = 1.2) +
            annotate("text", x = enq_value, y = 0, 
                     label = paste0(tr("Intvwr : "), round(100*enq_value),"%"),
                     vjust = -0.5, hjust = 1.1, color = "red")
          
          p
        })
      }))
    })
    
    
    # --- Information on Interviewer -----------------------------------
    
    output$intvwr_geo <- renderDataTable({
      
      req(selected_intvwr())
      
      df <- filt$df() %>%
        filter(!!sym(cfg()$var_intvwr) == selected_intvwr()) %>%
        group_by(!!sym(cfg()$var_info_geo)) %>%
        count() %>% ungroup() %>% mutate(prop = round(100*n/sum(n))) %>% arrange(-prop)
        
      datatable(df, rownames = FALSE, options = list(pageLength = 15, dom = "t"))
      
    })
    
    output$intvwr_stat <- renderDataTable({
      
      req(selected_intvwr())
      
      df <- data$df() %>%
        filter(!!sym(cfg()$var_intvwr) == selected_intvwr()) %>%
        group_by(!!sym(cfg()$var_wave),!!sym(cfg()$var_filter)) %>%
        count() %>% ungroup() %>% 
        pivot_wider(names_from = !!sym(cfg()$var_wave),values_from = n)
      
      datatable(df, rownames = FALSE, options = list(pageLength = 15, dom = "t"))
      
    })
  })
}
