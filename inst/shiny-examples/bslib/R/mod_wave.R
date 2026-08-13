# ============================================================================
# mod_wave.R  — onglet "Wave"
# Consomme : filt (df_sub, df_stats_sub), data (config, df), sel (wave, zone, wave_compare)
# ============================================================================

mod_wave_ui <- function(id, i18n) {
  ns <- NS(id)
  nav_panel(
    title = tagList(bs_icon("water"), i18n$t("Wave")),
    value = "tab_wave",
    uiOutput(ns("no_data_msg")),
    checkboxGroupInput(ns("wave_compare"), i18n$t("Compare with"),
                       inline = TRUE, choices = i18n$t("Loading...")),
    
    uiOutput(ns("ui_presence")),
    
    layout_columns(
      col_widths = c(6, 6),
      card(
        id = ns("card_cat"),
        full_screen = TRUE,
        min_height = "650px",
        icon_header("layout-text-sidebar-reverse", i18n$t("Categorical outliers"),
                    help = ns("card_cat")),
        DT::DTOutput(ns("tab_cat"))),
      card(
        id = ns("card_cat_detail"),
        full_screen = TRUE,
        min_height = "650px",
        icon_header("layout-text-sidebar-reverse", i18n$t("Detail of a variable"),
                    help = ns("card_cat_detail")),
        # plotOutput(ns("distri_cat")),
        plotOutput(ns("evo_cat")))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        id = ns("card_num"),
        full_screen = TRUE,
        min_height = "650px",
        icon_header("layout-text-sidebar-reverse", i18n$t("Numeric outliers"),
                    help = ns("card_num")),
        DT::DTOutput(ns("tab_num"))),
      card(
        id = ns("card_num_detail"),
        full_screen = TRUE,
        min_height = "650px",
        icon_header("layout-text-sidebar-reverse", i18n$t("Detail of a variable"),
                    help = ns("card_num_detail")),
        # plotOutput(ns("distri_num")),
        plotOutput(ns("evo_num")))
    )
  )
}

mod_wave_server <- function(id, filt, data, sel, r_focus, i18n_s) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tr <- function(x) i18n_s$t(x)
    cfg <- reactive(data$config())
    
    vars_wave <- reactive(vars_levels(cfg(), "wave"))

    # Les vagues de comparaison sont au même niveau que la vague sélectionnée
    wave_modality <- reactive({
      req(data$df())
      wave_modality <- keys_vars(data$df(), vars_wave(), cfg()$var_wave,
                                 level = sel$wave_level())
      wave_modality <- wave_modality[!wave_modality == sel$wave()]
      if (length(wave_modality) == 0) return(".all") else return(wave_modality)
    })
    
    output$no_data_msg <- renderUI({
      req(wave_modality())
      if (length(wave_modality()) > 1) return(NULL)
      if (length(wave_modality()) == 1 & wave_modality() != ".all") return(NULL)
      return(div(class = "alert alert-warning",
                 tr("No wave in config file, or only one wave in dataset.")))
    })
    
    observeEvent(filt$df(),{
      req(wave_modality())
      updateCheckboxGroupInput(session,"wave_compare",inline=T,
                               # label=data$config()$var_wave,
                               label=tr("Compare with"),
                               choices = wave_modality(),
                               selected = wave_modality()[length(wave_modality())])
    })
    
    df_stats_wave <- reactive({
      req(filt$df_stats_wave())
      
      filt$df_stats_wave() %>%
        filter(!!rlang::sym(data$config()$var_wave) %in%
                 c(sel$wave(), input$wave_compare))
    })
    
    df_wave <- reactive({
      keys <- c(sel$wave(), input$wave_compare)
      df <- data$df()
      df <- df[match_keys(df, vars_wave(), cfg()$var_wave, keys), ]

      # Les graphiques sont au niveau de la sélection (année, ou année / trimestre)
      if (length(vars_wave()) > 1 && sel$wave_level() == 1) {
        df[[cfg()$var_wave]] <- key_level1(df[[cfg()$var_wave]])
      }
      
      df
    })

    stats_outliers <- reactive({
      req(df_stats_wave())
      
      db_longer <- df_stats_wave() %>%
        select(variable,!!sym(data$config()$var_wave),Nrow,
               Nval,type,stat,value)
      
      # Nombre de modalité en tout (pour categorial)
      db_Nrow <- db_longer %>% 
        group_by(variable,type) %>% 
        summarise(Nrow = max(Nrow),Nval = max(Nval))
      
      db_Nmod <- df_wave() %>% 
        select(any_of(cfg()$vars_discretes)) %>% 
        mutate(across(cfg()$vars_discretes, ~ length(unique(.x)))) %>% distinct() %>% 
        pivot_longer(everything(),names_to = "variable", values_to = "value") %>% 
        mutate(
          !!sym(data$config()$var_wave) := "ALL",
          stat = "Nmod"
        ) %>% 
        left_join(db_Nrow, by = "variable")
      
      db_longer <- db_longer %>%
        add_row(db_Nmod) %>% 
        filter(stat %in% c("Nmod","missing","presence","median")) %>%
        group_by(variable,stat) %>%
        mutate(value = case_when(
          sum(Nrow,na.rm = TRUE) < sel$threshold_Nrow() ~ 0,
          sum(Nval,na.rm = TRUE) < sel$threshold_Nval() ~ 0,
          TRUE ~ value
        )) %>%
        ungroup() %>%
        select(-Nrow,-Nval) %>%
        group_by(variable,stat) %>%
        mutate(sd = sd(value,na.rm=T)/mean(value,na.rm=T),
               sd = tidyr::replace_na(sd,0)) %>%
        ungroup() %>%
        arrange(variable,stat)
      
      db_longer %>%
        arrange(!!sym(data$config()$var_wave),stat) %>%
        pivot_wider(
          names_from = c(!!sym(data$config()$var_wave),stat),
          names_sep = "|",
          values_from = c(value,sd)
        )
    })
    
    prepa_tab <- reactive({
      req(stats_outliers())
      stats_outliers() %>%
        filter(if_any(starts_with("sd|"), ~ abs(.x) > sel$threshold_wave()),
               if_all(matches("^value\\|.*\\|presence$"), ~ .x == 1)) %>%
        select(-matches("presence$"))
    })
    
    prepa_tab_cat <- reactive({
      req(prepa_tab())
      prepa_tab() %>% filter(type == "cha")
    })
    
    prepa_tab_num <- reactive({
      req(prepa_tab())
      prepa_tab() %>% filter(type == "num")
    })
    
    output$tab_cat <- DT::renderDT({
      tidy_to_dt(prepa_tab_cat(), sel$threshold_wave(), drop_inds = c("mean", "median"))
    })
    
    output$tab_num <- DT::renderDT({
      tidy_to_dt(prepa_tab_num(), sel$threshold_wave(), drop_inds = c("mean", "Nmod"))
    })
    
    tab_check_data <- reactive({
      req(stats_outliers())
      stats_outliers() %>%
        dplyr::select(variable, type,
                      dplyr::matches("^value.*presence$"),
                      dplyr::matches("^sd.*presence$")) %>%
        dplyr::filter(dplyr::if_any(dplyr::starts_with("sd|"),
                                    ~ !is.na(.x) & abs(.x) > 0))
    })
    
    output$ui_presence <- renderUI({
      if (nrow(tab_check_data()) == 0) return(NULL)
      
      card(
        id = ns("card_presence"),
        full_screen = TRUE,
        height = "200px",
        icon_header("layout-text-sidebar-reverse", tr("Presence outliers"),
                    help = ns("card_presence")),
        DT::DTOutput(ns("tab_check"))
      )
    })
    
    output$tab_check <- DT::renderDT({
      presence_check_dt(tab_check_data())
    })
    
    bind_selection <- function(input_id, df, field, id_col = 1) {
      observeEvent(input[[input_id]], {
        r_focus[[field]] <- dplyr::pull(df()[input[[input_id]], id_col])
      })
    }
    
    bind_selection("tab_cat_rows_selected", prepa_tab_cat, "variable")
    bind_selection("tab_num_rows_selected", prepa_tab_num, "variable")
    
    fill_levels <- reactive({
      req(r_focus$variable)
      levels <- data$df() %>% dplyr::pull(!!sym(r_focus$variable)) %>%
        as.character() %>% unique() %>% sort()
      
      c(levels,".NA")
    })
    
    output$evo_cat <- renderPlot({
      if(is.null(r_focus$variable) | r_focus$variable == ""){
        return(NULL)
      }
      if(!r_focus$variable %in% prepa_tab_cat()$variable){
        validate(tr("Plot only for categorical variable"))
      }
      
      df <- df_wave()
      v <- r_focus$variable
      
      if(length(fill_levels()) > 15){
        validate(tr("Too much modalities for this categorical variable"))
      }
      
      df <- df %>% mutate(!!sym(v) := as.character(!!sym(v)),
                          !!sym(v) := tidyr::replace_na(!!sym(v), ".NA"))
      ggplot(df) +
        aes(x = !!sym(data$config()$var_wave), fill = !!sym(v)) +
        geom_bar(position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_viridis_d(limits = fill_levels(), drop = FALSE) +
        labs(title = tr("Evolution accross waves")) +
        coord_flip() +
        theme_minimal(base_size = 15) +
        theme(legend.position = "bottom")
    })
    
    bornes <- reactive({
      req(r_focus$variable)
      trim <- c(0.01, 0.99)
      df <- data$df()
      bornes <- quantile(df[[r_focus$variable]], probs = trim, na.rm = TRUE)
    })
    
    output$distri_num <- renderPlot({
      if(is.null(r_focus$variable) | r_focus$variable == ""){
        return(NULL)
      }
      if(!r_focus$variable %in% prepa_tab_num()$variable){
        validate(tr("Plot only for continuous variable"))
      }
      
      v <- r_focus$variable
      df <- data$df() %>% 
        filter(between(!!sym(v), bornes()[1], bornes()[2]))
      
      ggplot(df) +
        aes(x = !!sym(v)) +
        geom_histogram(color = "white", fill = viridisLite::viridis(1, begin = 0.35)) +
        theme_minimal(base_size = 15) +
        labs(title = paste(tr("Distribution for wave"),sel$wave()),
             caption = tr("Trimmed at the 1st–99th percentiles"))
    })
    
    output$evo_num <- renderPlot({
      if(is.null(r_focus$variable) | r_focus$variable == ""){
        return(NULL)
      }
      if(!r_focus$variable %in% prepa_tab_num()$variable){
        validate(tr("Plot only for continuous variable"))
      }
      
      v <- r_focus$variable
      df <- df_wave() %>% 
        filter(between(!!sym(v), bornes()[1], bornes()[2]))
      
      ggplot(df) +
        aes(x = !!sym(r_focus$variable),
            fill  = factor(!!sym(data$config()$var_wave)),
            color = factor(!!sym(data$config()$var_wave))) +
        geom_density(alpha = 0.35) +
        scale_fill_viridis_d() +
        scale_color_viridis_d() +
        labs(fill = "Wave", color = "Wave",
             title = tr("Evolution accross waves")) +
        theme_minimal(base_size = 15)
    })
    
  })
}
