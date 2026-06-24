# ## ============================================================
# ##  timer.R  —  Pipeline "Analyse par enquêteur·rice" (timers)
# ## ============================================================
# 
# # -----------------------------
# # CONFIG
# # -----------------------------
# read_timer_config <- function() {
#   list(
#     duree_min   = 15,
#     duree_inter = 15,
#     debut_nuit  = 21,
#     fin_nuit    = 6,
#     vec_nuit    = c(21:23, 0:6)
#   )
# }
# 
# # -----------------------------
# # LECTURE
# # -----------------------------
# read_timer_csv <- function(file_timer) {
#   # df <- read_csv(file_timer, show_col_types = FALSE)
#   df <- as_tibble(fread(file_timer,encoding="UTF-8"))
#   colnames(df) <- toupper(colnames(df))
#   return(df)
# }
# 
# # read_timer_csvs <- function(path_timer) {
# #   files <- list.files(path_timer, full.names = TRUE, pattern = "\\.csv$")
# #   dfs   <- lapply(files, read_timer_csv)
# #   dfs   <- dfs[sapply(dfs, nrow) > 0]
# #   if (length(dfs) == 0) return(tibble())
# #   bind_rows(dfs)
# # }
# 
# read_timer_csvs <- function(files_timer) {
#   # files <- list.files(path_timer, full.names = TRUE, pattern = "\\.csv$")
#   dfs   <- lapply(files_timer, read_timer_csv)
#   dfs   <- dfs[sapply(dfs, nrow) > 0]
#   if (length(dfs) == 0) return(tibble())
#   bind_rows(dfs)
# }
# 
# # Détecte le CSV timer dans un dossier 
# find_timer_csv <- function(folder) {
#   files <- list.files(folder, full.names = TRUE, pattern = "\\.csv$")
#   # priorité : fichier avec "timer" dans le nom
#   timer_files <- files[grepl("timer", basename(files), ignore.case = TRUE)]
#   if (length(timer_files) > 0) return(timer_files[1])
#   # fallback : fichier dont les colonnes contiennent NR_INTVWR et TM_BLS_ITM_STRT
#   for (f in files) {
#     cols <- toupper(colnames(read_csv(f, n_max = 0, show_col_types = FALSE)))
#     if (all(c("NR_INTVWR", "TM_BLS_ITM_STRT") %in% cols)) return(f)
#   }
#   return(NULL)
# }
# 
# # Détecte le RDS timer dans un dossier 
# find_timer_rds <- function(folder) {
#   files <- list.files(folder, full.names = TRUE, pattern = "timers.rds$")
#   if (length(files) == 0) return(NULL)
#   if (length(files) > 1) files <- files[1]
#   return(files)
# }
# 
# # -----------------------------
# # PREP DATA
# # -----------------------------
# correct_df_timer <- function(df_timer) {
#   df_timer %>%
#     mutate(
#       NR_INTVWR       = as.character(NR_INTVWR),
#       NR_BLS_SSN      = as.character(NR_BLS_SSN),
#       NR_BLS_SSN_SQ   = as.integer(NR_BLS_SSN_SQ),
#       TX_BLS_PTH      = as.character(TX_BLS_PTH),
#       FL_BLS_ITM_MDF  = as.integer(FL_BLS_ITM_MDF),
#       # TM_BLS_ITM_STRT = as.POSIXct(TM_BLS_ITM_STRT, tz = "UTC"),
#       MS_BLS_ITM_TM   = as.numeric(MS_BLS_ITM_TM),
#       TX_QSTNR_SCTN   = str_extract(TX_BLS_PTH, "^[^.]+"),
#       
#       DT_SVY_YR        = substr(NR_DBENQ_HH, 1, 4),
#       TM_BLS_ITM_STRT  = dmy_hms(TM_BLS_ITM_STRT),
#       TM_BLS_SSN_STRT  = dmy_hms(TM_BLS_SSN_STRT),
#       NR_DBENQ_HH      = as.character(NR_DBENQ_HH),
#       NR_DBENQ_GRP     = as.character(NR_DBENQ_GRP),
#       NR_MB            = as.character(NR_MB),
#       NR_MB            = str_pad(NR_MB, 2, pad = "0"),
#       NR_DBENQ_MB      = if_else(is.na(NR_MB), NA,
#                                  paste0(substr(NR_DBENQ_HH, 1, 14), NR_MB)),
#       MS_BLS_ITM_TM    = MS_BLS_ITM_TM / 10 # Changement en secondes
#       
#     ) %>%
#     group_by(DT_SVY_YR, NR_INTVWR, NR_DBENQ_HH) %>% 
#     mutate(DT_SVY_QTR = quarter(mean(TM_BLS_ITM_STRT))) %>% 
#     ungroup() %>% 
#     arrange(NR_INTVWR, NR_DBENQ_HH, TM_BLS_ITM_STRT)
# }
# 
# correct_df_timer_session <- function(df_timer) {
#   mapping <- df_timer %>%
#     select(NR_INTVWR, NR_DBENQ_HH, NR_BLS_SSN, TM_BLS_ITM_STRT) %>%
#     distinct() %>%
#     group_by(NR_INTVWR, NR_DBENQ_HH, NR_BLS_SSN) %>%
#     summarise(TM_MIN = min(TM_BLS_ITM_STRT, na.rm = TRUE), .groups = "drop") %>%
#     group_by(NR_INTVWR, NR_DBENQ_HH) %>%
#     arrange(TM_MIN, .by_group = TRUE) %>%
#     mutate(NR_BLS_SSN_SQ = row_number()) %>%
#     ungroup() %>%
#     select(NR_INTVWR, NR_DBENQ_HH, NR_BLS_SSN, NR_BLS_SSN_SQ)
# 
#   df_timer %>%
#     select(-NR_BLS_SSN_SQ) %>%
#     left_join(mapping, by = c("NR_INTVWR", "NR_DBENQ_HH", "NR_BLS_SSN"))
# }
# 
# compute_timer_groups <- function(df_timer, cfg) {
#   df_timer %>%
#     group_by(DT_SVY_YR, NR_INTVWR, NR_DBENQ_HH) %>%
#     mutate(
#       NB_SSN              = n_distinct(NR_BLS_SSN_SQ),
#       MIN_TM_BLS_ENQ_STRT = min(TM_BLS_ITM_STRT, na.rm = TRUE),
#       MAX_TM_BLS_ENQ_STRT = max(TM_BLS_ITM_STRT, na.rm = TRUE)
#     ) %>%
#     group_by(DT_SVY_YR, NR_INTVWR, NR_DBENQ_HH, NR_BLS_SSN_SQ) %>%
#     mutate(
#       MIN_TM_BLS_SSN_STRT = min(TM_BLS_ITM_STRT, na.rm = TRUE),
#       MAX_TM_BLS_SSN_STRT = max(TM_BLS_ITM_STRT, na.rm = TRUE)
#     ) %>%
#     ungroup() %>%
#     mutate(FL_NUIT = hour(TM_BLS_ITM_STRT) %in% cfg$vec_nuit)
# }
# 
# # -----------------------------
# # SYNTHÈSES
# # -----------------------------
# build_df_timer_day <- function(df_timer) {
#   df_timer %>%
#     mutate(DT_ENQ = as.Date(MAX_TM_BLS_ENQ_STRT)) %>%
#     distinct(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR, NR_DBENQ_HH, DT_ENQ) %>%
#     group_by(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR, DT_ENQ) %>%
#     mutate(NB_ENQ_DAY = n()) %>%
#     group_by(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR) %>%
#     mutate(NB_ENQ = n()) %>%
#     group_by(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR, NB_ENQ) %>%
#     summarise(MAX_NB_ENQ_DAY = max(NB_ENQ_DAY), .groups = "drop")
# }
# 
# build_df_timer_sctn <- function(df_timer) {
#   df_timer %>%
#     group_by(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR, NR_DBENQ_HH, NR_BLS_SSN_SQ) %>%
#     count(TX_QSTNR_SCTN, name = "N") %>%
#     mutate(flag = "\U0001f7e2") %>%
#     group_by(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR, NR_DBENQ_HH) %>%
#     complete(NR_BLS_SSN_SQ, TX_QSTNR_SCTN) %>%
#     replace_na(list(flag = "\U0001f534")) %>%
#     arrange(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR, NR_DBENQ_HH, NR_BLS_SSN_SQ, TX_QSTNR_SCTN) %>%
#     group_by(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR, NR_DBENQ_HH, NR_BLS_SSN_SQ) %>%
#     summarise(CHECK_SCTN = paste0(flag, collapse = ""), .groups = "drop")
# }
# 
# build_df_timer_ssn <- function(df_timer, df_timer_sctn) {
#   df_timer %>%
#     group_by(
#       DT_SVY_YR, DT_SVY_QTR, NR_INTVWR, NR_DBENQ_HH,
#       NR_BLS_SSN_SQ, NB_SSN, MIN_TM_BLS_SSN_STRT, MAX_TM_BLS_SSN_STRT
#     ) %>%
#     summarise(
#       FL_NUIT   = max(FL_NUIT, na.rm = TRUE) == 1,
#       NB_MB     = n_distinct(NR_MB, na.rm = TRUE),
#       DUREE_SSN = sum(MS_BLS_ITM_TM, na.rm = TRUE) / 60,
#       .groups   = "drop"
#     ) %>%
#     arrange(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR, NR_DBENQ_HH, MIN_TM_BLS_SSN_STRT) %>%
#     group_by(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR, NR_DBENQ_HH) %>%
#     mutate(
#       DUREE_INTER_SSN = as.numeric(
#         difftime(MIN_TM_BLS_SSN_STRT, lag(MAX_TM_BLS_SSN_STRT), units = "mins")
#       )
#     ) %>%
#     ungroup() %>%
#     left_join(
#       df_timer_sctn,
#       by = c("DT_SVY_YR", "DT_SVY_QTR", "NR_INTVWR", "NR_DBENQ_HH", "NR_BLS_SSN_SQ")
#     )
# }
# 
# build_df_timer_enq <- function(df_timer_ssn, cfg, df_timer_day) {
#   df_timer_enq <- df_timer_ssn %>%
#     group_by(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR, NR_DBENQ_HH, NB_SSN) %>%
#     summarise(
#       MIN_TM_BLS_ENQ_STRT = min(MIN_TM_BLS_SSN_STRT, na.rm = TRUE),
#       MAX_TM_BLS_ENQ_STRT = max(MAX_TM_BLS_SSN_STRT, na.rm = TRUE),
#       DUREE_ENQ           = sum(DUREE_SSN, na.rm = TRUE),
#       FL_NUIT             = max(FL_NUIT, na.rm = TRUE),
#       .groups             = "drop"
#     ) %>%
#     arrange(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR, MIN_TM_BLS_ENQ_STRT) %>%
#     group_by(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR) %>%
#     mutate(
#       DUREE_INTER_ENQ = as.numeric(
#         difftime(MIN_TM_BLS_ENQ_STRT, lag(MAX_TM_BLS_ENQ_STRT), units = "mins")
#       ),
#       FL_DUREE    = DUREE_ENQ < cfg$duree_min,
#       FL_INTER    = DUREE_INTER_ENQ < cfg$duree_inter & DUREE_INTER_ENQ > 0,
#       FL_INTERCAL = DUREE_INTER_ENQ < 0,
#       FL_NB_SSN   = NB_SSN > 1,
#       FL_NUIT     = FL_NUIT == 1
#     ) %>%
#     ungroup() %>%
#     relocate(FL_NUIT, .after = FL_NB_SSN)
# 
#   df_timer_enq %>%
#     left_join(df_timer_day, by = c("DT_SVY_YR", "DT_SVY_QTR", "NR_INTVWR"))
# }
# 
# # -----------------------------
# # SYNTHÈSE PAR ENQUÊTEUR
# # -----------------------------
# build_df_timer_intvwr <- function(df_timer_enq) {
#   df_timer_enq %>%
#     group_by(DT_SVY_YR, DT_SVY_QTR, NR_INTVWR) %>%
#     summarise(
#       NB_ENQ          = n(),
#       DUREE_MED       = round(median(DUREE_ENQ,      na.rm = TRUE), 1),
#       DUREE_MIN_VAL   = round(min(DUREE_ENQ,         na.rm = TRUE), 1),
#       PCT_COURT       = round(mean(FL_DUREE,          na.rm = TRUE) * 100, 1),
#       PCT_NUIT        = round(mean(FL_NUIT,           na.rm = TRUE) * 100, 1),
#       PCT_INTER       = round(mean(FL_INTER,          na.rm = TRUE) * 100, 1),
#       PCT_INTERCAL    = round(mean(FL_INTERCAL,       na.rm = TRUE) * 100, 1),
#       PCT_MULTI_SSN   = round(mean(FL_NB_SSN,        na.rm = TRUE) * 100, 1),
#       MAX_ENQ_DAY     = max(MAX_NB_ENQ_DAY,          na.rm = TRUE),
#       .groups         = "drop"
#     )
# }
# 
# # -----------------------------
# # FONCTION GÉNÉRALE
# # -----------------------------
# prepa_timer_from_df <- function(df_timer_raw) {
#   cfg <- read_timer_config()
# 
#   df_timer <- df_timer_raw %>% correct_df_timer()
#   
#   df_timer_add <- df_timer %>%
#     correct_df_timer_session() %>%
#     compute_timer_groups(cfg)
# 
#   df_timer_day   <- build_df_timer_day(df_timer_add)
#   df_timer_sctn  <- build_df_timer_sctn(df_timer_add)
#   df_timer_ssn   <- build_df_timer_ssn(df_timer_add, df_timer_sctn)
#   df_timer_enq   <- build_df_timer_enq(df_timer_ssn, cfg, df_timer_day)
#   df_timer_intvwr <- build_df_timer_intvwr(df_timer_enq)
# 
#   list(
#     cfg             = cfg,
#     df_timer        = df_timer,
#     df_timer_day    = df_timer_day,
#     df_timer_sctn   = df_timer_sctn,
#     df_timer_ssn    = df_timer_ssn,
#     df_timer_enq    = df_timer_enq,
#     df_timer_intvwr = df_timer_intvwr
#   )
# }
