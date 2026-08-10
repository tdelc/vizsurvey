#' prepa_stats_dt
#'
#' @param df 
#' @param var_group 
#' @param configs 
#' @param na.rm 
#'
#' @returns data.frame
#' @export
prepa_stats_dt <- function(df, var_group, configs, na.rm = FALSE) {
  
  vars_discretes   <- configs$vars_discretes
  
  if (length(var_group) == 0) {
    return(tibble(NULL))
  }
  
  if (is.null(vars_discretes)){
    info_vars <- classify_df_pattern(df, configs)
    vars_discretes <- info_vars$vars_discretes
  }
  
  vars_discretes <- setdiff(vars_discretes, var_group)
  vars_discretes <- intersect(vars_discretes, names(df))

  df <- df %>%
    mutate(
      across(any_of(vars_discretes), as.factor),
      across(any_of(vars_discretes), as.numeric),
      across(any_of(var_group), as.character)
    )
  
  setDT(df)
  ldist <- list_dist_dt(df,vars_discretes,na.rm=na.rm)
  
  useNA <- if (na.rm) "ifany" else "no"
  
  exprs_vd <- lapply(vars_discretes, function(nm) {
    list(
      substitute(sum(!is.na(x)), list(x = as.name(nm))),
      substitute(mean(is.na(x)), list(x = as.name(nm))),
      substitute(mean(is.na(x)) != 1, list(x = as.name(nm))),
      substitute(if(is.na(mean(is.na(x))) | mean(is.na(x)) > 0.95) 
        NA_integer_ else data.table::uniqueN(x), list(x = as.name(nm))),
      substitute(my_chisq_test(x, vname, ldist, useNA), list(x = as.name(nm), vname = nm))
    )
  })
  
  all_exprs <- c(list(Nrow = quote(.N)), unlist(exprs_vd))
  
  names_vd <- expand.grid(stat = c("Nval", "missing", "presence", "Nmod", "chi2"), col = vars_discretes)

  names(all_exprs) <- c("Nrow", paste(names_vd$col, "cha", names_vd$stat, sep="|"))
  
  df_stats <- df[, eval(as.call(c(quote(list), all_exprs))), by = var_group]
  
  df_stats <- df_stats %>%
    pivot_longer(
      cols = -c(!!sym(var_group), Nrow),
      names_to = c("variable", "type", "stat"),
      names_pattern = "^(.*?)\\|(.*?)\\|(.*?)$"
    ) %>%
    group_by(!!sym(var_group), variable) %>%
    mutate(
      Nval = ifelse(stat == "Nval", value, NA),
      Nval = mean(Nval, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(stat != "Nval") %>%
    group_by(variable, type, stat) %>%
    mutate(
      value = ifelse(is.infinite(value), 1000, value),
      value_ref = mean(value, na.rm = TRUE),
      standard = abs(scale_IQR(value)),
      standard = case_when(
        sd(value, na.rm = TRUE) == 0 ~ 0,
        is.nan(standard) ~ NA,
        TRUE ~ standard
      )
    ) %>%
    dplyr::relocate(!!sym(var_group), variable, Nrow, Nval) %>%
    ungroup()
  
  return(df_stats)
}

#' List of proportions for categorical variables
#'
#' @param df data.frame
#' @param vars_discretes vector of categorical variable to calculate proportion 
#' @param na.rm Remove missing values or not ?
#'
#' @returns list
#' @export
#' 
#' @importFrom data.table setDT := .N
list_dist_dt <- function(df, vars_discretes, na.rm = FALSE) {
  if (!is.data.table(df)) setDT(df)
  
  res <- lapply(vars_discretes, function(nm) {
    dt_prop <- df[, .N, by = c(nm)]
    data.table::setnames(dt_prop, nm, "category")
    dt_prop[, category := as.character(category)]
    if (na.rm){
      dt_prop <- dt_prop[!is.na(category)]
    }else{
      dt_prop[is.na(category), category := "NA_"]
    }
    total <- sum(dt_prop$N)
    dt_prop[, prop := N / total]
    dt_prop[prop < 0.01, category := "OTH_"]
    final <- dt_prop[, .(prop = sum(prop)), by = category]
    setNames(final$prop, final$category)
  })
  
  names(res) <- vars_discretes
  return(res)
}