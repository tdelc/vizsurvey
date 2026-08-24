#' index_homogeneity
#'
#' @param df dataset
#' @param var_itw groupe variable 
#' @param threahold threahold 
#'
#' @returns dataset
#' @export
#' 
index_homogeneity <- function(df,var_itw,threahold=30){
  df_n_enq <- df %>% count(!!sym(var_itw))
  
  syn <- df_n_enq %>% 
    pivot_longer(cols = -!!sym(var_itw),values_transform=as.character) %>% 
    filter(!is.na(!!sym(var_itw)),!is.na(value),value != "") %>% 
    count(!!sym(var_itw),name,value)
	
  if (nrow(syn) == 0) return (tibble(
    !!sym(var_itw) := character(),
    min = numeric(), mean = numeric(), q1 = numeric(),
    median = numeric(), q3 = numeric(), max = numeric(),
    n = numeric()
  ))
  
  syn %>% 
    group_by(!!sym(var_itw)) %>% mutate(n_enq = max(n)) %>%
    group_by(!!sym(var_itw),name) %>% filter(sum(n) >= threahold) %>%
    group_by(!!sym(var_itw),name) %>% mutate(prop = n/sum(n,na.rm=T)) %>% 
    group_by(!!sym(var_itw),name) %>% filter(n >= 3) %>%
    arrange(!!sym(var_itw),name,-n) %>% filter(dplyr::row_number() == 1) %>% 
    group_by(name) %>% mutate(prop = prop / mean(prop)) %>%
    group_by(!!sym(var_itw)) %>% 
    summarise(
      min    = min(prop,na.rm=T),
      mean   = mean(prop,na.rm=T),
      q1     = quantile(prop,0.25,na.rm=T),
      median = quantile(prop,0.50,na.rm=T),
      q3     = quantile(prop,0.75,na.rm=T),
      max    = max(prop,na.rm=T)
    ) %>% 
    left_join(df_n_enq,by=var_itw) %>% 
    tidyr::replace_na(list(n = 0))
}