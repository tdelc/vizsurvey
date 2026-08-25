#### Description d'une variable (remplace summarytools::dfSummary) ####

# Formatage : entiers avec séparateur d'espace, réels arrondis, NA -> "—"
fmt_num <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x))) return("—")
  if (is.finite(x) && x == round(x) && abs(x) < 1e15) digits <- 0
  format(round(x, digits), big.mark = " ", trim = TRUE, scientific = FALSE,
         nsmall = digits)
}

# "3 095 (78.1%)" : effectif et part, comme le faisait summarytools
fmt_n_pct <- function(n, total, digits = 1) {
  if (is.na(n)) return("—")
  if (is.na(total) || total == 0) return(fmt_num(n))
  paste0(fmt_num(n), " (", format(round(100 * n / total, digits), nsmall = digits,
                                  trim = TRUE), "%)")
}

# Colonne d'une variable catégorielle, pour un groupe
describe_col_cat <- function(x, levels_kept, other, digits) {
  x <- as.character(x)
  n_tot <- length(x)
  n_na  <- sum(is.na(x))
  n_val <- n_tot - n_na
  
  if (length(other)) x[!is.na(x) & x %in% other] <- "(other)"
  cnt <- table(factor(x, levels = c(levels_kept, if (length(other)) "(other)")))
  
  c(vapply(cnt, fmt_n_pct, character(1), total = n_val, digits = digits),
    "(missing)" = fmt_n_pct(n_na, n_tot, digits),
    "Total"     = fmt_num(n_tot))
}

# Colonne d'une variable continue, pour un groupe
describe_col_num <- function(x, digits) {
  x     <- suppressWarnings(as.numeric(x))
  n_tot <- length(x)
  ok    <- x[!is.na(x)]
  q     <- if (length(ok)) stats::quantile(ok, c(0.25, 0.5, 0.75), names = FALSE)
  else rep(NA_real_, 3)
  
  c("N (valid)"  = fmt_num(length(ok)),
    "(missing)"  = fmt_n_pct(n_tot - length(ok), n_tot, digits),
    "Mean"       = fmt_num(if (length(ok)) mean(ok) else NA, digits),
    "SD"         = fmt_num(if (length(ok) > 1) stats::sd(ok) else NA, digits),
    "Min"        = fmt_num(if (length(ok)) min(ok) else NA, digits),
    "Q1"         = fmt_num(q[1], digits),
    "Median"     = fmt_num(q[2], digits),
    "Q3"         = fmt_num(q[3], digits),
    "Max"        = fmt_num(if (length(ok)) max(ok) else NA, digits),
    "N distinct" = fmt_num(length(unique(ok))))
}

#' Description d'une variable, globale ou par groupe
#'
#' Remplace summarytools::dfSummary. Renvoie un data.frame prêt à afficher :
#' une ligne par modalité (variables catégorielles) ou par statistique
#' (variables continues), une colonne par groupe.
#'
#' Les pourcentages des modalités sont calculés sur les valeurs valides,
#' celui de la ligne "(missing)" sur l'ensemble des lignes.
#'
#' @param x vecteur à décrire
#' @param by (optionnel) vecteur de même longueur, un bloc de colonnes par valeur
#' @param type "auto" (numérique et plus de 15 valeurs distinctes -> continue),
#'   "categorical" ou "continuous"
#' @param max_levels nombre maximum de modalités affichées, le reste est
#'   regroupé dans "(other)"
#' @param digits nombre de décimales
#'
#' @returns data.frame
#' @export
describe_variable <- function(x, by = NULL,
                              type = c("auto", "categorical", "continuous"),
                              max_levels = 20, digits = 1) {
  type <- match.arg(type)
  
  if (is.null(by)) by <- rep("Total", length(x))
  if (length(by) != length(x))
    stop("`by` doit avoir la meme longueur que `x`.", call. = FALSE)
  
  by <- as.character(by)
  by[is.na(by)] <- "(NA)"
  groups <- sort(unique(by))
  if (length(groups) == 0) groups <- "Total"   # vecteur vide
  
  if (type == "auto")
    type <- if (is.numeric(x) && length(unique(x[!is.na(x)])) > 15)
      "continuous" else "categorical"
  
  if (type == "continuous") {
    cols <- lapply(groups, function(g) describe_col_num(x[by == g], digits))
    first <- "Statistic"
  } else {
    # Modalités communes à tous les groupes, les plus rares regroupées
    tab <- sort(table(as.character(x), useNA = "no"), decreasing = TRUE)
    lev <- names(tab)
    other <- character(0)
    if (length(lev) > max_levels) {
      other <- lev[max_levels:length(lev)]
      lev   <- lev[seq_len(max_levels - 1)]
    }
    # tri naturel : 1, 2, 10 plutot que 1, 10, 2 quand les codes sont numeriques
    num <- suppressWarnings(as.numeric(lev))
    lev <- if (length(lev) && !anyNA(num)) lev[order(num)] else sort(lev)
    
    cols <- lapply(groups, function(g)
      describe_col_cat(x[by == g], lev, other, digits))
    first <- "Value"
  }
  
  out <- data.frame(names(cols[[1]]), stringsAsFactors = FALSE)
  for (i in seq_along(groups)) out[[groups[i]]] <- unname(cols[[i]])
  names(out)[1] <- first
  
  attr(out, "type") <- type
  out
}

#' Description d'une variable rendue en DT
#'
#' @inheritParams describe_variable
#' @param caption (optionnel) titre du tableau
#'
#' @returns datatable
#' @export
describe_variable_dt <- function(x, by = NULL, ..., caption = NULL) {
  d <- describe_variable(x, by = by, ...)
  
  DT::datatable(
    d, rownames = FALSE, caption = caption,
    options = list(dom = "t", paging = FALSE, ordering = FALSE,
                   columnDefs = list(list(className = "dt-right",
                                          targets = seq_len(ncol(d) - 1))))
  ) %>%
    DT::formatStyle(names(d)[1], fontWeight = "500") %>%
    DT::formatStyle(names(d)[1], target = "row",
                    backgroundColor = DT::styleEqual(
                      c("(missing)", "Total"), c("#f4f6f8", "#f4f6f8")))
}

#' Vecteur de groupe "VAR = valeur" / "VAR != valeur" pour les comparaisons
#'
#' @param x Vecteur a regrouper
#' @param value valeur discrétionnaire
#' @param label label à utiliser
#'
#' @returns vector
#' @export
group_vs_others <- function(x, value, label) {
  ifelse(!is.na(x) & x == value,
         paste(label, "=", value),
         paste(label, "!=", value))
}
