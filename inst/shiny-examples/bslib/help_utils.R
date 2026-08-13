# ============================================================================
# help_utils.R — aide contextuelle des cards
#
# Les textes ne sont PAS dans le code : ils vivent dans help/help_<lang>.md,
# un fichier par langue. Chaque section commence par "# <clé>", la clé étant
# l'identifiant complet de la card (ns compris) : "wave-card_cat".
# La première ligne "## ..." de la section sert de titre au modal.
#
#   UI     : icon_header("table", i18n$t("..."), help = ns("card_cat"))
#            ou help_button(key) n'importe où (titre h2, etc.)
#   Server : rien à faire dans les modules, un seul observeEvent(input$help_show)
#            dans app_server suffit (le bouton pousse la clé via JS).
#
# Ajouter une aide = ajouter une section dans help_en.md (+ les autres langues).
# Traduire = copier le fichier, garder les mêmes clés. Une clé absente dans la
# langue courante retombe sur l'anglais.
# ============================================================================

# Découpe un fichier markdown en sections nommées ----------------------------
help_parse <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  starts <- grep("^#\\s+\\S", lines)
  if (length(starts) == 0) return(list())

  keys <- trimws(sub("^#\\s+", "", lines[starts]))
  ends <- c(starts[-1] - 1L, length(lines))

  out <- Map(function(s, e) {
    if (e < s + 1L) "" else paste(lines[(s + 1L):e], collapse = "\n")
  }, starts, ends)

  names(out) <- keys
  out
}

# Charge toutes les langues disponibles : list(en = list(clé = texte), fr = ...)
help_load <- function(dir = "help") {
  files <- list.files(dir, pattern = "^help_.*\\.md$", full.names = TRUE)
  if (length(files) == 0) return(list())

  out <- lapply(files, help_parse)
  names(out) <- sub("^help_", "", tools::file_path_sans_ext(basename(files)))
  out
}

# Titre + corps d'une clé, avec repli sur l'anglais --------------------------
help_entry <- function(key, lang = "en", help = HELP) {
  txt <- help[[lang]][[key]]
  if (is.null(txt)) txt <- help[["en"]][[key]]
  if (is.null(txt)) return(list(title = NULL, body = NULL))

  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  title <- NULL
  if (length(lines) && grepl("^##\\s+", lines[1])) {
    title <- trimws(sub("^##\\s+", "", lines[1]))
    lines <- lines[-1]
  }
  list(title = title, body = paste(lines, collapse = "\n"))
}

# Bouton "?" — pousse la clé dans input$help_show (session principale) -------
help_button <- function(key, class = "ms-auto") {
  tags$button(
    type = "button",
    class = paste("btn btn-sm btn-link p-0 border-0 text-secondary", class),
    title = "?", `aria-label` = "Help",
    onclick = sprintf(
      "Shiny.setInputValue('help_show', '%s', {priority: 'event'});", key),
    bsicons::bs_icon("question-circle")
  )
}

# Modal d'aide ---------------------------------------------------------------
help_modal <- function(key, lang = "en", help = HELP, close_label = "Close") {
  e <- help_entry(key, lang, help)

  body <- if (is.null(e$body) || !nzchar(trimws(e$body))) {
    div(class = "text-muted", sprintf("No help available for '%s'.", key))
  } else {
    shiny::markdown(e$body)
  }

  modalDialog(
    title = e$title, body,
    easyClose = TRUE, size = "l",
    footer = modalButton(close_label)
  )
}
