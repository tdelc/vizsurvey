# ---- Thème ----
vz_theme <- bslib::bs_theme(
  version = 5,
  bg = "#fafbfc",
  fg = "#1a1d29",
  primary = "#2c5f7a",
  secondary = "#6c7a89",
  success = "#5a9f7d",
  info = "#5b8db8",
  warning = "#d4a056",
  # base_font = font_google("Inter"),
  # heading_font = font_google("Inter"),
  # code_font = font_google("JetBrains Mono"),
  "card-border-color" = "#e5e8ec",
  "card-cap-bg"       = "#f4f6f8"
)


# ---- Helper : header de carte avec icône ----
# help = clé de l'aide contextuelle (= id complet de la card, ns compris),
#        voir help_utils.R et help/help_<lang>.md
icon_header <- function(icon, title, ..., help = NULL) {
  card_header(
    class = "d-flex align-items-center gap-2",
    bs_icon(icon, class = "text-primary"),
    span(title, class = "fw-semibold"),
    ...,
    if (!is.null(help)) help_button(help)
  )
}
