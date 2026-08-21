# ============================================================================
# mod_tour.R — visite guidée de l'application
#
# Un bouton dans la barre de navigation déroule un parcours d'étapes : chaque
# étape ouvre un onglet, entoure un élément de l'écran et affiche une bulle
# avec un texte, sur les données actuellement chargées.
#
# Le contenu n'est PAS dans le code : il vient de help/tour_<lang>.md, sur le
# même principe que l'aide contextuelle (help_parse() est réutilisé).
# Une étape = une section :
#
#   # 03-filtres
#   tab: tab_summary
#   target: #filters-config_wave
#   ## Choisir la vague et le filtre
#   Tout ce qui est affiché porte sur la vague {wave} et le filtre {filter}.
#
#   tab     : onglet à ouvrir (facultatif ; valeur d'un nav_panel)
#   target  : sélecteur CSS de l'élément à entourer (facultatif -> bulle centrée)
#   ## ...  : titre de la bulle ; la suite est du markdown
#   {xxx}   : remplacé par les valeurs de tour_values() (données courantes)
#
# Ajouter une étape = copier un bloc. La réordonner = déplacer le bloc.
# Ajouter une valeur {xxx} = ajouter une entrée dans tour_values().
#
# Les fichiers sont relus à chaque lancement du tutoriel : on peut donc les
# corriger sans redémarrer l'application.
# ============================================================================

# --- Lecture des étapes -----------------------------------------------------

# Une section de tour_<lang>.md -> list(tab, target, title, body)
tour_parse_step <- function(txt) {
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  drop_empty <- function(l) { while (length(l) && !nzchar(trimws(l[1]))) l <- l[-1]; l }

  lines <- drop_empty(lines)

  meta <- list()
  while (length(lines) && grepl("^[a-z_]+:", lines[1])) {
    kv <- strsplit(lines[1], ":", fixed = TRUE)[[1]]
    meta[[trimws(kv[1])]] <- trimws(paste(kv[-1], collapse = ":"))
    lines <- drop_empty(lines[-1])
  }

  title <- NULL
  if (length(lines) && grepl("^##\\s+", lines[1])) {
    title <- trimws(sub("^##\\s+", "", lines[1]))
    lines <- lines[-1]
  }

  list(tab    = meta$tab,
       target = meta$target,
       title  = title,
       body   = paste(lines, collapse = "\n"))
}

# Étapes de la langue courante, avec repli sur l'anglais
tour_steps <- function(lang = "en", dir = "help") {
  f <- file.path(dir, paste0("tour_", lang, ".md"))
  if (!file.exists(f)) f <- file.path(dir, "tour_en.md")
  if (!file.exists(f)) return(list())
  lapply(help_parse(f), tour_parse_step)
}

# --- Valeurs injectées dans les textes --------------------------------------

# Chaque élément devient un {nom} utilisable dans les fichiers markdown.
# Les reactives protégées par req() peuvent échouer : on renvoie "?" alors.
tour_values <- function(data, filt, sel) {
  safe <- function(expr, default = "?")
    tryCatch({ v <- expr; if (length(v) == 0 || is.null(v)) default else v },
             error = function(e) default)
  fmt <- function(x) format(x, big.mark = " ", trim = TRUE)

  cfg <- safe(data$config(), list())
  df  <- safe(data$df(), NULL)

  list(
    survey     = safe(paste(cfg$name_survey, collapse = " ")),
    n_rows     = safe(fmt(nrow(df))),
    n_vars     = safe(fmt(length(cfg$vars_discretes) + length(cfg$vars_continuous))),
    n_intvwr   = safe(fmt(length(unique(as.character(df[[cfg$var_intvwr]]))))),
    n_waves    = safe(fmt(length(keys_vars(df, vars_levels(cfg, "wave"),
                                           cfg$var_wave, level = 1)))),
    wave       = safe(sel$wave()),
    filter     = safe(sel$filter()),
    n_outliers = safe(fmt(nrow(filt$df_var_ranking()))),
    var_intvwr = safe(cfg$var_intvwr),
    var_wave   = safe(paste(vars_levels(cfg, "wave"), collapse = " / "))
  )
}

tour_fill <- function(txt, values) {
  for (k in names(values))
    txt <- gsub(paste0("{", k, "}"), as.character(values[[k]]), txt, fixed = TRUE)
  txt
}

# --- UI ---------------------------------------------------------------------

# Bouton de la barre de navigation
mod_tour_ui <- function(id, i18n) {
  ns <- NS(id)
  bslib::nav_item(
    actionButton(ns("start"),
                 tagList(bs_icon("compass"), i18n$t("Guided tour")),
                 class = "btn-sm")
  )
}

# CSS + JS de la bulle : à placer dans le header = de page_navbar
tour_assets <- function() {
  tags$head(
    tags$style(HTML("
      .vz-tour-target {
        outline: 3px solid #d4a056 !important;
        outline-offset: 3px;
        border-radius: .5rem;
      }
      .vz-tour-box {
        position: fixed; z-index: 2000; display: none;
        width: min(400px, 92vw);
        background: #fff; border: 1px solid #e5e8ec; border-radius: .5rem;
        box-shadow: 0 8px 30px rgba(0,0,0,.18);
        padding: 1rem 1.1rem; font-size: .95rem;
      }
      .vz-tour-box h5 {
        color: #2c5f7a; font-weight: 600; font-size: 1.05rem; margin: 0 0 .5rem 0;
      }
      .vz-tour-box p:last-of-type { margin-bottom: 0; }
      .vz-tour-foot {
        display: flex; align-items: center; gap: .4rem; margin-top: .9rem;
      }
      .vz-tour-foot .vz-step {
        color: #6c7a89; font-size: .85rem; margin-right: auto;
      }
    ")),
    tags$script(HTML("
      (function() {
        var box = null, target = null;

        function send(a) {
          Shiny.setInputValue('vz_tour_action', {action: a, ts: Date.now()},
                              {priority: 'event'});
        }

        function build() {
          box = document.createElement('div');
          box.className = 'vz-tour-box';
          document.body.appendChild(box);

          box.addEventListener('click', function(e) {
            var b = e.target.closest('[data-vz]');
            if (b) send(b.dataset.vz);
          });

          document.addEventListener('keydown', function(e) {
            if (!box || box.style.display !== 'block') return;
            if (e.key === 'Escape')     send('quit');
            if (e.key === 'ArrowRight') send('next');
            if (e.key === 'ArrowLeft')  send('prev');
          });
        }

        function unhighlight() {
          if (target) { target.classList.remove('vz-tour-target'); target = null; }
        }

        function position(el) {
          var m = 12, w = box.offsetWidth, h = box.offsetHeight, top, left;
          if (el) {
            var r = el.getBoundingClientRect();
            top  = r.bottom + m;
            left = r.left;
            if (top + h > window.innerHeight - m) top = Math.max(m, r.top - h - m);
            if (left + w > window.innerWidth - m)
              left = Math.max(m, window.innerWidth - w - m);
          } else {
            top  = Math.max(m, (window.innerHeight - h) / 2);
            left = (window.innerWidth - w) / 2;
          }
          box.style.top  = top + 'px';
          box.style.left = left + 'px';
        }

        Shiny.addCustomMessageHandler('vz_tour', function(msg) {
          if (!box) build();

          // R peut envoyer des vecteurs de longueur 1 : on ramene au scalaire
          var one = function(v) { return Array.isArray(v) ? v[0] : v; };
          var L = msg.labels || {};
          Object.keys(L).forEach(function(k) { L[k] = one(L[k]); });

          if (String(one(msg.action)) === 'stop') {
            unhighlight();
            box.style.display = 'none';
            return;
          }

          var selector = one(msg.selector) || '',
              title    = one(msg.title)    || '',
              html     = one(msg.html)     || '',
              step     = +one(msg.step),
              total    = +one(msg.total),
              delay    = +one(msg.delay) || 200;

          unhighlight();
          // laisse à Shiny le temps de changer d'onglet et de dessiner
          setTimeout(function() {
            var el = selector ? document.querySelector(selector) : null;
            if (el) {
              el.classList.add('vz-tour-target');
              target = el;
              el.scrollIntoView({behavior: 'smooth', block: 'center'});
            }

            var last = step >= total;
            box.innerHTML =
              (title ? '<h5>' + title + '</h5>' : '') + html +
              '<div class=\"vz-tour-foot\">' +
                '<span class=\"vz-step\">' + L.step + ' ' + step + '/' + total +
                    '</span>' +
                '<button class=\"btn btn-sm btn-link\" data-vz=\"quit\">' +
                    L.quit + '</button>' +
                (step > 1 ?
                  '<button class=\"btn btn-sm btn-outline-secondary\" data-vz=\"prev\">' +
                    L.prev + '</button>' : '') +
                '<button class=\"btn btn-sm btn-primary\" data-vz=\"next\">' +
                    (last ? L.end : L.next) + '</button>' +
              '</div>';

            box.style.display = 'block';
            position(el);
            setTimeout(function() { position(el); }, 400);  // après le scroll
          }, delay);
        });
      })();
    "))
  )
}

# --- Server -----------------------------------------------------------------

mod_tour_server <- function(id, data, filt, sel, main_session, i18n_s) {
  moduleServer(id, function(input, output, session) {
    tr <- function(x) i18n_s$t(x)

    rv <- reactiveValues(steps = list(), i = 0)

    stop_tour <- function() {
      rv$i <- 0
      session$sendCustomMessage("vz_tour", list(action = "stop"))
    }

    show_step <- function(i) {
      s <- rv$steps[[i]]

      if (!is.null(s$tab) && nzchar(s$tab))
        bslib::nav_select("main_nav", s$tab, session = main_session)

      vals <- tour_values(data, filt, sel)

      session$sendCustomMessage("vz_tour", list(
        selector = if (is.null(s$target)) "" else s$target,
        title    = tour_fill(if (is.null(s$title)) "" else s$title, vals),
        html     = as.character(shiny::markdown(tour_fill(s$body, vals))),
        step     = i,
        total    = length(rv$steps),
        delay    = if (is.null(s$tab)) 150 else 500,
        labels   = list(prev = tr("Previous"), `next` = tr("Next"),
                        end  = tr("Finish"),   quit   = tr("Quit"),
                        step = tr("Step"))
      ))
    }

    observeEvent(input$start, {
      rv$steps <- tour_steps(main_session$input$selected_lang %||% "fr")
      if (length(rv$steps) == 0) {
        showNotification(tr("No tour available."), type = "warning")
        return()
      }
      rv$i <- 1
      show_step(1)
    })

    observeEvent(main_session$input$vz_tour_action, {
      req(rv$i > 0)
      action <- main_session$input$vz_tour_action$action

      if (identical(action, "quit")) return(stop_tour())

      i <- rv$i + if (identical(action, "prev")) -1 else 1
      if (i < 1 || i > length(rv$steps)) return(stop_tour())

      rv$i <- i
      show_step(i)
    })
  })
}
