# RioBigal Explorer — wildlife monitoring dashboard for the Reserva Biológica del Río Bigal.
# Files in R/ are sourced automatically by Shiny. Run locally: shiny::runApp()
library(shiny)
library(bslib)
# The server function defines many closures; byte-compiling them for every new session cost ~2.5 s per visitor
invisible(compiler::enableJIT(0))

DB0 <- load_db()  # parsed once per process; sessions share it until someone hits "refresh"

# Taxonomy/date columns every source table gets so the same filters apply everywhere
prep_src <- function(d, tax, class = "Mamífero", fam = NULL) {
  d$genus <- sub(" .*", "", d$binomial)
  if (is.null(d$family)) d$family <- tax$family[match(d$genus, tax$genus)]
  if (is.null(d$class)) d$class <- class
  d$order <- tax$order[match(d$genus, tax$genus)]
  d$iucn <- if (!is.null(SPECIES_MEDIA$iucn)) SPECIES_MEDIA$iucn[match(d$binomial, SPECIES_MEDIA$binomial)] else NA
  if (!is.null(fam)) d$order <- ifelse(is.na(d$order), fam$order[match(d$family, fam$family)], d$order)
  d$year <- as.integer(format(d$date, "%Y"))
  d$month <- as.integer(format(d$date, "%m"))
  d$ym <- as.Date(format(d$date, "%Y-%m-01"))
  d
}

month_choices <- function(lang) stats::setNames(1:12, vapply(sprintf("m%02d", 1:12), tr, "", lang = lang))

ui <- page_navbar(
  id = "nav", window_title = "RioBigal Explorer", theme = bigal_theme(), fillable = "map", lang = "es",
  title = tags$span(class = "brand", bsicons::bs_icon("feather"), tags$span(HTML("<span class='brand-rio'>Rio</span>Bigal"), tags$b("Explorer"))),
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"), tags$script(src = "app.js"), tags$script(src = "charts.js"),
    # PDF export (client side snapshot of the current page)
    tags$script(src = "https://cdn.jsdelivr.net/npm/html-to-image@1.11.11/dist/html-to-image.js", defer = NA),
    tags$script(src = "https://cdn.jsdelivr.net/npm/jspdf@2.5.1/dist/jspdf.umd.min.js", defer = NA),
    tags$link(rel = "icon", href = "data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16'><circle cx='8' cy='8' r='7' fill='%231E4D2B'/></svg>")
  ),
  sidebar = sidebar(
    id = "sb", width = 290, open = "desktop", class = "filters",
    title = tags$div(class = "sb-title", bsicons::bs_icon("funnel"), tt("filters")),
    tip(sliderInput("f_years", tt("years"), min = 2007, max = 2025, value = c(2007, 2025), sep = "", ticks = FALSE), "tip_f_years"),
    tip(selectizeInput("f_class", tt("class"), choices = NULL, multiple = TRUE, options = list(placeholder = "—")), "tip_f_class"),
    tip(selectizeInput("f_family", tt("family"), choices = NULL, multiple = TRUE, options = list(placeholder = "—")), "tip_f_family"),
    tip(selectizeInput("f_species", tt("species"), choices = NULL, multiple = TRUE,
                       options = list(placeholder = tr("search_species"), openOnFocus = FALSE, maxOptions = 30)), "tip_f_species"),
    tip(selectizeInput("f_months", tt("months"), choices = month_choices("es"), multiple = TRUE, options = list(placeholder = "—")), "tip_f_months"),
    tip(input_switch("f_threat", tagList(tags$span(class = "iucn-dot"), tt("threatened_only")), FALSE), "tip_f_threat"),
    tip(actionButton("f_reset", tagList(bsicons::bs_icon("arrow-counterclockwise"), tt("reset_filters")), class = "btn-reset w-100"), "tip_f_reset"),
    tags$hr(),
    uiOutput("filter_summary")
  ),
  nav_panel(tt("nav_map"), value = "map", icon = bsicons::bs_icon("globe-americas"), map_ui()),
  nav_panel(tt("nav_overview"), value = "overview", icon = bsicons::bs_icon("grid-1x2"), stats_ui$panorama()),
  nav_panel(tt("nav_species"), value = "species", icon = bsicons::bs_icon("search-heart"), stats_ui$species()),
  nav_panel(tt("nav_cameras"), value = "cameras", icon = bsicons::bs_icon("camera"), stats_ui$cameras()),
  nav_panel(tt("nav_primates"), value = "primates", icon = bsicons::bs_icon("tree"), stats_ui$primates()),
  nav_panel(tt("nav_herps"), value = "herps", icon = bsicons::bs_icon("droplet"), stats_ui$herps()),
  nav_panel(tt("nav_tracks"), value = "tracks", icon = bsicons::bs_icon("signpost-split"), stats_ui$tracks()),
  nav_panel(tt("nav_quality"), value = "quality", icon = bsicons::bs_icon("clipboard-check"), stats_ui$quality()),
  nav_spacer(),
  nav_item(tip(radioButtons("names", NULL, inline = TRUE, choiceNames = list(tt("names_latin"), tt("names_common")),
                            choiceValues = c("latin", "common")), "tip_names", "bottom")),
  nav_item(tip(tags$button(id = "pdf_export", type = "button", class = "btn-pdf", bsicons::bs_icon("arrow-down"), tags$span("PDF")),
               "tip_pdf", "bottom")),
  nav_item(tip(radioButtons("lang", NULL, c(ES = "es", EN = "en"), inline = TRUE), "tip_lang", "bottom")),
  nav_item(tip(actionButton("refresh", NULL, icon = bsicons::bs_icon("arrow-repeat"), class = "btn-nav"), "tip_refresh", "bottom")),
  nav_item(uiOutput("synced", inline = TRUE))
)

server <- function(input, output, session) {
  lang <- reactive(input$lang %||% "es")
  # Deep links: .../#species opens that page
  observe({
    h <- sub("^#", "", session$clientData$url_hash)
    if (nzchar(h)) nav_select("nav", h)
  })
  # ...?lang=en opens the app in English
  observeEvent(session$clientData$url_search, {
    q <- parseQueryString(session$clientData$url_search)
    if (isTRUE(q$lang %in% c("es", "en"))) updateRadioButtons(session, "lang", selected = q$lang)
  }, once = TRUE)
  observeEvent(lang(), {
    set_language(session, lang())
    updateSelectizeInput(session, "f_months", choices = month_choices(lang()), selected = isolate(input$f_months))
  })

  db <- reactiveVal(DB0)
  stations <- reactiveVal(tryCatch(read_stations(), error = function(e) {
    showNotification(conditionMessage(e), type = "error"); data.frame(camera_id = character(), lat = numeric(), lon = numeric(), set_at = character())
  }))

  observeEvent(input$refresh, {
    id <- showNotification(tr("syncing", lang()), duration = NULL, type = "message")
    new <- tryCatch(load_db(prev = db()), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    removeNotification(id)
    if (is.null(new)) return()
    DB0 <<- new; db(new)
    try(stations(read_stations()))
    if (length(new$errors)) showNotification(tagList(tags$b(tr("load_errors", lang())), tags$br(), paste(new$errors, collapse = "; ")),
                                             type = "warning", duration = 12)
    else showNotification(tr("synced_ok", lang()), type = "message", duration = 3)
  })
  output$synced <- renderUI(tags$span(class = "synced", title = tr("last_sync", lang()), bsicons::bs_icon("cloud-check"),
                                      format(db()$synced, "%d/%m %H:%M")))

  # identical for every visitor until the data or the camera stations change: cached app-wide (bindCache)
  obs_all <- reactive(build_obs(db(), stations())) |> bindCache(db()$synced, stations())
  src_all <- reactive({
    d <- db(); tax <- d$tax
    list(photos = prep_src(d$photos, tax), monkeys = prep_src(d$monkeys, tax),
         herps = prep_src(d$herps, tax, class = NULL, fam = d$fam), tracks = prep_src(d$tracks, tax))
  }) |> bindCache(db()$synced)

  # Species display names: Latin (identity) or common name in the current language. binomial stays the key everywhere.
  spn <- reactive({
    if (!identical(input$names, "common")) return(identity)
    o <- obs_all(); b <- sort(unique(stats::na.omit(o$binomial)))
    k <- !is.na(o$common_es)
    lab <- sp_common(b, o$common_es[k][match(b, o$binomial[k])], lang())
    lab[!nzchar(lab)] <- b[!nzchar(lab)]  # no common name: keep the Latin one
    d <- lab %in% lab[duplicated(lab)]; lab[d] <- sprintf("%s (%s)", lab[d], b[d])  # labels must stay unique (axes, factor levels)
    tab <- stats::setNames(lab, b)
    function(x) { v <- unname(tab[as.character(x)]); ifelse(is.na(v), as.character(x), v) }
  })

  # ---- global filters ----
  observe({
    o <- obs_all(); y <- range(o$year, na.rm = TRUE)
    updateSliderInput(session, "f_years", min = y[1], max = y[2], value = y)
    cl <- sort(unique(stats::na.omit(o$class)))
    updateSelectizeInput(session, "f_class", choices = stats::setNames(cl, trv(cl, "cls", lang())), selected = isolate(input$f_class))
  })
  observe({
    o <- obs_all(); if (length(input$f_class)) o <- o[o$class %in% input$f_class, ]
    updateSelectizeInput(session, "f_family", choices = sort(unique(stats::na.omit(o$family))), selected = isolate(input$f_family))
  })
  observe({
    o <- obs_all()
    if (length(input$f_class)) o <- o[o$class %in% input$f_class, ]
    if (length(input$f_family)) o <- o[o$family %in% input$f_family, ]
    b <- setdiff(sort(unique(stats::na.omit(o$binomial))), "Homo sapiens"); nm <- spn()
    # the label carries both names so typing either one finds the species
    k <- !is.na(o$common_es)
    other <- if (identical(input$names, "common")) b else sp_common(b, o$common_es[k][match(b, o$binomial[k])], lang())
    lab <- ifelse(nzchar(other) & other != nm(b), paste(nm(b), other, sep = " · "), nm(b))
    updateSelectizeInput(session, "f_species", choices = stats::setNames(b, lab), selected = isolate(input$f_species), server = TRUE,
                         options = list(placeholder = tr("search_species", lang()), openOnFocus = FALSE, maxOptions = 30))
  })
  observeEvent(input$f_reset, {
    y <- range(obs_all()$year, na.rm = TRUE)
    updateSliderInput(session, "f_years", value = y)
    for (id in c("f_class", "f_family", "f_species", "f_months")) updateSelectizeInput(session, id, selected = character())
    update_switch("f_threat", value = FALSE)
  })
  # Sunburst click on the overview sets the family / species filter
  observeEvent(input$sunburst_click, {
    o <- obs_all(); k <- input$sunburst_click
    if (k %in% o$binomial) updateSelectizeInput(session, "f_species", selected = k)
    else if (k %in% o$family) updateSelectizeInput(session, "f_family", selected = k)
    else if (k %in% o$class) updateSelectizeInput(session, "f_class", selected = k)
  })

  apply_filters <- function(d) {
    keep <- rep(TRUE, nrow(d))
    y <- input$f_years
    if (length(y) == 2) keep <- keep & (is.na(d$year) | (d$year >= y[1] & d$year <= y[2]))
    if (length(input$f_class)) keep <- keep & d$class %in% input$f_class
    if (length(input$f_family)) keep <- keep & d$family %in% input$f_family
    if (length(input$f_species)) keep <- keep & d$binomial %in% input$f_species
    if (length(input$f_months)) keep <- keep & d$month %in% as.integer(input$f_months)
    keep <- keep & !d$binomial %in% "Homo sapiens"  # people in camera photos are never shown
    if (isTRUE(input$f_threat)) keep <- keep & d$iucn %in% IUCN_THREAT
    d[keep, ]
  }
  obs <- reactive(apply_filters(obs_all())) |> debounce(300)
  src <- reactive(lapply(src_all(), apply_filters)) |> debounce(300)

  output$filter_summary <- renderUI({
    o <- obs(); l <- lang()
    tags$div(class = "summary",
      lapply(names(SOURCE_COL), function(s) tags$div(class = "sum-row",
        tags$span(class = "sum-ico", html_icon(SOURCE_ICON[[s]], SOURCE_COL[[s]])),
        tags$span(tr(paste0("src_", s), l)), tags$b(format(sum(o$source == s), big.mark = " ")))),
      tags$div(class = "sum-row total", tags$span(tr("species_n", l)), tags$b(length(unique(stats::na.omit(o$binomial))))))
  })

  map_server(input, output, session, obs, db, stations, lang, spn)
  stats_server(input, output, session, obs, src, db, stations, lang, spn)
}

shinyApp(ui, server)
