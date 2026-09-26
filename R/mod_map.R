# Landing page: satellite map + floating toolbox (basemap, layers, colour by, time animation) + camera placement drawer.

COLOR_BY <- c("source", "species", "class", "family", "trail", "moon", "hour", "year", "date", "prec")
OTHER_COL <- "#B8B3A3"
RAMP <- grDevices::colorRampPalette(c("#FBEFC5", "#E9B44C", "#C4502F", "#7A2618"))  # warm sequential, reads on satellite imagery
HOUR_COL <- c(night = "#2B3A67", twilight = "#D9822B", day = "#E8C547")

map_ui <- function() {
  sw <- function(id, key, tipkey, value = TRUE, icon = NULL, col = NULL) {
    ico <- if (!is.null(icon)) htmltools::span(class = "sw-ico", html_icon(icon, col))
    tip(bslib::input_switch(id, htmltools::tagList(ico, tt(key)), value), tipkey, "right")
  }
  src_sw <- function(s) sw(paste0("lyr_", s), paste0("src_", s), paste0("tip_lyr_", s), icon = SOURCE_ICON[[s]], col = SOURCE_COL[[s]])
  htmltools::div(
    class = "map-wrap",
    leaflet::leafletOutput("map", height = "100%"),
    htmltools::div(
      class = "glass toolbox",
      bslib::accordion(
        id = "toolbox", open = c("layers", "time"), multiple = TRUE,
        bslib::accordion_panel(
          tt("layers"), value = "layers", icon = bsicons::bs_icon("layers"),
          tip(shiny::radioButtons("basemap", NULL, inline = TRUE, choiceNames = list(tt("base_sat"), tt("base_topo")),
                                  choiceValues = c("sat", "topo")), "tip_basemap", "right"),
          src_sw("photo"), src_sw("monkey"), src_sw("herp"), src_sw("track"),
          htmltools::hr(),
          sw("lyr_trails", "trails", "tip_lyr_trails"),
          sw("lyr_bal_names", "balise_names", "tip_lyr_bal_names", value = FALSE),
          sw("lyr_balises", "balises", "tip_lyr_balises", value = FALSE),
          sw("lyr_heat", "heatmap", "tip_lyr_heat", value = FALSE),
          htmltools::hr(),
          tip(shiny::selectInput("color_by", tt("color_by"), choices = stats::setNames(COLOR_BY, vapply(paste0("cb_", COLOR_BY), tr, "")),
                                 selectize = FALSE), "tip_color_by", "right")
        ),
        bslib::accordion_panel(
          tt("time_anim"), value = "time", icon = bsicons::bs_icon("play-circle"),
          sw("anim_on", "anim_enable", "tip_anim_enable", value = FALSE),
          shiny::conditionalPanel(
            "input.anim_on",
            shiny::sliderInput("anim_t", NULL, min = as.Date("2011-01-01"), max = Sys.Date(), value = as.Date("2011-01-01"),
                               step = 31, ticks = FALSE, animate = shiny::animationOptions(interval = 300)),
            tip(shiny::radioButtons("anim_mode", NULL, inline = TRUE,
                                    choiceNames = list(tt("anim_cumulative"), tt("anim_month")),
                                    choiceValues = c("cum", "month")), "tip_anim_mode")
          )
        ),
        bslib::accordion_panel(
          tt("cameras"), value = "cams", icon = bsicons::bs_icon("camera"),
          htmltools::p(class = "small text-muted", tt("cams_help_short")),
          tip(htmltools::tags$button(class = "btn btn-primary btn-sm w-100", `data-toggle-drawer` = "1",
                                     bsicons::bs_icon("geo-alt"), tt("place_cameras")), "tip_place_cameras")
        )
      )
    ),
    shiny::uiOutput("map_legend"),
    htmltools::div(class = "glass map-count", shiny::uiOutput("map_count", inline = TRUE)),
    shiny::uiOutput("placing_banner"),
    htmltools::div(
      class = "glass drawer",
      htmltools::div(class = "drawer-head",
                     htmltools::h6(bsicons::bs_icon("camera"), tt("camera_stations")),
                     tip(htmltools::tags$button(class = "btn-close", `data-toggle-drawer` = "1"), "tip_close")),
      htmltools::p(class = "small text-muted", tt("cams_help")),
      shiny::uiOutput("cam_list")
    )
  )
}

popup_html <- function(o, lang) {
  img <- sp_img(o$binomial)
  # a handful of distinct pills / icons: build each once and look them up (htmltools per record took ~9 s for 13k points)
  pills <- vapply(names(IUCN_COL), function(k) as.character(iucn_pill(k, lang)), "")
  pill <- unname(pills[o$iucn %||% rep(NA, nrow(o))]); pill[is.na(pill)] <- ""
  icons <- vapply(SOURCE_ICON, function(i) as.character(html_icon(i)), "")
  sprintf("<div class='pop'>%s<div><div class='pop-src' style='--c:%s'>%s %s</div><b><i>%s</i></b> %s<br>%s<div class='pop-meta'>%s · %s<br>%s: %s</div></div></div>",
          ifelse(is.na(img), "", sprintf("<img class='pop-img' src='%s'>", img)),
          SOURCE_COL[o$source], unname(icons[o$source]),
          trv(o$source, "src", lang),
          ifelse(is.na(o$binomial), "?", o$binomial), pill, sp_common(o$binomial, o$common_es, lang),
          format(o$date, "%d/%m/%Y"), ifelse(is.na(o$trail), ifelse(is.na(o$station), "", o$station), o$trail),
          tr("location", lang), trv(ifelse(is.na(o$prec), "none", o$prec), "prec", lang))
}

hour_period <- function(h) ifelse(is.na(h), NA, ifelse(h < 5 | h >= 19, "night", ifelse(h < 6 | h >= 18, "twilight", "day")))

# Colour for each record + legend description, for the "colour by" choice
color_scheme <- function(o, by, lang) {
  cat_top <- function(v, labels = v, n = 7, pal = PAL) {
    top <- names(utils::head(sort(table(v), decreasing = TRUE), n))
    col <- ifelse(v %in% top, pal[match(v, top)], OTHER_COL)
    lab <- labels[match(top, v)]
    list(col = col, legend = data.frame(label = c(lab, if (any(!v %in% top)) tr("other", lang)),
                                        col = c(pal[seq_along(top)], if (any(!v %in% top)) OTHER_COL), italic = FALSE))
  }
  switch(by,
    source = list(col = unname(SOURCE_COL[o$source]),
                  legend = data.frame(label = trv(names(SOURCE_COL), "src", lang), col = unname(SOURCE_COL), italic = FALSE,
                                      icon = unname(SOURCE_ICON))),
    species = { r <- cat_top(o$binomial); r$legend$italic <- TRUE; r },
    class = cat_top(o$class, trv(o$class, "cls", lang), 3),
    family = cat_top(o$family),
    trail = list(col = ifelse(is.na(TRAIL_COL[o$trail]), OTHER_COL, unname(TRAIL_COL[o$trail])),
                 legend = data.frame(label = c(names(TRAIL_COL)[names(TRAIL_COL) %in% o$trail], tr("no_trail", lang)),
                                     col = c(unname(TRAIL_COL)[names(TRAIL_COL) %in% o$trail], OTHER_COL), italic = FALSE)),
    moon = { lit <- (1 - cos(2 * pi * (seq_along(MOON_ES) - 1) / 8)) / 2
             pc <- grDevices::colorRampPalette(c("#34453A", "#8FA88A", "#F1E3AE"))(101)[round(lit * 100) + 1]
             list(col = ifelse(is.na(o$moon), OTHER_COL, pc[as.integer(o$moon)]),
                  legend = data.frame(label = trv(MOON_ES, "moon", lang), col = pc, italic = FALSE, img = MOON_URI)) },
    hour = { p <- hour_period(o$hour)
             list(col = ifelse(is.na(p), OTHER_COL, unname(HOUR_COL[p])),
                  legend = data.frame(label = c(tr("p_night", lang), tr("p_twilight", lang), tr("p_day", lang), tr("no_time", lang)),
                                      col = c(unname(HOUR_COL), OTHER_COL), italic = FALSE)) },
    year = , date = {
      v <- if (by == "year") o$year else as.numeric(o$date)
      r <- range(v, na.rm = TRUE); pal <- RAMP(100)
      k <- pmin(100, pmax(1, round(99 * (v - r[1]) / max(1, diff(r))) + 1))
      lab <- if (by == "year") r else format(as.Date(r, origin = "1970-01-01"), "%m/%Y")
      list(col = ifelse(is.na(v), OTHER_COL, pal[k]), ramp = list(from = lab[1], to = lab[2], cols = pal[c(1, 33, 66, 100)])) },
    prec = cat_top(o$prec, trv(o$prec, "prec", lang), 6)
  )
}

map_server <- function(input, output, session, obs, db, stations, lang) {
  output$map <- leaflet::renderLeaflet({
    b <- db()$balises
    leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE, zoomControl = FALSE)) |>
      leaflet::addProviderTiles("Esri.WorldImagery", group = "sat") |>
      leaflet::addProviderTiles("Esri.WorldTopoMap", group = "topo") |>
      leaflet::hideGroup(if (identical(shiny::isolate(input$basemap), "topo")) "sat" else "topo") |>
      htmlwidgets::onRender("function(el, x){ L.control.zoom({position:'bottomright'}).addTo(this); }") |>
      leaflet::addScaleBar(position = "bottomright", options = leaflet::scaleBarOptions(imperial = FALSE)) |>
      leaflet::fitBounds(min(b$lon, na.rm = TRUE), min(b$lat, na.rm = TRUE), max(b$lon, na.rm = TRUE), max(b$lat, na.rm = TRUE))
  })
  shiny::observeEvent(input$basemap, {
    other <- setdiff(c("sat", "topo"), input$basemap)
    leaflet::leafletProxy("map") |> leaflet::showGroup(input$basemap) |> leaflet::hideGroup(other)
  }, ignoreInit = TRUE)

  # Colour-by labels follow the language
  shiny::observeEvent(lang(), {
    shiny::updateSelectInput(session, "color_by", choices = stats::setNames(COLOR_BY, vapply(paste0("cb_", COLOR_BY), tr, "", lang = lang())),
                             selected = shiny::isolate(input$color_by))
  }, ignoreInit = TRUE)

  # Trails and balises
  shiny::observe({
    b <- db()$balises
    p <- leaflet::leafletProxy("map") |> leaflet::clearGroup("trails") |> leaflet::clearGroup("balises") |> leaflet::clearGroup("bal_names")
    if (isTRUE(input$lyr_bal_names))
      p <- leaflet::addCircleMarkers(p, b$lon, b$lat, radius = 2, stroke = FALSE, fillColor = "#FFFFFF", fillOpacity = 0.9,
                                     group = "bal_names", label = b$code,
                                     labelOptions = leaflet::labelOptions(permanent = TRUE, direction = "top", offset = c(0, -2),
                                                                          className = "bal-label"))
    if (isTRUE(input$lyr_trails)) {
      for (ln in trail_lines(b)) {
        col <- unname(TRAIL_COL[ln$trail[1]]) %||% "#FFFFFF"
        p <- leaflet::addPolylines(p, ln$lon, ln$lat, color = col, weight = 3, opacity = 0.9, group = "trails",
                                   label = ln$trail[1], highlightOptions = leaflet::highlightOptions(weight = 6, bringToFront = TRUE))
      }
    }
    if (isTRUE(input$lyr_balises))
      p <- leaflet::addCircleMarkers(p, b$lon, b$lat, radius = 3, color = "#FFFFFF", weight = 1, fillColor = unname(TRAIL_COL[b$trail]),
                                     fillOpacity = 1, group = "balises",
                                     label = sprintf("%s · %s%s", b$code, b$trail, ifelse(is.na(b$alt), "", paste0(" · ", b$alt, " m"))))
  })

  # Observations shown on the map = global filters + layer switches + animation time
  map_obs <- shiny::reactive({
    o <- obs()
    keep <- c(photo = isTRUE(input$lyr_photo), monkey = isTRUE(input$lyr_monkey), herp = isTRUE(input$lyr_herp), track = isTRUE(input$lyr_track))
    o <- o[o$source %in% names(keep)[keep] & !is.na(o$lat), ]
    if (isTRUE(input$anim_on) && !is.null(input$anim_t)) {
      t <- as.Date(format(input$anim_t, "%Y-%m-01"))
      o <- if (identical(input$anim_mode, "month")) o[!is.na(o$ym) & o$ym == t, ] else o[!is.na(o$ym) & o$ym <= t, ]
    }
    o
  })
  # Colours are computed on the unanimated selection so they stay fixed while the timeline plays
  scheme <- shiny::reactive({
    o <- obs(); o <- o[!is.na(o$lat), ]
    list(o = o, s = color_scheme(o, input$color_by %||% "source", lang()))
  })
  rec_col <- function(o) { sc <- scheme(); sc$s$col[match(o$rid, sc$o$rid)] }

  shiny::observe({
    o <- map_obs(); l <- lang(); by <- input$color_by %||% "source"
    p <- leaflet::leafletProxy("map") |> leaflet::clearGroup("obs") |> leaflet::clearGroup("heat")
    if (!nrow(o)) return()
    if (isTRUE(input$lyr_heat)) {
      leaflet.extras::addHeatmap(p, o$lon, o$lat, intensity = 1, radius = 15, blur = 18, max = 0.6, minOpacity = 0.25, group = "heat",
                                 gradient = HEAT_GRADIENT)
      return()
    }
    o$col <- rec_col(o)
    # Camera photos: one bubble per station (x colour category when colouring by something else), sized by photos
    ph <- o[o$source == "photo", ]
    if (nrow(ph)) {
      grp <- if (by == "source") ph$station else paste(ph$station, ph$col)
      s <- do.call(rbind, lapply(split(ph, grp), function(d) {
        top <- utils::head(sort(table(d$binomial), decreasing = TRUE), 5)
        data.frame(station = d$station[1], lat = d$lat[1], lon = d$lon[1], col = d$col[1], n = nrow(d),
                   sp = length(unique(stats::na.omit(d$binomial))),
                   top = paste(sprintf("<li><i>%s</i> <span>%d</span></li>", names(top), as.integer(top)), collapse = ""))
      }))
      # fan out same-station bubbles so each category stays visible
      k <- stats::ave(seq_len(nrow(s)), s$station, FUN = seq_along) - 1
      s$lat <- s$lat + ifelse(k > 0, 0.00018 * sin(k * 2.4), 0); s$lon <- s$lon + ifelse(k > 0, 0.00018 * cos(k * 2.4), 0)
      p <- leaflet::addCircleMarkers(p, s$lon, s$lat, radius = 6 + 3 * sqrt(s$n / 10), color = "#FFFFFF", weight = 2,
                                     fillColor = s$col, fillOpacity = 0.9, group = "obs",
                                     label = sprintf("%s · %d %s", s$station, s$n, tr("photos", l)),
                                     popup = sprintf("<div class='pop'><div><div class='pop-src' style='--c:%s'>%s %s</div><b>%s</b> · %d %s · %d %s<ul class='pop-top'>%s</ul></div></div>",
                                                     SOURCE_COL[["photo"]], as.character(html_icon("camera")), tr("src_photo", l), s$station,
                                                     s$n, tr("photos", l), s$sp, tr("species_n", l), s$top))
    }
    d <- o[o$source != "photo", ]
    if (nrow(d)) {
      # shape cue besides colour: tracks = hollow ring, direct observations = filled dot
      tr_ <- d$source == "track"
      # popups are built on click (map_marker_click): sending 13k popup HTML strings made the first load ~10 s slower
      p <- leaflet::addCircleMarkers(p, d$lon, d$lat, radius = ifelse(tr_, 4.5, 4), stroke = TRUE, weight = ifelse(tr_, 1.8, 0.8),
                                     color = ifelse(tr_, d$col, "#FFFFFF"), fillColor = d$col, fillOpacity = ifelse(tr_, 0.15, 0.9),
                                     group = "obs", layerId = paste0("r", d$rid), label = ifelse(is.na(d$binomial), "?", d$binomial))
    }
  })

  shiny::observeEvent(input$map_marker_click, {
    id <- input$map_marker_click$id
    if (is.null(id) || !startsWith(id, "r")) return()
    o <- obs(); r <- o[o$rid == as.integer(sub("^r", "", id)), ]
    if (!nrow(r)) return()
    leaflet::leafletProxy("map") |> leaflet::clearPopups() |> leaflet::addPopups(r$lon, r$lat, popup_html(r, lang()))
  })

  output$map_legend <- shiny::renderUI({
    s <- scheme()$s; l <- lang(); by <- input$color_by %||% "source"
    if (isTRUE(input$lyr_heat)) return(NULL)
    body <- if (!is.null(s$ramp)) htmltools::div(class = "leg-ramp",
                htmltools::span(s$ramp$from), htmltools::span(class = "ramp", style = sprintf("background:linear-gradient(90deg,%s)", paste(s$ramp$cols, collapse = ","))),
                htmltools::span(s$ramp$to)) else
      lapply(seq_len(nrow(s$legend)), function(i) {
        lg <- s$legend[i, ]
        sw <- if (!is.null(lg$img) && !is.na(lg$img)) htmltools::tags$img(src = lg$img, class = "leg-moon") else
          if (!is.null(lg$icon) && !is.na(lg$icon)) htmltools::span(class = "leg-ico", html_icon(lg$icon, lg$col)) else
            htmltools::span(class = "legend-dot", style = paste0("background:", lg$col))
        htmltools::div(class = "leg-row", sw, htmltools::span(class = if (isTRUE(lg$italic)) "fst-italic", lg$label))
      })
    htmltools::div(class = "glass map-legend", htmltools::div(class = "leg-title", tr(paste0("cb_", by), l)), body)
  })

  output$map_count <- shiny::renderUI({
    o <- map_obs(); l <- lang()
    t <- if (isTRUE(input$anim_on) && !is.null(input$anim_t)) htmltools::span(class = "count-date", paste(tr(sprintf("m%02d", as.integer(format(input$anim_t, "%m"))), l), format(input$anim_t, "%Y")))
    # photos whose camera has no position yet are invisible: say so and offer the fix
    hidden <- if (isTRUE(input$lyr_photo)) sum(obs()$source == "photo" & is.na(obs()$lat)) else 0
    hint <- if (hidden > 0) htmltools::tags$a(class = "count-hint", href = "#", `data-toggle-drawer` = "1", onclick = "return false;",
                                              bsicons::bs_icon("camera"), sprintf(tr("photos_hidden", l), format(hidden, big.mark = " ")))
    htmltools::tagList(t, htmltools::tags$b(format(nrow(o), big.mark = " ")), tr("records", l), " · ",
                       htmltools::tags$b(length(unique(stats::na.omit(o$binomial)))), tr("species_n", l), hint)
  })

  # Animation slider follows the data's date range
  shiny::observe({
    r <- range(obs()$ym, na.rm = TRUE)
    if (all(is.finite(r))) shiny::updateSliderInput(session, "anim_t", min = r[1], max = r[2], value = r[1])
  })

  # ---- camera placement --------------------------------------------------------------
  placing <- shiny::reactiveVal(NULL)

  cam_counts <- shiny::reactive({
    tb <- sort(table(db()$photos$camera), decreasing = TRUE)
    data.frame(camera_id = names(tb), n = as.integer(tb))
  })

  output$cam_list <- shiny::renderUI({
    cc <- cam_counts(); st <- stations(); l <- lang(); pl <- placing()
    rows <- lapply(seq_len(nrow(cc)), function(i) {
      id <- cc$camera_id[i]; placed <- id %in% st$camera_id
      js <- sprintf("Shiny.setInputValue('%s', %s, {priority:'event'})", "%s", jsonlite::toJSON(id, auto_unbox = TRUE))
      htmltools::div(
        class = paste("cam-row", if (identical(pl, id)) "active"),
        htmltools::span(class = paste("cam-dot", if (placed) "on"), title = tr(if (placed) "placed" else "unplaced", l)),
        htmltools::span(class = "cam-name", id),
        htmltools::span(class = "cam-n", cc$n[i]),
        tip(htmltools::tags$button(class = "btn btn-icon", onclick = sprintf(js, "cam_place"), bsicons::bs_icon("crosshair")),
            if (placed) "tip_cam_move" else "tip_cam_place"),
        if (placed) bslib::popover(
          # native title: a bslib tooltip nested in a popover trigger swallows the click
          htmltools::tags$button(class = "btn btn-icon danger", title = tr("tip_cam_delete", l), bsicons::bs_icon("trash3")),
          htmltools::p(class = "mb-2 small", tt("confirm_delete")),
          htmltools::tags$button(class = "btn btn-danger btn-sm", onclick = sprintf(js, "cam_delete"), tt("delete")),
          title = id
        )
      )
    })
    htmltools::div(class = "cam-list", rows)
  })

  shiny::observeEvent(input$cam_place, {
    placing(input$cam_place)
    session$sendCustomMessage("placing", TRUE)
  })
  output$placing_banner <- shiny::renderUI({
    id <- placing(); if (is.null(id)) return(NULL)
    htmltools::div(class = "glass placing-banner", bsicons::bs_icon("crosshair"),
                   htmltools::span(sprintf(tr("click_to_place", lang()), id)),
                   htmltools::tags$button(class = "btn btn-sm btn-outline-secondary",
                                          onclick = "Shiny.setInputValue('cam_cancel', Date.now())", tt("cancel")))
  })
  shiny::observeEvent(input$cam_cancel, { placing(NULL); session$sendCustomMessage("placing", FALSE) })

  save_stations <- function(st) {
    ok <- tryCatch({ write_stations(st); TRUE }, error = function(e) {
      shiny::showNotification(paste(tr("save_failed", lang()), conditionMessage(e)), type = "error", duration = 8); FALSE })
    if (ok) stations(st)
  }

  shiny::observeEvent(input$map_click, {
    id <- placing(); if (is.null(id)) return()
    st <- stations()
    st <- st[st$camera_id != id, ]
    st <- rbind(st, data.frame(camera_id = id, lat = round(input$map_click$lat, 6), lon = round(input$map_click$lng, 6),
                               set_at = format(Sys.time(), "%Y-%m-%d %H:%M")))
    save_stations(st)
    placing(NULL); session$sendCustomMessage("placing", FALSE)
    shiny::showNotification(sprintf(tr("cam_saved", lang()), id), type = "message", duration = 3)
  })

  shiny::observeEvent(input$cam_delete, {
    save_stations(stations()[stations()$camera_id != input$cam_delete, ])
    shiny::showNotification(sprintf(tr("cam_deleted", lang()), input$cam_delete), duration = 3)
  })

  # Station pins (visible while the photos layer is on)
  shiny::observe({
    st <- stations()
    p <- leaflet::leafletProxy("map") |> leaflet::clearGroup("stations")
    if (nrow(st) && isTRUE(input$lyr_photo))
      leaflet::addCircleMarkers(p, st$lon, st$lat, radius = 3, color = COL$green_dk, weight = 1.5, fillColor = "#FFFFFF",
                                fillOpacity = 1, group = "stations", label = st$camera_id,
                                labelOptions = leaflet::labelOptions(permanent = TRUE, direction = "right", className = "cam-label"))
  })
}
