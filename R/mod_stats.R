# Statistics pages. Every chart reads the globally filtered data (sidebar) and the current language.

# head: inline controls placed right after the title (dropdowns, toggles, isolation chips)
chart_card <- function(id, key, info = NULL, height = "320px", ..., footer = NULL, head = NULL) {
  bslib::card(
    full_screen = TRUE, class = "chart-card",
    bslib::card_header(
      htmltools::span(class = "card-title", tt(key)),
      head,
      if (!is.null(info)) tip(htmltools::span(class = "info", bsicons::bs_icon("info-circle")), info)
    ),
    ...,
    bslib::card_body(echarts4r::echarts4rOutput(id, height = height), padding = c(4, 8, 4, 8)),
    if (!is.null(footer)) bslib::card_footer(footer)
  )
}
kpi <- function(key, id, icon, info) {
  bslib::value_box(title = tip(tt(key), info, "bottom"), value = shiny::textOutput(id, inline = TRUE),
                   showcase = bsicons::bs_icon(icon), showcase_layout = "left center", class = "kpi",
                   theme = bslib::value_box_theme(bg = COL$surface, fg = COL$ink))
}
page_head <- function(key, sub) htmltools::div(class = "page-head", htmltools::h2(tt(key)), htmltools::p(tt(sub)))
info_icon <- function(key) tip(htmltools::span(class = "info", bsicons::bs_icon("info-circle")), key)
# Segmented radio (styled like the basemap control)
seg <- function(id, keys, values, selected = values[1])
  htmltools::div(class = "seg", shiny::radioButtons(id, NULL, inline = TRUE, choiceNames = lapply(keys, tt), choiceValues = values,
                                                    selected = selected))
# "by: Species / Family / Order" dropdown for the herp charts (labels translated server-side)
MODALITIES <- c("binomial", "family", "order")
mod_select <- function(id, selected = "family")
  htmltools::div(class = "mod-select", shiny::selectInput(id, NULL, choices = stats::setNames(MODALITIES, vapply(paste0("mod_", MODALITIES), tr, "")),
                                                          selected = selected, width = "130px", selectize = FALSE))
iso_chip <- function(id) shiny::uiOutput(id, inline = TRUE)
clock_legend <- htmltools::div(class = "clock-legend",
  htmltools::span(htmltools::span(class = "cl-sw", style = "background:#2B3A67"), html_icon("moon"), tt("p_night")),
  htmltools::span(htmltools::span(class = "cl-sw", style = "background:#D9822B"), tt("p_twilight")),
  htmltools::span(htmltools::span(class = "cl-sw", style = "background:#E8C547"), html_icon("sun"), tt("p_day")))

stats_ui <- list(
  panorama = function() bslib::page_fillable(
    fillable = FALSE, class = "stats-page",
    page_head("nav_overview", "sub_overview"),
    bslib::layout_columns(
      col_widths = c(4, 8), fill = FALSE,
      bslib::layout_column_wrap(width = 1, fill = FALSE, class = "kpi-stack", gap = ".75rem",
        kpi("kpi_records", "k_records", "collection", "tip_kpi_records"),
        kpi("kpi_species", "k_species", "flower1", "tip_kpi_species"),
        kpi("kpi_events", "k_events", "camera", "tip_kpi_events"),
        kpi("kpi_threat", "k_threat", "shield-exclamation", "tip_kpi_threat")),
      bslib::card(class = "podium-card",
        bslib::card_header(htmltools::span(class = "card-title", tt("t_podium")), info_icon("tip_podium")),
        shiny::uiOutput("podium"))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      chart_card("c_accum", "t_accum", "tip_accum", height = "380px", footer = shiny::uiOutput("accum_note")),
      chart_card("c_sunburst", "t_sunburst", "tip_sunburst", height = "520px",
                 head = seg("taxo_mode", c("taxo_tree", "taxo_sunburst"), c("tree", "sunburst")))
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      chart_card("c_discovery", "t_discovery", "tip_discovery", height = "320px", footer = shiny::uiOutput("newcomers")),
      chart_card("c_trail_sim", "t_trail_sim", "tip_trail_sim", height = "400px")
    ),
    chart_card("c_elev", "t_elev", "tip_elev", height = "300px", head = shiny::uiOutput("elev_pick", inline = TRUE),
               footer = htmltools::span(class = "small text-muted", tt("elev_note"))),
    bslib::layout_columns(
      col_widths = c(6, 6),
      chart_card("c_years", "t_years", "tip_years"),
      chart_card("c_top", "t_top", "tip_top", height = "420px")
    )
  ),

  species = function() bslib::page_fillable(
    fillable = FALSE, class = "stats-page",
    htmltools::div(class = "sp-header",
      shiny::uiOutput("sp_id", class = "sp-id"),
      htmltools::div(class = "sp-right",
        htmltools::div(class = "sp-picker", tip(shiny::selectizeInput("sp", NULL, choices = NULL, width = "100%"), "tip_sp_pick")),
        shiny::uiOutput("sp_stats"))),
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(full_screen = TRUE, bslib::card_header(htmltools::span(class = "card-title", tt("t_sp_map")),
                                                         tip(htmltools::span(class = "info", bsicons::bs_icon("info-circle")), "tip_sp_map")),
                  leaflet::leafletOutput("sp_map", height = "340px")),
      chart_card("c_sp_years", "t_sp_years", "tip_years", height = "340px")
    ),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      chart_card("c_sp_clock", "t_clock", "tip_clock", footer = clock_legend),
      chart_card("c_sp_moon", "t_moon", "tip_moon"),
      bslib::card(full_screen = TRUE, class = "chart-card",
                  bslib::card_header(htmltools::span(class = "card-title", tt("t_weather")), info_icon("tip_weather")),
                  bslib::card_body(shiny::uiOutput("sp_weather")))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      chart_card("c_sp_site", "t_site", "tip_site", height = "380px"),
      bslib::card(bslib::card_header(htmltools::span(class = "card-title", tt("t_sp_where"))), reactable::reactableOutput("sp_where"))
    )
  ),

  cameras = function() bslib::page_fillable(
    fillable = FALSE, class = "stats-page",
    page_head("nav_cameras", "sub_cameras"),
    htmltools::div(class = "effort-warning", bsicons::bs_icon("exclamation-triangle"), tt("effort_warning")),
    chart_card("c_rai", "t_rai", "tip_rai", height = "440px"),
    chart_card("c_ov_matrix", "t_ov_matrix", "tip_ov_matrix", height = "420px"),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(htmltools::span(class = "card-title", tt("t_overlap")),
                         tip(htmltools::span(class = "info", bsicons::bs_icon("info-circle")), "tip_overlap")),
      bslib::layout_columns(
        col_widths = c(3, 9),
        htmltools::div(
          shiny::selectizeInput("ov_a", tt("species_a"), choices = NULL),
          shiny::selectizeInput("ov_b", tt("species_b"), choices = NULL),
          shiny::uiOutput("ov_stat")
        ),
        echarts4r::echarts4rOutput("c_overlap", height = "320px")
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      chart_card("c_cam_moon", "t_cam_moon", "tip_cam_moon", height = "400px"),
      chart_card("c_cam_site", "t_cam_site", "tip_cam_site", height = "400px")
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      chart_card("c_cam_age", "t_cam_age", "tip_cam_age", height = "340px"),
      chart_card("c_cam_temp", "t_cam_temp", "tip_cam_temp", height = "340px")
    ),
    chart_card("c_cam_effort", "t_cam_effort", "tip_cam_effort", height = "380px")
  ),

  primates = function() bslib::page_fillable(
    fillable = FALSE, class = "stats-page",
    page_head("nav_primates", "sub_primates"),
    bslib::layout_columns(col_widths = c(7, 5),
                          chart_card("c_pr_rate", "t_pr_rate", "tip_pr_rate"),
                          chart_card("c_pr_trail", "t_pr_trail", "tip_pr_trail")),
    bslib::layout_columns(col_widths = c(6, 6),
                          chart_card("c_pr_group", "t_pr_group", "tip_pr_group", height = "340px"),
                          chart_card("c_pr_height", "t_pr_height", "tip_pr_height", height = "340px")),
    bslib::layout_columns(col_widths = c(6, 6),
                          chart_card("c_pr_activity", "t_pr_activity", "tip_pr_activity"),
                          chart_card("c_pr_dist", "t_pr_dist", "tip_pr_dist",
                                     head = tip(bslib::input_switch("pr_dist_sp", tt("by_species"), FALSE), "tip_by_species")))
  ),

  herps = function() bslib::page_fillable(
    fillable = FALSE, class = "stats-page",
    page_head("nav_herps", "sub_herps"),
    bslib::layout_columns(col_widths = c(6, 6),
                          chart_card("c_he_rate", "t_he_rate", "tip_he_rate", footer = shiny::uiOutput("he_rate_note")),
                          chart_card("c_he_river", "t_he_river", "tip_he_river")),
    bslib::layout_columns(col_widths = c(7, 5),
                          chart_card("c_he_substrate", "t_he_substrate", "tip_he_substrate", height = "520px",
                                     head = htmltools::tagList(mod_select("he_mod_sub", "family"), iso_chip("he_iso_sub"))),
                          chart_card("c_he_snakes", "t_he_snakes", "tip_he_snakes", height = "520px")),
    bslib::layout_columns(col_widths = c(6, 6),
                          chart_card("c_he_height", "t_he_height", "tip_he_height", height = "380px",
                                     head = htmltools::tagList(mod_select("he_mod_h", "family"), iso_chip("he_iso_h"))),
                          chart_card("c_he_size", "t_he_size", "tip_he_size", height = "380px",
                                     head = htmltools::tagList(mod_select("he_mod_lt", "binomial"), iso_chip("he_iso_lt")))),
    bslib::layout_columns(col_widths = c(7, 5),
                          chart_card("c_he_sex", "t_he_sex", "tip_he_sex", height = "360px", head = shiny::uiOutput("he_sex_ratio", inline = TRUE),
                                     footer = htmltools::span(class = "small text-muted", tt("he_sex_note"))),
                          bslib::card(full_screen = TRUE, class = "chart-card",
                                      bslib::card_header(htmltools::span(class = "card-title", tt("t_he_recruit")), info_icon("tip_he_recruit")),
                                      bslib::card_body(shiny::uiOutput("he_recruit")))),
    bslib::card(full_screen = TRUE, class = "chart-card",
                bslib::card_header(htmltools::span(class = "card-title", tt("t_he_wx")), info_icon("tip_he_wx")),
                bslib::layout_columns(col_widths = c(8, 4),
                                      echarts4r::echarts4rOutput("c_he_wx", height = "360px"),
                                      shiny::uiOutput("he_wx_strip")),
                bslib::card_footer(shiny::uiOutput("he_wx_stats")))
  ),

  tracks = function() bslib::page_fillable(
    fillable = FALSE, class = "stats-page",
    page_head("nav_tracks", "sub_tracks"),
    bslib::layout_columns(col_widths = c(6, 6),
                          chart_card("c_tr_trail", "t_tr_trail", "tip_tr_trail", height = "400px"),
                          chart_card("c_tr_size", "t_tr_size", "tip_tr_size", height = "400px")),
    chart_card("c_tr_season", "t_tr_season", "tip_tr_season", height = "340px")
  ),

  quality = function() bslib::page_fillable(
    fillable = FALSE, class = "stats-page",
    page_head("nav_quality", "sub_quality"),
    shiny::uiOutput("load_errors"),
    shiny::uiOutput("q_boxes"),
    bslib::card(
      bslib::card_header(htmltools::span(class = "card-title", tt("t_issues")),
                         tip(shiny::downloadButton("q_download", tt("download_csv"), class = "btn-sm btn-outline-primary"), "tip_download")),
      reactable::reactableOutput("q_table")
    )
  )
)

# ---- helpers ---------------------------------------------------------------------------

count_by <- function(d, ...) as.data.frame(dplyr::count(d, ...))
top_species <- function(d, n = 12) names(utils::head(sort(table(d$binomial), decreasing = TRUE), n))
src_label <- function(src, l) trv(src, "src", l)
# group_by(src) emits series sorted by label; return each series' source colour in that order
src_colors <- function(f) { lv <- sort(unique(f$src)); unname(SOURCE_COL[f$source[match(lv, f$src)]]) }
# Legend entries drawn as the icon of each data source (camera, binoculars, frog, paw)
src_legend <- function(e, f) {
  lv <- sort(unique(f$src))
  e$x$opts$legend$data <- lapply(lv, function(s) list(name = s, icon = e_icon(SOURCE_ICON[[f$source[match(s, f$src)]]])))
  e$x$opts$legend$itemWidth <- 16; e$x$opts$legend$itemHeight <- 14
  e
}
empty_chart <- function(l) no_axes(echarts4r::e_charts() |> echarts4r::e_title(tr("no_data", l), left = "center", top = "middle",
                                                                        textStyle = list(color = COL$muted, fontWeight = 400, fontSize = 13)))
# e_grid() appends a new grid; this edits the one e_bigal() created
grid_set <- function(e, ...) { e$x$opts$grid[[1]] <- utils::modifyList(e$x$opts$grid[[1]], list(...)); e }
no_axes <- function(e) { e$x$opts$xAxis <- NULL; e$x$opts$yAxis <- NULL; e }
cat_axis <- function(e, rotate = 0) echarts4r::e_x_axis(e, axisLabel = list(rotate = rotate, interval = 0), axisTick = list(show = FALSE))
dashed_series <- function(e, idx) { for (i in idx) e$x$opts$series[[i]]$lineStyle <- list(type = "dashed", width = 2); e }
heat_colors <- c("#F2EEDF", "#B9D3AE", "#6FA36A", "#2F7D32", "#12301B")
italic_axis <- list(fontStyle = "italic", fontSize = 11, interval = 0)

# Chart from a raw echarts option list, with the house tooltip / text / export defaults
e_raw <- function(opts) {
  e <- echarts4r::e_charts()
  base <- list(color = as.list(PAL), textStyle = list(fontFamily = "Inter", color = COL$muted),
               tooltip = list(trigger = "item", backgroundColor = COL$surface, borderColor = COL$line,
                              textStyle = list(color = COL$ink, fontFamily = "Inter")),
               toolbox = list(feature = list(saveAsImage = list(title = "PNG"))))
  e$x$opts <- utils::modifyList(base, opts)
  e
}
js_array <- function(x) jsonlite::toJSON(unname(x), auto_unbox = FALSE)
# Pictogram series (bigal.pictogram): rows = list(c(category index, value, value per icon, background value))
pictogram_series <- function(rows, icon, color, name, horizontal = FALSE, offset = 0, size = 20, info = NULL) {
  cfg <- list(icon = icon_uri(icon, color), aspect = ICON_BOX[[icon]][3] / ICON_BOX[[icon]][4], horizontal = horizontal,
              offset = offset, size = size)
  list(type = "custom", name = name, renderItem = htmlwidgets::JS(sprintf("bigal.pictogram(%s)", jsonlite::toJSON(cfg, auto_unbox = TRUE))),
       data = lapply(rows, function(r) unname(as.numeric(r))),  # named vectors would serialise as JSON objects
       itemStyle = list(color = color),
       encode = if (horizontal) list(x = c(1, 3), y = 0) else list(x = 0, y = c(1, 3)),
       tooltip = if (!is.null(info)) list(formatter = htmlwidgets::JS(sprintf("function(p){ return %s[p.dataIndex]; }", js_array(info)))))
}
# Axis label formatter: category index -> "{iconK|}\nname" with rich-text icons
icon_axis <- function(names, icons, italic = FALSE, col = COL$green) {
  u <- unique(icons)
  rich <- stats::setNames(lapply(u, function(i) list(height = 16, width = 16, align = "center", backgroundColor = list(image = icon_uri(i, col)))),
                          paste0("i", seq_along(u)))
  rich$t <- list(fontSize = 9, color = COL$muted, fontStyle = if (italic) "italic" else "normal", lineHeight = 12, align = "center")
  key <- paste0("i", match(icons, u))
  list(interval = 0, rich = rich,
       formatter = htmlwidgets::JS(sprintf("function(v, i){ var n=%s, k=%s; if (i<0 || i>=n.length) return ''; return '{'+k[i]+'|}\\n{t|'+n[i]+'}'; }",
                                           js_array(names), js_array(key))))
}

# Nested sunburst from a count table: levels = column names outer->inner, n = counts
sunburst_tree <- function(f, levels) {
  if (length(levels) == 1)
    return(unname(Map(function(nm, v) list(name = nm, value = v), f[[levels]], f$n)))
  lapply(split(f, f[[levels[1]]]), function(d) list(name = d[[levels[1]]][1], children = sunburst_tree(d, levels[-1]))) |> unname()
}
# Leaves show nm(name) and keep the original name as `key` (what click handlers send back)
relabel_leaves <- function(tree, nm) lapply(tree, function(n) {
  if (is.null(n$children)) { n$key <- n$name; n$name <- nm(n$name) } else n$children <- relabel_leaves(n$children, nm)
  n
})
e_sunburst_tree <- function(tree, radius = c("12%", "95%"), colors = PAL) {
  e <- echarts4r::e_charts()
  e$x$opts$series <- list(list(type = "sunburst", data = tree, radius = radius, sort = "desc",
                               itemStyle = list(borderColor = COL$surface, borderWidth = 2),
                               emphasis = list(focus = "ancestor"),
                               levels = list(list(), list(label = list(rotate = "tangential")), list(), list(label = list(fontSize = 9))),
                               label = list(minAngle = 10, fontSize = 10, color = "#fff", overflow = "truncate", width = 90)))
  e |> e_bigal(legend = FALSE) |> echarts4r::e_color(colors) |> no_axes()  # default cartesian axes crash a sunburst series
}

# activity density on the 24h clock (overlap package: circular kernel)
diel_density <- function(h, grid = seq(0, 2 * pi, length.out = 97)) {
  x <- stats::na.omit(h) / 24 * 2 * pi
  if (length(x) < 5) return(NULL)
  data.frame(hour = grid / (2 * pi) * 24, dens = overlap::densityFit(x, grid, overlap::getBandWidth(x)) * 2 * pi / 24)
}

# 24 h clock: records per hour over a shaded night / twilight / day background (equatorial sun times)
clock_chart <- function(h, lang) {
  n <- as.integer(table(factor(floor(stats::na.omit(h)), levels = 0:23)))
  mx <- max(1, n) * 1.1
  bg <- unname(HOUR_COL[hour_period(0:23 + 0.5)])
  e_raw(list(
    polar = list(radius = c("6%", "80%")),
    angleAxis = list(type = "category", data = sprintf("%02dh", 0:23), startAngle = 90, boundaryGap = TRUE,
                     axisLabel = list(interval = 2, color = COL$muted), axisTick = list(show = FALSE),
                     axisLine = list(lineStyle = list(color = COL$line))),
    radiusAxis = list(max = mx, axisLabel = list(show = FALSE), axisLine = list(show = FALSE), axisTick = list(show = FALSE),
                      splitLine = list(lineStyle = list(color = COL$line, type = "dashed"))),
    series = list(
      list(type = "bar", coordinateSystem = "polar", name = "bg", silent = TRUE, barGap = "-100%", barCategoryGap = "0%",
           tooltip = list(show = FALSE), data = lapply(bg, function(c) list(value = mx, itemStyle = list(color = c, opacity = 0.14)))),
      list(type = "bar", coordinateSystem = "polar", name = tr("records", lang), barGap = "-100%", barCategoryGap = "0%",
           # each hour's bar takes the colour of its time of day (navy night -> orange dawn -> yellow midday)
           data = lapply(seq_along(n), function(i) list(value = n[i], itemStyle = list(color = HOUR_RAMP[i]))),
           itemStyle = list(borderRadius = 3, borderColor = COL$surface, borderWidth = 1))
    )))
}

# Violins (+ jittered points, + optional silhouettes behind) for a numeric value by group, drawn with bigal.violin
# Groups are ranked by decreasing median. icons / silhouettes: vectors named by group.
# outlier_mult: drop values above k x the group median (typos like a 50 cm frog would squash every violin).
# sil = "range": silhouette spans min..max, fitted in its band; "median": stands on 0 with height = median, i.e. its
# length is proportional to the typical value (not squeezed into the band).
# click_input: Shiny input receiving the clicked group name.
violin_chart <- function(g, v, lang, icons = NULL, silhouettes = NULL, italic = FALSE, unit = "", colors = PAL, max_pts = 300,
                         outlier_mult = NULL, sil = "range", click_input = NULL, nm = identity) {
  ok <- !is.na(g) & !is.na(v); g <- as.character(g[ok]); v <- v[ok]
  if (length(v) < 3) return(empty_chart(lang))
  lv <- names(sort(tapply(v, g, stats::median), decreasing = TRUE)); K <- length(lv); lab <- nm(lv)
  if (!is.null(icons)) icons <- unname(icons[lv])
  if (!is.null(silhouettes)) silhouettes <- unname(silhouettes[lv])
  set.seed(1)
  parts <- lapply(seq_len(K), function(k) {
    x <- v[g == lv[k]]
    if (!is.null(outlier_mult)) x <- x[x <= outlier_mult * stats::median(x)]
    out <- list(tr = NULL, pts = NULL, info = NULL)
    if (length(unique(x)) >= 3) {
      d <- stats::density(x, from = min(x), to = max(x), n = 48)
      w <- 0.42 * d$y / max(d$y)
      out$tr <- lapply(seq_len(47), function(i) c(k - 1, d$x[i], d$x[i + 1], w[i], w[i + 1]))
    }
    xs <- if (length(x) > max_pts) sample(x, max_pts) else x
    out$pts <- lapply(xs, function(y) list(value = c(k - 1 + stats::runif(1, -0.13, 0.13), y), itemStyle = list(color = colors[(k - 1) %% length(colors) + 1])))
    out$med <- list(value = c(k - 1, stats::median(x)))
    out$info <- sprintf("<b>%s</b><br>n = %d · %s %s %s<br>%s–%s %s", lab[k], length(x), tr("median", lang), signif(stats::median(x), 3), unit,
                        signif(min(x), 3), signif(max(x), 3), unit)
    out$range <- if (sil == "median") c(k - 1, 0, stats::median(x)) else c(k - 1, min(x), max(x))
    out
  })
  # Series live on a hidden numeric x axis (index 1, -0.5..K-0.5) so points can be jittered;
  # the visible category axis (index 0) carries the labels at the same band centres.
  series <- list(
    list(type = "custom", name = "v", xAxisIndex = 1, renderItem = htmlwidgets::JS(sprintf("bigal.violin(%s)", js_array(colors))),
         data = unlist(lapply(parts, `[[`, "tr"), recursive = FALSE), encode = list(x = 0, y = c(1, 2)), z = 2,
         tooltip = list(formatter = htmlwidgets::JS(sprintf("function(p){ return %s[Math.round(p.value[0])]; }", js_array(vapply(parts, `[[`, "", "info")))))),
    list(type = "scatter", name = "pts", xAxisIndex = 1, symbolSize = 4, z = 3, data = unlist(lapply(parts, `[[`, "pts"), recursive = FALSE),
         itemStyle = list(opacity = 0.55), tooltip = list(show = FALSE)),
    list(type = "scatter", name = "med", xAxisIndex = 1, symbol = "rect", symbolSize = c(18, 3), z = 4, itemStyle = list(color = COL$ink),
         data = lapply(parts, `[[`, "med"), tooltip = list(formatter = htmlwidgets::JS(sprintf("function(p){ return %s[p.dataIndex]; }", js_array(vapply(parts, `[[`, "", "info"))))))
  )
  if (!is.null(silhouettes)) {
    u <- unique(silhouettes); uris <- vapply(u, icon_uri, "", col = "#12301B")
    aspect <- vapply(u, function(i) ICON_BOX[[i]][3] / ICON_BOX[[i]][4], 0)
    series <- c(list(list(type = "custom", name = "sil", xAxisIndex = 1, silent = TRUE, z = 1, tooltip = list(show = FALSE),
                          renderItem = htmlwidgets::JS(sprintf("bigal.silhouette(%s, %s, %s)", js_array(uris), js_array(aspect),
                                                               tolower(sil != "median"))),
                          data = lapply(seq_len(K), function(k) c(parts[[k]]$range, match(silhouettes[k], u) - 1)),
                          encode = list(x = 0, y = c(1, 2)))), series)
  }
  e <- e_raw(list(
    grid = list(left = 48, right = 16, top = 36, bottom = 8, containLabel = TRUE),
    xAxis = list(
      list(type = "category", data = lab, axisTick = list(show = FALSE), axisLine = list(lineStyle = list(color = COL$line)),
           axisLabel = if (!is.null(icons)) icon_axis(lab, icons, italic) else
             list(interval = 0, rotate = 20, fontSize = 10, fontStyle = if (italic) "italic" else "normal")),
      list(type = "value", show = FALSE, min = -0.5, max = K - 0.5)),
    yAxis = list(type = "value", name = unit, nameTextStyle = list(align = "left"), axisLine = list(onZero = FALSE), splitLine = list(lineStyle = list(color = COL$line))),
    series = series))
  if (!is.null(click_input)) e <- on_click(e, sprintf(
    "function(p){ var n = %s; var k = Math.round(p.value && p.value.length ? p.value[0] : -1); if (n[k]) Shiny.setInputValue('%s', n[k], {priority: 'event'}); }",
    js_array(lv), click_input))
  e
}
# Attach a click handler to every series of a chart
on_click <- function(e, handler) { e$x$on <- c(e$x$on, list(list(event = "click", query = "series", handler = htmlwidgets::JS(handler)))); e }

# Bubble circle-pack (bigal.pack): clusters (e.g. substrates, site types) with an icon badge, and inside each cluster one
# bubble per group (family, species, camera...) with area proportional to n. f: columns clu, grp, n.
# clu_icons: icon per cluster value; clu_prefix: translation prefix of cluster labels; grp_icons: legend icon per group;
# click_input: Shiny input receiving "clu|<value>" or "grp|<value>".
pack_chart <- function(f, lang, clu_icons = SUBSTRATE_ICON, clu_prefix = "sub", grp_icons = NULL, italic = FALSE,
                       click_input = NULL, top_n = 7, nm = identity) {
  if (!nrow(f)) return(empty_chart(lang))
  fam_top <- names(utils::head(sort(tapply(f$n, f$grp, sum), decreasing = TRUE), top_n))
  f$fam <- ifelse(f$grp %in% fam_top, f$grp, "~other")
  f <- stats::aggregate(n ~ clu + fam, f, sum)
  f$substrate <- f$clu
  subs <- names(sort(tapply(f$n, f$substrate, sum), decreasing = TRUE))
  cl <- lapply(subs, function(s) {
    d <- f[f$substrate == s, ]; d <- d[order(-d$n), ]
    lay <- packcircles::circleProgressiveLayout(d$n, sizetype = "area")
    c0 <- c(mean(range(lay$x - lay$radius, lay$x + lay$radius)), mean(range(lay$y - lay$radius, lay$y + lay$radius)))
    d$x <- lay$x - c0[1]; d$y <- lay$y - c0[2]; d$r <- lay$radius
    list(d = d, R = max(sqrt(d$x^2 + d$y^2) + d$r) * 1.12)
  })
  # clusters keep a minimum radius so small substrates still show their icon badge (bubbles stay on one area scale)
  R <- pmax(vapply(cl, `[[`, 0, "R"), 0.22 * max(vapply(cl, `[[`, 0, "R")))
  outer <- packcircles::circleProgressiveLayout(R * 1.22, sizetype = "radius")
  ox <- mean(range(outer$x - R, outer$x + R)); oy <- mean(range(outer$y - R, outer$y + R))
  sc <- 1 / max(diff(range(outer$x - R, outer$x + R)), diff(range(outer$y - R, outer$y + R))) * 2
  fams <- c(fam_top, "~other"); cols <- c(PAL[seq_along(fam_top)], OTHER_COL)
  famlab <- c(nm(fam_top), tr("other", lang))
  clab <- trv(subs, clu_prefix, lang)
  tot <- tapply(f$n, f$substrate, sum)
  items <- list(); info <- character(); pay <- character()
  for (k in seq_along(cl)) {  # outlines first, then bubbles, then badges on top
    items[[length(items) + 1]] <- c((outer$x[k] - ox) * sc, (outer$y[k] - oy) * sc, R[k] * sc, 1, k - 1)
    info <- c(info, sprintf("<b>%s</b><br>n = %d", clab[k], tot[[subs[k]]])); pay <- c(pay, paste0("clu|", subs[k]))
  }
  for (k in seq_along(cl)) for (i in seq_len(nrow(cl[[k]]$d))) {
    d <- cl[[k]]$d[i, ]; j <- match(d$fam, fams)
    items[[length(items) + 1]] <- c((outer$x[k] + d$x - ox) * sc, (outer$y[k] + d$y - oy) * sc, d$r * sc, 0, j - 1)
    info <- c(info, sprintf("<b>%s</b> · %s<br>n = %d · %.0f %%", if (italic && j <= length(fam_top)) paste0("<i>", famlab[j], "</i>") else famlab[j],
                            clab[k], d$n, 100 * d$n / tot[[subs[k]]]))
    pay <- c(pay, if (d$fam == "~other") "" else paste0("grp|", d$fam))
  }
  for (k in seq_along(cl)) {
    items[[length(items) + 1]] <- c((outer$x[k] - ox) * sc, (outer$y[k] - oy) * sc, R[k] * sc, 2, k - 1)
    info <- c(info, info[k]); pay <- c(pay, pay[k])
  }
  icons <- vapply(subs, function(s) icon_uri(clu_icons[s] %|NA|% "circle", COL$green), "")
  cfg <- list(colors = cols, icons = unname(icons), labels = unname(clab))
  # empty scatter series only to get a group legend with each group's icon
  leg <- lapply(seq_along(fams), function(j) list(type = "scatter", name = famlab[j], data = list(), itemStyle = list(color = cols[j])))
  e <- e_raw(list(
    legend = list(bottom = 0, selectedMode = FALSE, itemWidth = 14, itemHeight = 14,
                  textStyle = list(color = COL$muted, fontSize = 10, fontStyle = if (italic) "italic" else "normal"),
                  data = lapply(seq_along(fams), function(j) list(name = famlab[j],
                    icon = e_icon(if (j <= length(fam_top) && !is.null(grp_icons)) grp_icons[fam_top[j]] %|NA|% "circle" else "circle")))),
    xAxis = list(show = FALSE), yAxis = list(show = FALSE),
    series = c(list(list(type = "custom", coordinateSystem = "none", renderItem = htmlwidgets::JS(sprintf("bigal.pack(%s)", jsonlite::toJSON(cfg, auto_unbox = TRUE))),
                         data = items, tooltip = list(formatter = htmlwidgets::JS(sprintf("function(p){ return %s[p.dataIndex]; }", js_array(info)))))), leg)))
  if (!is.null(click_input)) e <- on_click(e, sprintf(
    "function(p){ var v = %s[p.dataIndex]; if (p.seriesIndex === 0 && v) Shiny.setInputValue('%s', v, {priority: 'event'}); }", js_array(pay), click_input))
  e
}
`%|NA|%` <- function(a, b) { a <- unname(a); if (length(a) == 0 || is.na(a[1])) b else a[1] }

# 100 % stacked bars of a category per species; `labels`/`icons` translate and decorate the categories
stack100 <- function(d, cat, lang, n = 12, prefix = NULL, icons = NULL, nm = identity) {
  d <- d[!is.na(d[[cat]]), ]; if (!nrow(d)) return(empty_chart(lang))
  sp <- top_species(d, n); d <- d[d$binomial %in% sp, ]
  lv <- names(sort(table(d[[cat]]), decreasing = TRUE))
  lab <- if (is.null(prefix)) lv else trv(lv, prefix, lang)
  f <- as.data.frame(prop.table(table(d$binomial, factor(d[[cat]], lv)), 1) * 100)
  names(f) <- c("binomial", "cat", "pct")
  f$cat <- factor(lab[match(f$cat, lv)], lab)  # factor keeps series (and colour) order = category frequency
  f$binomial <- factor(nm(as.character(f$binomial)), nm(rev(sp))); f <- f[order(f$cat, f$binomial), ]
  e <- f |> dplyr::group_by(cat) |> echarts4r::e_charts(binomial, reorder = FALSE) |>
    echarts4r::e_bar(pct, stack = "s", barMaxWidth = 16, itemStyle = list(borderColor = COL$surface, borderWidth = 1)) |>
    echarts4r::e_flip_coords() |> e_bigal() |>
    e_tip(trigger = "axis", valueFormatter = htmlwidgets::JS("v => v.toFixed(0) + ' %'")) |>
    echarts4r::e_x_axis(max = 100) |> echarts4r::e_y_axis(axisLabel = italic_axis)
  if (!is.null(icons)) {
    e$x$opts$legend$data <- lapply(seq_along(lv), function(i) list(name = lab[i], icon = e_icon(icons[[lv[i]]] %||% "circle")))
    e$x$opts$legend$itemWidth <- 20; e$x$opts$legend$itemHeight <- 18
  }
  e
}

# ---- v1.2 builders ------------------------------------------------------------------------

MOON_LIT_COL <- local({ lit <- (1 - cos(2 * pi * (0:7) / 8)) / 2
  grDevices::colorRampPalette(c("#34453A", "#8FA88A", "#F1E3AE"))(101)[round(lit * 100) + 1] })
# cyclic hour colours: night navy -> dawn orange -> midday yellow -> dusk orange -> navy
HOUR_RAMP <- grDevices::colorRampPalette(c("#2B3A67", "#34477A", "#D9822B", "#E8C547", "#E8C547", "#E8C547", "#D9822B", "#34477A", "#2B3A67"))(24)

# Taxonomic tree (Linnaean hierarchy, not a phylogeny with branch lengths): Class > Order > Family > Genus > Species
taxo_tree <- function(o, lang, nm = identity) {
  o <- o[!is.na(o$binomial) & !is.na(o$class), ]
  if (!nrow(o)) return(empty_chart(lang))
  o$order[is.na(o$order)] <- "?"; o$family[is.na(o$family)] <- "?"
  f <- count_by(o, class, order, family, genus, binomial)
  lab <- nm  # `nm` is reused below for node names
  ccol <- c("Mamífero" = PAL[1], "Anfibio" = PAL[3], "Reptil" = PAL[8])
  keys <- c("class", "order", "family", "genus", "binomial")
  node <- function(d, lv, col) unname(lapply(split(d, d[[keys[lv]]]), function(s) {
    nm <- s[[keys[lv]]][1]; n <- sum(s$n)
    col2 <- if (lv == 1) ccol[nm] %|NA|% COL$green else col
    if (lv == 5) {
      iu <- if (!is.null(SPECIES_MEDIA$iucn)) SPECIES_MEDIA$iucn[match(nm, SPECIES_MEDIA$binomial)] else NA
      thr <- isTRUE(iu %in% c(IUCN_THREAT, "NT"))
      return(list(name = lab(nm), key = nm, value = n, symbolSize = 4 + 1.6 * sqrt(n)^0.8,
                  itemStyle = list(color = col2, borderColor = if (thr) IUCN_COL[[iu]] else col2, borderWidth = if (thr) 3 else 1),
                  label = list(show = FALSE), emphasis = list(label = list(show = TRUE, fontStyle = "italic", fontSize = 10, color = COL$ink))))
    }
    list(name = if (lv == 1) trv(nm, "cls", lang) else nm, value = n, itemStyle = list(color = col2),
         label = list(show = lv != 4, fontSize = c(12, 10, 9, 8)[lv], fontWeight = if (lv <= 2) 600 else 400, color = col2),
         children = node(s, lv + 1, col2))
  }))
  tree <- list(name = "RioBigal", itemStyle = list(color = COL$green_dk), label = list(show = FALSE), children = node(f, 1, COL$green))
  e <- e_raw(list(
    tooltip = list(trigger = "item", formatter = htmlwidgets::JS("function(p){ return '<b>' + p.name + '</b><br>' + p.value + ' reg.'; }")),
    series = list(list(type = "tree", data = list(tree), layout = "radial", symbol = "circle", symbolSize = 6, roam = TRUE,
                       initialTreeDepth = -1, expandAndCollapse = TRUE, animationDuration = 400,
                       lineStyle = list(color = "#CFC6AE", width = 1, curveness = 0.4),
                       label = list(fontSize = 9, color = COL$muted), emphasis = list(focus = "ancestor")))))
  on_click(e, "function(p){ if (p.data && !p.data.children) Shiny.setInputValue('sunburst_click', p.data.key || p.name, {priority: 'event'}); }")
}

# First record of each species through time (all sources), with photo markers for recent & threatened species
discovery_chart <- function(o, lang, nm = identity) {
  o <- o[!is.na(o$date) & !is.na(o$binomial) & !grepl(" sp\\.$", o$binomial), ]
  if (!nrow(o)) return(empty_chart(lang))
  f <- stats::aggregate(date ~ binomial, o, min); f <- f[order(f$date, f$binomial), ]
  f$src <- o$source[match(paste(f$binomial, f$date), paste(o$binomial, o$date))]
  f$cum <- seq_len(nrow(f))
  img <- sp_img(f$binomial)
  iu <- SPECIES_MEDIA$iucn[match(f$binomial, SPECIES_MEDIA$binomial)]
  show <- !is.na(img) & (seq_len(nrow(f)) > nrow(f) - 8 | iu %in% IUCN_THREAT)
  info <- sprintf("<b><i>%s</i></b><br>%s · %s<br>#%d", nm(f$binomial), format(f$date, "%d/%m/%Y"), trv(f$src, "src", lang), f$cum)
  pts <- lapply(seq_len(nrow(f)), function(i) {
    p <- list(value = list(format(f$date[i]), f$cum[i]))
    if (show[i]) { p$symbol <- paste0("image://", img[i]); p$symbolSize <- 26 } else { p$symbolSize <- 5 }
    p
  })
  e_raw(list(
    grid = list(left = 40, right = 24, top = 40, bottom = 24, containLabel = TRUE),
    xAxis = list(type = "time", splitLine = list(show = FALSE)),
    yAxis = list(type = "value", name = tr("species_n", lang), nameTextStyle = list(align = "left"), splitLine = list(lineStyle = list(color = COL$line))),
    tooltip = list(trigger = "item", formatter = htmlwidgets::JS(sprintf("function(p){ return %s[p.dataIndex]; }", js_array(info)))),
    series = list(list(type = "line", step = "end", data = pts, symbol = "circle", itemStyle = list(color = COL$green),
                       lineStyle = list(color = COL$green, width = 2),
                       areaStyle = list(color = list(type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
                                                     colorStops = list(list(offset = 0, color = "rgba(47,125,50,0.25)"), list(offset = 1, color = "rgba(47,125,50,0.02)"))))))))
}

# Jaccard similarity of species lists between trails, ordered by average-linkage clustering, with a dendrogram on top
trail_sim_chart <- function(o, lang) {
  o <- o[!is.na(o$trail) & !is.na(o$binomial) & !grepl(" sp\\.$", o$binomial), ]
  tr_n <- table(o$trail); trails <- names(tr_n[tr_n >= 15])
  if (length(trails) < 3) return(empty_chart(lang))
  sets <- lapply(stats::setNames(trails, trails), function(t) unique(o$binomial[o$trail == t]))
  J <- outer(trails, trails, Vectorize(function(a, b) length(intersect(sets[[a]], sets[[b]])) / length(union(sets[[a]], sets[[b]]))))
  dimnames(J) <- list(trails, trails)
  hc <- stats::hclust(stats::as.dist(1 - J), "average")
  ord <- trails[hc$order]; n <- length(ord); pos <- stats::setNames(seq_len(n) - 1, trails[hc$order])
  cx <- numeric(n - 1); ch <- numeric(n - 1); seg <- list()
  for (i in seq_len(n - 1)) {
    kid <- hc$merge[i, ]
    xs <- vapply(kid, function(k) if (k < 0) pos[[trails[-k]]] else cx[k], 0)
    hs <- vapply(kid, function(k) if (k < 0) 0 else ch[k], 0)
    H <- hc$height[i]
    seg <- c(seg, list(c(xs[1], hs[1], xs[1], H), c(xs[2], hs[2], xs[2], H), c(xs[1], H, xs[2], H)))
    cx[i] <- mean(xs); ch[i] <- H
  }
  cells <- expand.grid(i = seq_len(n) - 1, j = seq_len(n) - 1)
  cells$v <- mapply(function(i, j) round(J[ord[i + 1], ord[j + 1]], 2), cells$i, cells$j)
  info <- mapply(function(i, j) { a <- sets[[ord[i + 1]]]; b <- sets[[ord[j + 1]]]
    sprintf("<b>%s</b> × <b>%s</b><br>Jaccard %.2f<br>%s: %d · %s: %d / %d", ord[i + 1], ord[j + 1], J[ord[i + 1], ord[j + 1]],
            tr("shared", lang), length(intersect(a, b)), tr("unique", lang), length(setdiff(a, b)), length(setdiff(b, a))) }, cells$i, cells$j)
  e_raw(list(
    grid = list(list(left = 110, right = 70, top = 10, height = "16%"), list(left = 110, right = 70, top = "22%", bottom = 70)),
    xAxis = list(list(type = "value", gridIndex = 0, show = FALSE, min = -0.5, max = n - 0.5),
                 list(type = "category", gridIndex = 1, data = ord, axisLabel = list(rotate = 40, interval = 0, fontSize = 10), axisTick = list(show = FALSE))),
    yAxis = list(list(type = "value", gridIndex = 0, show = FALSE, min = 0, max = max(hc$height)),
                 list(type = "category", gridIndex = 1, data = ord, axisLabel = list(interval = 0, fontSize = 10), axisTick = list(show = FALSE))),
    visualMap = list(seriesIndex = 1, min = 0, max = 1, calculable = TRUE, orient = "vertical", right = 0, top = "middle", itemHeight = 110,
                     itemWidth = 10, precision = 2, inRange = list(color = c("#F2EEDF", "#B9D3AE", "#6FA36A", "#2F7D32", "#12301B")),
                     textStyle = list(color = COL$muted)),
    series = list(
      list(type = "custom", xAxisIndex = 0, yAxisIndex = 0, renderItem = htmlwidgets::JS(sprintf("bigal.segment('%s')", COL$sage)),
           data = lapply(seg, unname), silent = TRUE, tooltip = list(show = FALSE)),
      list(type = "heatmap", xAxisIndex = 1, yAxisIndex = 1, itemStyle = list(borderColor = COL$surface, borderWidth = 2),
           data = lapply(seq_len(nrow(cells)), function(k) c(cells$i[k], cells$j[k], cells$v[k])),
           tooltip = list(formatter = htmlwidgets::JS(sprintf("function(p){ return %s[p.dataIndex]; }", js_array(info))))))))
}

# Elevation profile of a trail from GPS balises, with the most tracked species at each balise as a photo pin
elev_chart <- function(b, tracks, trail, lang, nm = identity) {
  d <- b[!is.na(b$alt) & b$trail %in% trail & !is.na(b$num), ]; d <- d[order(d$num), ]
  if (nrow(d) < 3) return(empty_chart(lang))
  km <- c(0, cumsum(sqrt((diff(d$lon) * 111320)^2 + (diff(d$lat) * 110570)^2)) / 1000)
  col <- unname(TRAIL_COL[trail]) %|NA|% COL$green
  t <- tracks[!is.na(tracks$balise) & tracks$balise %in% d$code & !is.na(tracks$binomial), ]
  pins <- lapply(seq_len(nrow(d)), function(i) {
    tb <- sort(table(t$binomial[t$balise == d$code[i]]), decreasing = TRUE)
    if (!length(tb)) return(NULL)
    img <- sp_img(names(tb)[1])
    list(value = c(km[i], d$alt[i]), symbol = if (is.na(img)) e_icon(track_icon(names(tb)[1])) else paste0("image://", img),
         symbolSize = 26, symbolOffset = c(0, -22),
         tip = sprintf("<b>%s</b> · %d m<br>%s", d$code[i], d$alt[i],
                       paste(sprintf("<i>%s</i> %d", nm(names(tb)[1:min(4, length(tb))]), as.integer(tb)[1:min(4, length(tb))]), collapse = "<br>")))
  })
  pins <- Filter(Negate(is.null), pins)
  tips <- vapply(pins, `[[`, "", "tip"); pins <- lapply(pins, function(p) { p$tip <- NULL; p })
  e_raw(list(
    grid = list(left = 50, right = 24, top = 40, bottom = 28, containLabel = TRUE),
    xAxis = list(type = "value", name = "km", nameLocation = "end", splitLine = list(show = FALSE)),
    yAxis = list(type = "value", min = floor((min(d$alt) - 40) / 10) * 10, name = "m", splitLine = list(lineStyle = list(color = COL$line))),
    series = list(
      list(type = "line", name = trail, smooth = 0.3, symbol = "circle", symbolSize = 6, data = unname(Map(c, km, d$alt)),
           itemStyle = list(color = col), lineStyle = list(color = col, width = 2.5),
           label = list(show = TRUE, formatter = htmlwidgets::JS(sprintf("function(p){ return %s[p.dataIndex]; }", js_array(d$code))),
                        fontSize = 9, color = COL$muted, position = "bottom"),
           areaStyle = list(color = list(type = "linear", x = 0, y = 0, x2 = 0, y2 = 1,
                                         colorStops = list(list(offset = 0, color = paste0(col, "99")), list(offset = 1, color = paste0(col, "08"))))),
           tooltip = list(formatter = htmlwidgets::JS(sprintf("function(p){ return '<b>' + %s[p.dataIndex] + '</b> · ' + p.value[1] + ' m · ' + p.value[0].toFixed(2) + ' km'; }", js_array(d$code))))),
      list(type = "scatter", name = "pins", data = pins, z = 5,
           tooltip = list(formatter = htmlwidgets::JS(sprintf("function(p){ return %s[p.dataIndex]; }", js_array(tips))))))))
}

# ---- weather infographics (HTML/SVG) ----
thermo_svg <- function(base, sp, lo, hi) {
  y <- function(t) 150 - (t - lo) / max(1, hi - lo) * 130
  q <- stats::quantile(sp, c(.25, .5, .75), na.rm = TRUE); qb <- stats::quantile(base, c(.05, .95), na.rm = TRUE)
  col <- grDevices::colorRampPalette(c("#3C6E9F", "#E8B84A", "#C4502F"))(100)[max(1, min(100, round((q[2] - lo) / max(1, hi - lo) * 99) + 1))]
  htmltools::HTML(sprintf(paste0(
    "<svg viewBox='0 0 110 190' class='thermo'><rect x='38' y='10' width='18' height='150' rx='9' fill='#EFE9DA' stroke='#CFC6AE'/>",
    "<rect x='41' y='%.1f' width='12' height='%.1f' rx='4' fill='#D8D0BC'/>",
    "<rect x='40' y='%.1f' width='14' height='%.1f' rx='5' fill='%s'/>",
    "<line x1='34' x2='60' y1='%.1f' y2='%.1f' stroke='#1F2A22' stroke-width='2'/>",
    "<circle cx='47' cy='170' r='14' fill='%s' stroke='#CFC6AE'/>",
    "<text x='64' y='%.1f' class='th-med'>%.1f°</text>",
    "<text x='30' y='%.1f' class='th-tick' text-anchor='end'>%.0f°</text><text x='30' y='%.1f' class='th-tick' text-anchor='end'>%.0f°</text></svg>"),
    y(qb[2]), y(qb[1]) - y(qb[2]), y(q[3]), max(3, y(q[1]) - y(q[3])), col, y(q[2]), y(q[2]), col, y(q[2]) + 4, q[2],
    y(hi) + 4, hi, y(lo) + 4, lo))
}
drop_svg <- function(h) {
  m <- stats::median(h, na.rm = TRUE); fy <- 76 - m / 100 * 70
  htmltools::HTML(sprintf(paste0(
    "<svg viewBox='0 0 60 82' class='drop'><defs><clipPath id='dropclip'><path d='M30 3 C30 3 7 34 7 52 a23 23 0 0 0 46 0 C53 34 30 3 30 3Z'/></clipPath></defs>",
    "<path d='M30 3 C30 3 7 34 7 52 a23 23 0 0 0 46 0 C53 34 30 3 30 3Z' fill='#EAF1F6' stroke='#9DB4C0' stroke-width='1.5'/>",
    "<rect x='0' y='%.1f' width='60' height='%.1f' fill='#5B8DB8' clip-path='url(#dropclip)'/>",
    "<text x='30' y='58' text-anchor='middle' class='drop-txt'>%.0f%%</text></svg>"), fy, 82 - fy, m))
}
# Species weather card: sky preference vs survey baseline, thermometer, humidity droplet
weather_card <- function(sp, base, lang) {
  sp <- sp[!is.na(sp$sky) | !is.na(sp$temp) | !is.na(sp$hum), ]
  if (!nrow(sp)) return(htmltools::p(class = "text-muted small", tr("no_weather", lang)))
  share <- function(x) { t <- table(factor(x, SKY_LEVELS)); if (sum(t)) t / sum(t) else t }
  s1 <- share(sp$sky); s0 <- share(base$sky)
  sky <- if (sum(!is.na(sp$sky)) >= 3) htmltools::div(class = "wx-sky", lapply(SKY_LEVELS, function(k) {
    p <- as.numeric(s1[k]); r <- if (as.numeric(s0[k]) > 0) p / as.numeric(s0[k]) else NA
    sz <- 30 + 34 * sqrt(p)
    badge <- if (is.na(r) || p == 0) "" else if (r > 1.25) sprintf("<span class='wx-up'>×%.1f</span>", r) else if (r < 0.8) sprintf("<span class='wx-down'>×%.1f</span>", r) else "<span class='wx-eq'>≈</span>"
    htmltools::div(class = "wx-item", title = sprintf("%s: %.0f %% (%s %.0f %%)", trv(k, "sky", lang), 100 * p, tr("all_surveys", lang), 100 * as.numeric(s0[k])),
      htmltools::div(class = "wx-ring", style = sprintf("width:%.0fpx;height:%.0fpx;border-color:%s;opacity:%s", sz, sz, SKY_COL[[k]], if (p == 0) 0.35 else 1),
                     html_icon(SKY_ICON[[k]], SKY_COL[[k]], sprintf("%.0fpx", sz * 0.45))),
      htmltools::div(class = "wx-pct", sprintf("%.0f%%", 100 * p)), htmltools::HTML(badge),
      htmltools::div(class = "wx-lab", trv(k, "sky", lang)))
  }))
  temps <- sp$temp[!is.na(sp$temp)]; bt <- base$temp[!is.na(base$temp)]
  hums <- sp$hum[!is.na(sp$hum)]
  lo <- floor(stats::quantile(bt, .02, na.rm = TRUE)); hi <- ceiling(stats::quantile(bt, .98, na.rm = TRUE))
  era5 <- mean(sp$wx_src == "era5", na.rm = TRUE)
  htmltools::div(class = "wx-card",
    sky,
    htmltools::div(class = "wx-row",
      if (length(temps) >= 3) htmltools::div(class = "wx-th", thermo_svg(bt, temps, lo, hi),
        htmltools::div(class = "wx-cap", sprintf("%s %.0f–%.0f °C", tr("iqr", lang), stats::quantile(temps, .25), stats::quantile(temps, .75)))),
      if (length(hums) >= 3) htmltools::div(class = "wx-dr", drop_svg(hums),
        htmltools::div(class = "wx-cap", sprintf("%s %.0f–%.0f %%", tr("iqr", lang), stats::quantile(hums, .25), stats::quantile(hums, .75))))),
    htmltools::div(class = "wx-note", sprintf(tr("wx_note", lang), nrow(sp)), if (era5 > 0) htmltools::span(class = "era5", sprintf(tr("wx_era5", lang), 100 * era5))))
}

# Herp nights: temperature x humidity "sweet spot" bubbles (size = individuals that night, colour = sky)
herp_nights <- function(h) {
  h <- h[!is.na(h$date), ]; if (!nrow(h)) return(NULL)
  key <- paste(h$date, h$trail_raw)
  mode <- function(x) { x <- x[!is.na(x)]; if (length(x)) names(sort(table(x), decreasing = TRUE))[1] else NA }
  data.frame(n = as.numeric(tapply(key, key, length)), temp = as.numeric(tapply(h$temp, key, mean, na.rm = TRUE)),
             hum = as.numeric(tapply(h$hum, key, mean, na.rm = TRUE)), sky = as.character(tapply(h$sky, key, mode)))
}
# Association of each night-weather factor with individuals per night: Spearman ρ for temperature / humidity,
# Kruskal-Wallis for the sky (categories with >= 5 nights). `best` = most significant factor (lowest p).
herp_wx_tests <- function(nt) {
  rho <- function(v) { ok <- is.finite(v)
    if (sum(ok) < 10 || stats::sd(v[ok]) == 0) return(c(NA, NA))
    t <- suppressWarnings(stats::cor.test(nt$n[ok], v[ok], method = "spearman", exact = FALSE)); c(unname(t$estimate), t$p.value) }
  s <- nt[!is.na(nt$sky), ]; s <- s[s$sky %in% names(which(table(s$sky) >= 5)), ]
  kw <- if (length(unique(s$sky)) >= 2) stats::kruskal.test(s$n, factor(s$sky))$p.value else NA
  r <- data.frame(var = c("temp", "hum", "sky"), rho = c(rho(nt$temp)[1], rho(nt$hum)[1], NA), p = c(rho(nt$temp)[2], rho(nt$hum)[2], kw))
  attr(r, "best") <- if (all(is.na(r$p))) NA else r$var[which.min(r$p)]
  r
}
p_label <- function(p) ifelse(is.na(p), "–", ifelse(p < 0.001, "p < 0.001", sprintf("p = %.3f", p)))

# Nights as sky icons: x = mean temperature, y = individuals found, colour = sky, size = humidity
# (scaled over the observed humidity range, which is narrow; nights without humidity are drawn small and faint)
herp_wx_chart <- function(nt, lang) {
  d <- nt[is.finite(nt$temp), ]; if (nrow(d) < 3) return(empty_chart(lang))
  d$sky[is.na(d$sky)] <- "?"
  lv <- c(SKY_LEVELS, "?"); lv <- lv[lv %in% d$sky]
  hr <- range(d$hum[is.finite(d$hum)]); if (!all(is.finite(hr))) hr <- c(0, 1)
  size <- ifelse(is.finite(d$hum), 7 + 17 * (d$hum - hr[1]) / max(1, diff(hr)), 6)
  name <- function(k) if (k == "?") tr("no_sky", lang) else trv(k, "sky", lang)
  ser <- lapply(lv, function(k) {
    i <- which(d$sky == k)
    list(type = "scatter", name = name(k), symbol = e_icon(if (k == "?") "circle" else SKY_ICON[[k]]),
         itemStyle = list(color = if (k == "?") OTHER_COL else SKY_COL[[k]], opacity = 0.6),
         data = lapply(i, function(j) list(value = c(round(d$temp[j], 1), d$n[j]), symbolSize = round(size[j], 1),
                                           hum = if (is.finite(d$hum[j])) round(d$hum[j]) else "–",
                                           itemStyle = if (!is.finite(d$hum[j])) list(opacity = 0.35))))
  })
  e_raw(list(
    grid = list(left = 44, right = 40, top = 36, bottom = 56, containLabel = TRUE),
    legend = list(bottom = 0, textStyle = list(color = COL$muted), itemWidth = 18, itemHeight = 16, itemStyle = list(opacity = 1),
                  data = lapply(lv, function(k) list(name = name(k), icon = e_icon(if (k == "?") "circle" else SKY_ICON[[k]])))),
    graphic = list(list(type = "text", right = 24, top = 4,
                        style = list(text = sprintf(tr("size_hum", lang), hr[1], hr[2]), fill = COL$muted, fontSize = 10, fontFamily = "Inter"))),
    tooltip = list(formatter = htmlwidgets::JS(sprintf("function(p){ return p.seriesName + '<br>' + p.value[0] + ' °C · %s ' + p.data.hum + ' %%<br><b>' + p.value[1] + '</b> %s'; }",
                                                       tr("humidity_short", lang), tr("per_night", lang)))),
    xAxis = list(type = "value", name = "°C", min = "dataMin", nameLocation = "end", splitLine = list(lineStyle = list(color = COL$line))),
    yAxis = list(type = "value", name = tr("per_night", lang), min = 0, nameTextStyle = list(align = "left"), splitLine = list(lineStyle = list(color = COL$line))),
    series = ser))
}

rt_lang <- function(lang) if (lang == "es") reactable::reactableLang(searchPlaceholder = "Buscar", noData = "Sin datos",
  pageInfo = "{rowStart}–{rowEnd} de {rows}", pagePrevious = "‹", pageNext = "›", filterPlaceholder = "Filtrar") else
  reactable::reactableLang(searchPlaceholder = "Search", filterPlaceholder = "Filter")

stats_server <- function(input, output, session, obs, src, db, stations, lang, spn) {
  l <- lang
  # ---------------- Overview ----------------
  output$k_records <- shiny::renderText(format(nrow(obs()), big.mark = " "))
  output$k_species <- shiny::renderText(length(unique(stats::na.omit(obs()$binomial[!grepl(" sp\\.$", obs()$binomial)]))))
  output$k_events <- shiny::renderText(format(length(unique(src()$photos$event)), big.mark = " "))
  output$k_threat <- shiny::renderText(length(unique(obs()$binomial[obs()$iucn %in% IUCN_THREAT])))

  # modality dropdown labels follow the language
  shiny::observeEvent(l(), {
    for (id in c("he_mod_sub", "he_mod_h", "he_mod_lt"))
      shiny::updateSelectInput(session, id, choices = stats::setNames(MODALITIES, vapply(paste0("mod_", MODALITIES), tr, "", lang = l())),
                               selected = shiny::isolate(input[[id]]))
  }, ignoreInit = TRUE)

  output$c_discovery <- echarts4r::renderEcharts4r(discovery_chart(obs(), l(), spn()))
  output$newcomers <- shiny::renderUI({
    o <- obs(); lang <- l(); o <- o[!is.na(o$date) & !is.na(o$binomial) & !grepl(" sp\\.$", o$binomial), ]
    if (!nrow(o)) return(NULL)
    f <- stats::aggregate(date ~ binomial, o, min); f <- utils::tail(f[order(f$date), ], 5)
    htmltools::div(class = "newcomers", htmltools::span(class = "nc-title", tr("newcomers", lang)),
      lapply(rev(seq_len(nrow(f))), function(i) htmltools::div(class = "nc",
        sp_avatar(f$binomial[i], o$class[match(f$binomial[i], o$binomial)], 34, family = o$family[match(f$binomial[i], o$binomial)]),
        htmltools::div(htmltools::tags$i(spn()(f$binomial[i])), htmltools::tags$small(format(f$date[i], "%m/%Y"))))))
  })
  output$c_trail_sim <- echarts4r::renderEcharts4r(trail_sim_chart(obs(), l()))
  elev_trails <- shiny::reactive({ b <- db()$balises; tb <- table(b$trail[!is.na(b$alt)]); names(tb[tb >= 3]) })
  output$elev_pick <- shiny::renderUI({
    tr_ <- elev_trails(); if (!length(tr_)) return(NULL)
    htmltools::div(class = "seg", shiny::radioButtons("elev_trail", NULL, choices = tr_, inline = TRUE,
                                                      selected = shiny::isolate(input$elev_trail) %||% tr_[1]))
  })
  output$c_elev <- echarts4r::renderEcharts4r({
    shiny::req(input$elev_trail)
    elev_chart(db()$balises, src()$tracks, input$elev_trail, l(), spn())
  })

  output$podium <- shiny::renderUI({
    o <- obs(); lang <- l(); o <- o[!is.na(o$binomial) & !grepl(" sp\\.$", o$binomial), ]
    top <- utils::head(sort(table(o$binomial), decreasing = TRUE), 3)
    if (!length(top)) return(htmltools::p(class = "text-muted", tr("no_data", lang)))
    medal <- c("gold", "silver", "bronze"); ring <- c("#D4AF37", "#B8BCC2", "#C07A45")
    step <- function(i) {
      b <- names(top)[i]; r <- o[o$binomial == b, ][1, ]
      htmltools::div(class = paste0("pod pod-", i),
        htmltools::div(class = "pod-av", sp_avatar(b, r$class, size = c(84, 68, 64)[i], ring = ring[i], family = r$family),
                       htmltools::span(class = paste("medal", medal[i]), i)),
        htmltools::div(class = "pod-name", htmltools::tags$i(b), htmltools::tags$small(sp_common(b, r$common_es, lang)), iucn_pill(r$iucn, lang)),
        htmltools::div(class = "pod-block", htmltools::tags$b(format(as.integer(top[[i]]), big.mark = " ")), htmltools::span(tr("records", lang))))
    }
    htmltools::div(class = "podium", lapply(intersect(c(2, 1, 3), seq_along(top)), step))
  })

  accum <- shiny::reactive({
    o <- obs()
    o <- o[!is.na(o$binomial) & !grepl(" sp\\.$", o$binomial), ]
    x <- lapply(split(o$binomial, o$source), function(b) as.numeric(sort(table(b), decreasing = TRUE)))
    x <- x[vapply(x, function(v) length(v) >= 3 && sum(v) > 10, TRUE)]
    if (!length(x)) return(NULL)
    tryCatch(iNEXT::iNEXT(x, q = 0, datatype = "abundance", knots = 30, nboot = 0), error = function(e) NULL)
  }) |> shiny::bindCache(obs())

  output$c_accum <- echarts4r::renderEcharts4r({
    r <- accum(); lang <- l()
    if (is.null(r)) return(empty_chart(lang))
    s <- r$iNextEst$size_based
    s$src <- src_label(s$Assemblage, lang)
    s$part <- ifelse(s$Method == "Extrapolation", "b", "a")
    s$serie <- paste(s$src, ifelse(s$part == "b", tr("extrapolated", lang), ""))
    # join the interpolated and extrapolated pieces at the observed point
    obs_pt <- s[s$Method == "Observed", ]; obs_pt$part <- "b"; obs_pt$serie <- paste(obs_pt$src, tr("extrapolated", lang))
    s <- rbind(s, obs_pt)
    s <- s[order(s$Assemblage, s$part, s$m), ]
    s$serie <- trimws(s$serie)
    series <- sort(unique(s$serie))  # group_by() emits series in this order
    cols <- unname(SOURCE_COL[s$Assemblage[match(series, s$serie)]])
    main <- series[!grepl(tr("extrapolated", lang), series, fixed = TRUE)]
    e <- s |> dplyr::group_by(serie) |> echarts4r::e_charts(m, reorder = FALSE) |>
      echarts4r::e_line(qD, symbol = "none", smooth = TRUE) |> e_bigal() |> echarts4r::e_color(cols) |>
      e_tip(trigger = "axis", valueFormatter = htmlwidgets::JS("v => Math.round(v)")) |>
      echarts4r::e_x_axis(type = "value", name = tr("n_records", lang), nameLocation = "end",
                          axisLabel = list(formatter = htmlwidgets::JS("v => v >= 1000 ? (v/1000) + 'k' : v"))) |>
      echarts4r::e_y_axis(nameTextStyle = list(align = "left"), name = tr("species_n", lang))
    e$x$opts$legend$data <- lapply(main, function(m) list(name = m, icon = e_icon(SOURCE_ICON[[s$Assemblage[match(m, s$serie)]]])))
    dashed_series(e, which(grepl(tr("extrapolated", lang), series, fixed = TRUE)))
  })
  output$accum_note <- shiny::renderUI({
    r <- accum(); lang <- l(); if (is.null(r)) return(NULL)
    a <- r$AsyEst; a <- a[a$Diversity == "Species richness", ]
    htmltools::div(class = "small text-muted", lapply(seq_len(nrow(a)), function(i)
      htmltools::span(class = "note-chip", html_icon(SOURCE_ICON[[a$Assemblage[i]]], SOURCE_COL[[a$Assemblage[i]]]),
                      sprintf(tr("chao_note", lang), src_label(a$Assemblage[i], lang), round(a$Observed[i]), round(a$Estimator[i])))))
  })

  output$c_sunburst <- echarts4r::renderEcharts4r({
    o <- obs(); lang <- l()
    o <- o[!is.na(o$binomial) & !is.na(o$class), ]
    if (!nrow(o)) return(empty_chart(lang))
    o$family[is.na(o$family)] <- "?"
    if (!identical(input$taxo_mode, "sunburst")) return(taxo_tree(o, lang, spn()))
    o$class <- trv(o$class, "cls", lang)
    f <- count_by(o, class, family, binomial)
    e_sunburst_tree(relabel_leaves(sunburst_tree(f, c("class", "family", "binomial")), spn())) |>
      echarts4r::e_on(list(seriesType = "sunburst"), "function(p){ Shiny.setInputValue('sunburst_click', (p.data && p.data.key) || p.name, {priority:'event'}); }")
  })

  years_chart <- function(o, lang, years = NULL) {
    o <- o[!is.na(o$year), ]; if (!nrow(o)) return(empty_chart(lang))
    f <- count_by(o, year, source)
    yr <- years %||% seq(min(f$year), max(f$year))
    f <- tidyr::complete(f, year = yr, source, fill = list(n = 0))
    f$src <- src_label(f$source, lang); f$year <- as.character(f$year)
    f |> dplyr::group_by(src) |> echarts4r::e_charts(year, reorder = FALSE) |>
      echarts4r::e_bar(n, stack = "s", barMaxWidth = 28, itemStyle = list(borderColor = COL$surface, borderWidth = 1, borderRadius = 2)) |>
      e_bigal() |> echarts4r::e_color(src_colors(f)) |> e_tip(trigger = "axis") |> cat_axis(rotate = if (length(yr) > 12) 45 else 0) |> src_legend(f)
  }
  output$c_years <- echarts4r::renderEcharts4r(years_chart(obs(), l()))

  output$c_top <- echarts4r::renderEcharts4r({
    o <- obs(); lang <- l(); o <- o[!is.na(o$binomial), ]
    if (!nrow(o)) return(empty_chart(lang))
    sp <- top_species(o, 15)
    f <- count_by(o[o$binomial %in% sp, ], binomial, source)
    f <- tidyr::complete(f, binomial, source, fill = list(n = 0))
    nm <- spn(); f$binomial <- factor(nm(f$binomial), levels = nm(rev(sp))); f <- f[order(f$binomial), ]
    f$src <- src_label(f$source, lang)
    f |> dplyr::group_by(src) |> echarts4r::e_charts(binomial, reorder = FALSE) |>
      echarts4r::e_bar(n, stack = "s", barMaxWidth = 16, itemStyle = list(borderColor = COL$surface, borderWidth = 1)) |>
      echarts4r::e_flip_coords() |> e_bigal() |> echarts4r::e_color(src_colors(f)) |> e_tip(trigger = "axis") |>
      echarts4r::e_y_axis(axisLabel = italic_axis, axisTick = list(show = FALSE)) |> src_legend(f)
  })

  # ---------------- Species profile ----------------
  shiny::observe({
    o <- obs(); tb <- sort(table(o$binomial), decreasing = TRUE)
    ch <- names(tb); names(ch) <- sprintf("%s (%d)", spn()(names(tb)), as.integer(tb))
    sel <- shiny::isolate(input$sp); if (is.null(sel) || !sel %in% ch) sel <- if ("Panthera onca" %in% ch) "Panthera onca" else ch[1]
    shiny::updateSelectizeInput(session, "sp", choices = ch, selected = sel, server = TRUE)
  })
  sp_obs <- shiny::reactive({ shiny::req(input$sp); o <- obs(); o[!is.na(o$binomial) & o$binomial == input$sp, ] })

  output$sp_id <- shiny::renderUI({
    o <- sp_obs(); lang <- l(); if (!nrow(o)) return(NULL)
    common <- names(sort(table(o$common_es), decreasing = TRUE))[1]
    htmltools::tagList(
      sp_avatar(input$sp, o$class[1], size = 92, ring = COL$ochre, family = o$family[1]),
      htmltools::div(htmltools::h3(htmltools::tags$i(input$sp), iucn_pill(o$iucn[1], lang)),
                     htmltools::p(class = "sp-common", sp_common(input$sp, common %||% NA, lang)),
                     htmltools::p(class = "sp-tax", paste(stats::na.omit(c(o$family[1], trv(o$class[1], "cls", lang))), collapse = " · "))))
  })
  output$sp_stats <- shiny::renderUI({
    o <- sp_obs(); lang <- l(); if (!nrow(o)) return(NULL)
    bysrc <- table(o$source)
    htmltools::div(class = "sp-stats",
      lapply(names(bysrc), function(s) htmltools::div(class = "sp-stat",
        html_icon(SOURCE_ICON[[s]], SOURCE_COL[[s]], "1.1em"), htmltools::tags$b(bysrc[[s]]), htmltools::span(src_label(s, lang)))),
      htmltools::div(class = "sp-stat", bsicons::bs_icon("calendar3"),
                     htmltools::span(paste(format(range(o$date, na.rm = TRUE), "%m/%Y"), collapse = " → "))))
  })

  output$sp_map <- leaflet::renderLeaflet({
    o <- sp_obs(); o <- o[!is.na(o$lat), ]; b <- db()$balises
    m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |> leaflet::addProviderTiles("Esri.WorldImagery")
    for (ln in trail_lines(b)) m <- leaflet::addPolylines(m, ln$lon, ln$lat, color = "#FFFFFF", weight = 1.2, opacity = 0.45)
    if (nrow(o)) m <- leaflet.extras::addHeatmap(m, o$lon, o$lat, intensity = 1, radius = 16, blur = 20, max = 0.5, minOpacity = 0.3,
                                                 gradient = HEAT_GRADIENT)
    leaflet::fitBounds(m, min(b$lon, na.rm = TRUE), min(b$lat, na.rm = TRUE), max(b$lon, na.rm = TRUE), max(b$lat, na.rm = TRUE))
  })

  output$c_sp_years <- echarts4r::renderEcharts4r(
    years_chart(sp_obs(), l(), seq(min(obs()$year, na.rm = TRUE), max(obs()$year, na.rm = TRUE))))
  output$c_sp_clock <- echarts4r::renderEcharts4r(clock_chart(sp_obs()$hour, l()))

  moon_bar <- function(moon, lang) {
    moon <- moon[!is.na(moon)]
    if (length(moon) < 3) return(empty_chart(lang))
    nm <- trv(MOON_ES, "moon", lang)
    e_raw(list(
      grid = list(left = 40, right = 16, top = 30, bottom = 8, containLabel = TRUE),
      xAxis = list(type = "category", data = nm, axisTick = list(show = FALSE), axisLabel = moon_axis_label(nm)),
      yAxis = list(type = "value", name = "%", splitLine = list(lineStyle = list(color = COL$line))),
      # bars take the moon's brightness: slate for new moon, faint yellow for full moon (ochre outline keeps them visible)
      series = list(list(type = "bar", name = "%", barMaxWidth = 22,
                         data = lapply(seq_along(MOON_ES), function(i) list(value = as.numeric(prop.table(table(moon)))[i] * 100,
                                                                             itemStyle = list(color = MOON_LIT_COL[i]))),
                         itemStyle = list(borderRadius = c(4, 4, 0, 0), borderColor = "#C8A24A", borderWidth = 1),
                         tooltip = list(valueFormatter = htmlwidgets::JS("v => v.toFixed(1) + ' %'")),
                         markLine = list(symbol = "none", silent = TRUE, data = list(list(yAxis = 12.5)),
                                         label = list(formatter = tr("expected", lang), position = "insideEndTop", color = COL$muted, fontSize = 10),
                                         lineStyle = list(color = COL$ochre, type = "dashed"))))))
  }
  output$c_sp_moon <- echarts4r::renderEcharts4r(moon_bar(sp_obs()$moon, l()))

  # Site types as bubble clusters (icon badge), cameras inside sized by this species' photos
  output$c_sp_site <- echarts4r::renderEcharts4r({
    shiny::req(input$sp); lang <- l(); p <- src()$photos; p <- p[!is.na(p$binomial) & p$binomial == input$sp & !is.na(p$site), ]
    if (!nrow(p)) return(empty_chart(lang))
    f <- count_by(p, site, camera); names(f) <- c("clu", "grp", "n")
    pack_chart(f, lang, clu_icons = SITE_ICON, clu_prefix = "site",
               grp_icons = stats::setNames(rep("camera", length(unique(f$grp))), unique(f$grp)), top_n = 7)
  })
  output$sp_weather <- shiny::renderUI({
    o <- sp_obs(); shiny::req(nrow(o))
    base <- obs(); base <- base[base$source %in% unique(o$source), ]
    weather_card(o, base, l())
  })

  output$sp_where <- reactable::renderReactable({
    o <- sp_obs(); lang <- l(); shiny::req(nrow(o) > 0)
    o$place <- ifelse(is.na(o$station), o$trail, o$station)
    f <- count_by(o[!is.na(o$place), ], source, place)
    f$source <- src_label(f$source, lang)
    names(f) <- c(tr("source", lang), tr("place", lang), tr("records", lang))
    reactable::reactable(f[order(-f[[3]]), ], compact = TRUE, striped = TRUE, defaultPageSize = 8, searchable = TRUE, language = rt_lang(lang))
  })

  # ---------------- Camera traps ----------------
  ph_events <- shiny::reactive({
    p <- src()$photos; p <- p[!is.na(p$binomial), ]
    p[!duplicated(p$event), ]  # one row per independent event
  })

  output$c_rai <- echarts4r::renderEcharts4r({
    p <- ph_events(); lang <- l(); if (!nrow(p)) return(empty_chart(lang))
    sp <- top_species(p, 20); cams <- names(sort(table(p$camera), decreasing = TRUE))
    cams <- cams[cams %in% names(which(table(p$camera) >= 5))]
    f <- count_by(p[p$binomial %in% sp & p$camera %in% cams, ], camera, binomial)
    f <- tidyr::complete(f, camera = cams, binomial = rev(sp), fill = list(n = 0))
    nm <- spn(); f$camera <- factor(f$camera, cams); f$binomial <- factor(nm(f$binomial), nm(rev(sp))); f <- f[order(f$camera, f$binomial), ]
    f |> echarts4r::e_charts(camera, reorder = FALSE) |> echarts4r::e_heatmap(binomial, n, name = tr("events", lang),
                                                                             itemStyle = list(borderColor = COL$surface, borderWidth = 2)) |>
      echarts4r::e_visual_map(n, inRange = list(color = heat_colors), orient = "vertical", right = 0, top = "middle", itemHeight = 110, itemWidth = 10,
                              textStyle = list(color = COL$muted)) |>
      e_bigal(legend = FALSE) |> grid_set(right = 72, bottom = 16) |> cat_axis(rotate = 45) |>
      echarts4r::e_y_axis(axisLabel = italic_axis)
  })

  shiny::observe({
    ch <- top_species(ph_events(), 40); ch <- stats::setNames(ch, spn()(ch))
    a <- shiny::isolate(input$ov_a); b <- shiny::isolate(input$ov_b)
    shiny::updateSelectizeInput(session, "ov_a", choices = ch, selected = if (!is.null(a) && a %in% ch) a else unname(intersect(c("Panthera onca", ch[1]), ch)[1]))
    shiny::updateSelectizeInput(session, "ov_b", choices = ch, selected = if (!is.null(b) && b %in% ch) b else unname(intersect(c("Tayassu pecari", ch[2]), ch)[1]))
  })

  ov <- shiny::reactive({
    shiny::req(input$ov_a, input$ov_b)
    p <- ph_events()
    list(a = p$hour[p$binomial == input$ov_a & !is.na(p$hour)], b = p$hour[p$binomial == input$ov_b & !is.na(p$hour)])
  })
  output$c_overlap <- echarts4r::renderEcharts4r({
    x <- ov(); lang <- l()
    da <- diel_density(x$a); db_ <- diel_density(x$b)
    if (is.null(da) || is.null(db_)) return(empty_chart(lang))
    f <- data.frame(hour = da$hour, a = da$dens, b = db_$dens, both = pmin(da$dens, db_$dens))
    e <- f |> echarts4r::e_charts(hour) |>
      echarts4r::e_area(both, name = tr("overlap", lang), symbol = "none", smooth = TRUE, lineStyle = list(width = 0),
                        areaStyle = list(opacity = 0.35)) |>
      echarts4r::e_line(a, name = spn()(input$ov_a), symbol = "none", smooth = TRUE, lineStyle = list(width = 2.5)) |>
      echarts4r::e_line(b, name = spn()(input$ov_b), symbol = "none", smooth = TRUE, lineStyle = list(width = 2.5)) |>
      e_bigal() |> echarts4r::e_color(c(COL$sage, PAL[1], PAL[2])) |>
      e_tip(trigger = "axis", valueFormatter = htmlwidgets::JS("v => v.toFixed(3)")) |>
      echarts4r::e_x_axis(type = "value", min = 0, max = 24, interval = 3, name = tr("hour", lang), nameLocation = "end") |>
      echarts4r::e_y_axis(nameTextStyle = list(align = "left"), name = tr("density", lang))
    # night / twilight shading behind the curves
    e$x$opts$series[[1]]$markArea <- list(silent = TRUE, data = list(
      list(list(xAxis = 0, itemStyle = list(color = "rgba(43,58,103,0.08)")), list(xAxis = 5)),
      list(list(xAxis = 5, itemStyle = list(color = "rgba(217,130,43,0.10)")), list(xAxis = 6)),
      list(list(xAxis = 18, itemStyle = list(color = "rgba(217,130,43,0.10)")), list(xAxis = 19)),
      list(list(xAxis = 19, itemStyle = list(color = "rgba(43,58,103,0.08)")), list(xAxis = 24))))
    e
  })
  output$ov_stat <- shiny::renderUI({
    x <- ov(); lang <- l()
    if (length(x$a) < 5 || length(x$b) < 5) return(htmltools::p(class = "text-muted small", tr("too_few", lang)))
    ra <- x$a / 24 * 2 * pi; rb <- x$b / 24 * 2 * pi
    d <- overlap::overlapEst(ra, rb, type = if (min(length(ra), length(rb)) < 75) "Dhat1" else "Dhat4")
    htmltools::div(class = "ov-stat",
                   htmltools::div(class = "ov-avs", sp_avatar(input$ov_a, "Mamífero", 44, PAL[1]), sp_avatar(input$ov_b, "Mamífero", 44, PAL[2])),
                   htmltools::div(class = "ov-num", sprintf("Δ = %.2f", d)),
                   htmltools::p(class = "small text-muted", sprintf(tr("ov_n", lang), length(ra), length(rb))),
                   htmltools::p(class = "small", tr(if (d > 0.75) "ov_high" else if (d > 0.5) "ov_mid" else "ov_low", lang)))
  })

  # All-pairs activity overlap (Δ) of the 10 most photographed species; click a cell to open that pair below
  output$c_ov_matrix <- echarts4r::renderEcharts4r({
    p <- ph_events(); lang <- l(); nm <- spn(); p <- p[!is.na(p$hour), ]
    n_sp <- table(p$binomial); sp <- names(utils::head(sort(n_sp[n_sp >= 15], decreasing = TRUE), 10))
    if (length(sp) < 3) return(empty_chart(lang))
    rad <- lapply(stats::setNames(sp, sp), function(s) p$hour[p$binomial == s] / 24 * 2 * pi)
    cells <- list(); info <- character(); pairs <- character()
    for (i in seq_along(sp)) for (j in seq_along(sp)) if (i > j) {
      a <- rad[[sp[i]]]; b <- rad[[sp[j]]]
      d <- unname(overlap::overlapEst(a, b, type = if (min(length(a), length(b)) < 75) "Dhat1" else "Dhat4"))
      cells[[length(cells) + 1]] <- c(j - 1, i - 2, round(d, 2))  # triangle: x = first K-1 species, y = last K-1
      info <- c(info, sprintf("<i>%s</i> × <i>%s</i><br>Δ = <b>%.2f</b>", nm(sp[i]), nm(sp[j]), d)); pairs <- c(pairs, paste(sp[i], sp[j], sep = "|"))
    }
    img <- sp_img(sp)
    rich <- stats::setNames(lapply(seq_along(sp), function(k) if (is.na(img[k])) list(width = 0) else
      list(width = 22, height = 22, borderRadius = 11, backgroundColor = list(image = img[k]))), paste0("s", seq_along(sp)))
    rich$t <- list(fontStyle = "italic", fontSize = 10, color = COL$muted)
    fmt <- function(with_name, off = 0) htmlwidgets::JS(sprintf("function(v, i){ return %s'{s' + (i + 1 + %d) + '|}'; }",
                                                       if (with_name) "'{t|' + v + '} ' + " else "", off))
    e <- e_raw(list(
      grid = list(left = 8, right = 72, top = 16, bottom = 8, containLabel = TRUE),
      xAxis = list(type = "category", data = nm(sp[-length(sp)]), axisTick = list(show = FALSE), splitArea = list(show = FALSE),
                   axisLabel = list(interval = 0, rich = rich, formatter = fmt(FALSE))),
      yAxis = list(type = "category", data = nm(sp[-1]), axisTick = list(show = FALSE), axisLabel = list(interval = 0, rich = rich, formatter = fmt(TRUE, 1))),
      visualMap = list(min = 0, max = 1, calculable = TRUE, orient = "vertical", right = 0, top = "middle", itemHeight = 110, itemWidth = 10,
                       precision = 2, inRange = list(color = unname(HEAT_GRADIENT)), textStyle = list(color = COL$muted)),
      tooltip = list(formatter = htmlwidgets::JS(sprintf("function(p){ return %s[p.dataIndex] + '<br><small>%s</small>'; }", js_array(info), tr("click_pair", lang)))),
      series = list(list(type = "heatmap", data = cells, itemStyle = list(borderColor = COL$surface, borderWidth = 2),
                         label = list(show = TRUE, fontSize = 9, color = "#fff", formatter = htmlwidgets::JS("function(p){ return p.value[2].toFixed(2); }"))))))
    on_click(e, sprintf("function(p){ var q = %s[p.dataIndex]; if (q) Shiny.setInputValue('ov_pair', q, {priority: 'event'}); }", js_array(pairs)))
  })
  shiny::observeEvent(input$ov_pair, {
    s <- strsplit(input$ov_pair, "|", fixed = TRUE)[[1]]
    shiny::updateSelectizeInput(session, "ov_a", selected = s[1]); shiny::updateSelectizeInput(session, "ov_b", selected = s[2])
  })

  output$c_cam_moon <- echarts4r::renderEcharts4r({
    p <- ph_events(); lang <- l(); p <- p[!is.na(p$date), ]; if (!nrow(p)) return(empty_chart(lang))
    p$moon <- moon_phase(p$date)
    sp <- top_species(p, 12); nm <- trv(MOON_ES, "moon", lang)
    tb <- prop.table(table(factor(p$binomial[p$binomial %in% sp], sp), p$moon[p$binomial %in% sp]), 1) * 100
    cells <- expand.grid(i = seq_along(nm) - 1, j = seq_along(sp) - 1)
    cells$v <- mapply(function(i, j) round(tb[j + 1, i + 1], 1), cells$i, cells$j)
    e_raw(list(
      grid = list(left = 8, right = 72, top = 16, bottom = 8, containLabel = TRUE),
      xAxis = list(type = "category", data = nm, axisTick = list(show = FALSE), axisLabel = moon_axis_label(nm), splitArea = list(show = FALSE)),
      yAxis = list(type = "category", data = spn()(rev(sp)), axisLabel = italic_axis, axisTick = list(show = FALSE)),
      visualMap = list(min = 0, max = max(cells$v, 1), calculable = TRUE, orient = "vertical", right = 0, top = "middle", itemHeight = 110,
                       itemWidth = 10, inRange = list(color = heat_colors), precision = 0, textStyle = list(color = COL$muted)),
      series = list(list(type = "heatmap", name = "%", itemStyle = list(borderColor = COL$surface, borderWidth = 2),
                         data = lapply(seq_len(nrow(cells)), function(k) c(cells$i[k], length(sp) - 1 - cells$j[k], cells$v[k])))),
      tooltip = list(formatter = htmlwidgets::JS(sprintf("function(p){ var s=%s, n=%s; return '<i>'+s[p.value[1]]+'</i><br>'+n[p.value[0]]+': <b>'+p.value[2]+' %%</b>'; }",
                                                         js_array(spn()(rev(sp))), js_array(nm))))))
  })

  output$c_cam_site <- echarts4r::renderEcharts4r(stack100(ph_events(), "site", l(), prefix = "site", nm = spn()))
  output$c_cam_age <- echarts4r::renderEcharts4r(stack100(ph_events(), "age", l(), 10, prefix = "age", nm = spn()))

  output$c_cam_temp <- echarts4r::renderEcharts4r({
    p <- ph_events(); lang <- l(); p <- p[!is.na(p$temp_c) & !is.na(p$hour) & p$temp_c > 10 & p$temp_c < 40, ]
    if (!nrow(p)) return(empty_chart(lang))
    e_raw(list(
      grid = list(left = 40, right = 48, top = 36, bottom = 16, containLabel = TRUE),
      xAxis = list(type = "value", min = 0, max = 24, interval = 3, name = tr("hour", lang), splitLine = list(lineStyle = list(color = COL$line))),
      yAxis = list(type = "value", min = "dataMin", name = "°C", splitLine = list(lineStyle = list(color = COL$line))),
      tooltip = list(formatter = htmlwidgets::JS("function(p){ var h = Math.floor(p.value[0]), m = Math.round((p.value[0]-h)*60); return (h<10?'0':'')+h+':'+(m<10?'0':'')+m+' · <b>'+p.value[1]+' °C</b>'; }")),
      series = list(list(type = "scatter", symbolSize = 5, itemStyle = list(color = PAL[3], opacity = 0.35),
                         data = unname(Map(function(a, b) c(round(a, 2), b), p$hour, p$temp_c))))))
  })

  output$c_cam_effort <- echarts4r::renderEcharts4r({
    p <- src()$photos; lang <- l(); p <- p[!is.na(p$ym), ]; if (!nrow(p)) return(empty_chart(lang))
    cams <- names(sort(table(p$camera), decreasing = TRUE)); cams <- cams[cams %in% names(which(table(p$camera) >= 5))]
    f <- count_by(p[p$camera %in% cams, ], ym, camera)
    months <- seq(min(f$ym), max(f$ym), by = "month")
    f <- tidyr::complete(f, ym = months, camera = cams, fill = list(n = 0))
    f$m <- format(f$ym, "%Y-%m"); f$camera <- factor(f$camera, rev(cams)); f$n[f$n == 0] <- NA
    f <- f[order(f$ym, f$camera), ]
    f |> echarts4r::e_charts(m, reorder = FALSE) |> echarts4r::e_heatmap(camera, n, name = tr("photos", lang)) |>
      echarts4r::e_visual_map(n, inRange = list(color = heat_colors[-1]), orient = "vertical", right = 0, top = "middle", itemHeight = 110, itemWidth = 10,
                              textStyle = list(color = COL$muted)) |>
      e_bigal(legend = FALSE) |> grid_set(right = 72, bottom = 16) |>
      echarts4r::e_x_axis(axisLabel = list(interval = 5, rotate = 45), splitArea = list(show = FALSE))
  })

  # ---------------- Primates ----------------
  mk <- shiny::reactive({ m <- src()$monkeys; m[!is.na(m$binomial), ] })
  output$c_pr_rate <- echarts4r::renderEcharts4r({
    m <- mk(); lang <- l(); m <- m[!is.na(m$year), ]; if (!nrow(m)) return(empty_chart(lang))
    days <- tapply(m$date, m$year, function(d) length(unique(d)))
    sp <- top_species(m, 6)
    f <- count_by(m[m$binomial %in% sp, ], year, binomial)
    f <- tidyr::complete(f, year = as.integer(names(days)), binomial = sp, fill = list(n = 0))
    f$rate <- f$n / days[as.character(f$year)]; f$year <- as.character(f$year); f$lab <- spn()(f$binomial)
    e <- f |> dplyr::group_by(lab) |> echarts4r::e_charts(year) |>
      echarts4r::e_line(rate, symbol = "circle", symbolSize = 8, lineStyle = list(width = 2)) |> e_bigal() |>
      e_tip(trigger = "axis", valueFormatter = htmlwidgets::JS("v => v.toFixed(2)")) |>
      echarts4r::e_y_axis(nameTextStyle = list(align = "left"), name = tr("per_day", lang))
    # legend: each species' photo as its marker
    e$x$opts$legend$data <- lapply(sort(unique(f$lab)), function(b) { img <- sp_img(f$binomial[match(b, f$lab)])
      list(name = b, icon = if (is.na(img)) e_icon("monkey") else paste0("image://", img)) })
    e$x$opts$legend$itemWidth <- 18; e$x$opts$legend$itemHeight <- 18
    e$x$opts$legend$textStyle$fontStyle <- "italic"
    e
  })

  # Pictogram: one monkey per individual of the median group; faint bar = largest group seen
  output$c_pr_group <- echarts4r::renderEcharts4r({
    m <- mk(); lang <- l(); m <- m[!is.na(m$group) & m$group > 0, ]; if (nrow(m) < 3) return(empty_chart(lang))
    sp <- rev(top_species(m, 7))
    st <- t(vapply(sp, function(b) { g <- m$group[m$binomial == b]; c(stats::median(g), min(g), max(g), length(g)) }, numeric(4)))
    o <- order(st[, 1], st[, 3]); sp <- sp[o]; st <- st[o, , drop = FALSE]  # category axis runs bottom-up: largest groups on top
    info <- sprintf("<i>%s</i><br>%s: <b>%s</b> · %s %s–%s · n = %d", spn()(sp), tr("median", lang), st[, 1], tr("range", lang), st[, 2], st[, 3], st[, 4])
    e_raw(list(
      grid = list(left = 8, right = 24, top = 30, bottom = 8, containLabel = TRUE),
      xAxis = list(type = "value", min = 0, name = tr("individuals", lang), nameLocation = "middle", nameGap = 24, splitLine = list(lineStyle = list(color = COL$line)), minInterval = 1),
      yAxis = list(type = "category", data = spn()(sp), axisLabel = italic_axis, axisTick = list(show = FALSE)),
      series = list(pictogram_series(lapply(seq_along(sp), function(k) c(k - 1, max(1, round(st[k, 1])), 1, st[k, 3])),
                                     "monkey", PAL[2], tr("individuals", lang), horizontal = TRUE, size = 20, info = info))))
  })

  output$c_pr_height <- echarts4r::renderEcharts4r({
    m <- mk(); lang <- l(); m <- m[!is.na(m$height) & m$height <= 60, ]
    sp <- top_species(m, 7); m <- m[m$binomial %in% sp, ]
    violin_chart(m$binomial, m$height, lang, silhouettes = stats::setNames(rep("tree", length(sp)), sp), italic = TRUE, unit = "m", nm = spn())
  })
  output$c_pr_activity <- echarts4r::renderEcharts4r(stack100(mk(), "activity", l(), 7, prefix = "act", icons = ACTIVITY_ICON, nm = spn()))
  output$c_pr_dist <- echarts4r::renderEcharts4r({
    m <- mk(); lang <- l(); m <- m[!is.na(m$dist) & m$dist <= 100, ]; d <- m$dist
    if (length(d) < 3) return(empty_chart(lang))
    br <- seq(0, 100, by = 5); bins <- sprintf("%d–%d", br[-length(br)], br[-1])
    if (isTRUE(input$pr_dist_sp)) {  # stacked by species (top 5 + other), legend with species photos
      sp <- top_species(m, 5)
      keys <- c(sp, tr("other", lang)); labs <- c(spn()(sp), tr("other", lang))
      m$grp <- factor(labs[match(ifelse(m$binomial %in% sp, m$binomial, tr("other", lang)), keys)], labs)
      m$bin <- factor(bins[as.integer(cut(m$dist, br, include.lowest = TRUE))], bins)
      f <- as.data.frame(table(bin = m$bin, grp = m$grp))
      e <- f |> dplyr::group_by(grp) |> echarts4r::e_charts(bin, reorder = FALSE) |>
        echarts4r::e_bar(Freq, stack = "s", barCategoryGap = "8%", itemStyle = list(borderColor = COL$surface, borderWidth = 0.5)) |>
        e_bigal() |> echarts4r::e_color(c(PAL[seq_along(sp)], OTHER_COL)) |> e_tip(trigger = "axis") |> cat_axis(rotate = 45)
      e$x$opts$legend$data <- lapply(seq_along(labs), function(i) { b <- labs[i]; img <- sp_img(keys[i])
        list(name = b, icon = if (is.na(img)) "circle" else paste0("image://", img)) })
      e$x$opts$legend$itemWidth <- 18; e$x$opts$legend$itemHeight <- 18; e$x$opts$legend$textStyle$fontStyle <- "italic"
      return(e)
    }
    f <- data.frame(bin = bins, n = as.integer(table(cut(d, br, include.lowest = TRUE))))
    f |> echarts4r::e_charts(bin) |> echarts4r::e_bar(n, name = tr("records", lang), barCategoryGap = "8%", itemStyle = list(borderRadius = c(3, 3, 0, 0))) |>
      e_bigal(legend = FALSE) |> echarts4r::e_color(PAL[2]) |> cat_axis(rotate = 45) |>
      echarts4r::e_x_axis(name = "{i|} m", nameLocation = "end",
                          nameTextStyle = list(rich = list(i = list(width = 16, height = 16, backgroundColor = list(image = icon_uri("binoculars", PAL[2]))))))
  })
  output$c_pr_trail <- echarts4r::renderEcharts4r({
    m <- mk(); lang <- l(); m <- m[!is.na(m$trail), ]; if (!nrow(m)) return(empty_chart(lang))
    f <- count_by(m, trail); f <- f[order(f$n), ]
    f |> echarts4r::e_charts(trail) |> echarts4r::e_bar(n, name = tr("records", lang), barMaxWidth = 16, itemStyle = list(borderRadius = c(0, 4, 4, 0))) |>
      echarts4r::e_flip_coords() |> e_bigal(legend = FALSE) |> echarts4r::e_color(PAL[2])
  })

  # ---------------- Herpetofauna ----------------
  hp <- shiny::reactive(src()$herps)
  # Pictogram: frogs / lizards per night transect and year
  he_rate <- shiny::reactive({
    h <- hp(); h <- h[!is.na(h$year), ]; if (!nrow(h)) return(NULL)
    nights <- tapply(paste(h$date, h$trail_raw), h$year, function(x) length(unique(x)))
    f <- count_by(h, year, class)
    f <- tidyr::complete(f, year = as.integer(names(nights)), class = c("Anfibio", "Reptil"), fill = list(n = 0))
    f$rate <- f$n / nights[as.character(f$year)]
    list(f = f, unit = max(1, round(max(f$rate) / 12)))
  })
  output$c_he_rate <- echarts4r::renderEcharts4r({
    r <- he_rate(); lang <- l(); if (is.null(r)) return(empty_chart(lang))
    f <- r$f; yrs <- as.character(sort(unique(f$year)))
    ser <- function(cls, icon, col, off) {
      v <- f$rate[f$class == cls][match(yrs, f$year[f$class == cls])]; v[is.na(v)] <- 0
      info <- sprintf("%s · %s<br><b>%.1f</b> %s", yrs, trv(cls, "cls", lang), v, tr("per_night", lang))
      pictogram_series(lapply(seq_along(yrs), function(i) c(i - 1, v[i], r$unit, 0)), icon, col, trv(cls, "cls", lang),
                       offset = off, size = 14, info = info)
    }
    e_raw(list(
      grid = list(left = 40, right = 16, top = 36, bottom = 44, containLabel = TRUE),
      legend = list(bottom = 0, textStyle = list(color = COL$muted), itemWidth = 16, itemHeight = 14,
                    data = list(list(name = trv("Anfibio", "cls", lang), icon = e_icon("frog")), list(name = trv("Reptil", "cls", lang), icon = e_icon("lizard")))),
      xAxis = list(type = "category", data = yrs, axisTick = list(show = FALSE)),
      yAxis = list(type = "value", min = 0, name = tr("per_night", lang), nameTextStyle = list(align = "left"), splitLine = list(lineStyle = list(color = COL$line))),
      series = list(ser("Anfibio", "frog", PAL[3], -0.2), ser("Reptil", "lizard", PAL[8], 0.2))))
  })
  output$he_rate_note <- shiny::renderUI({ r <- he_rate(); if (is.null(r)) return(NULL)
    htmltools::span(class = "small text-muted", html_icon("frog", PAL[3]), " / ", html_icon("lizard", PAL[8]), " = ",
                    sprintf(tr("he_rate_note", l()), r$unit)) })

  output$c_he_river <- echarts4r::renderEcharts4r({
    h <- hp(); lang <- l(); h <- h[!is.na(h$year) & !is.na(h$family), ]; if (!nrow(h)) return(empty_chart(lang))
    fam <- names(utils::head(sort(table(h$family), decreasing = TRUE), 7))
    h$fam <- ifelse(h$family %in% fam, h$family, tr("other", lang))
    lv <- c(fam, tr("other", lang))
    f <- as.data.frame(prop.table(table(year = h$year, fam = factor(h$fam, lv)), 1) * 100)
    e <- f |> dplyr::group_by(fam) |> echarts4r::e_charts(year, reorder = FALSE) |>
      echarts4r::e_bar(Freq, stack = "s", barMaxWidth = 26, itemStyle = list(borderColor = COL$surface, borderWidth = 1)) |>
      e_bigal() |> echarts4r::e_color(c(PAL[seq_along(fam)], OTHER_COL)) |>
      e_tip(trigger = "axis", valueFormatter = htmlwidgets::JS("v => v.toFixed(0) + ' %'")) |>
      echarts4r::e_y_axis(max = 100) |> cat_axis()
    e$x$opts$legend$data <- lapply(seq_along(lv), function(i) list(name = lv[i], icon = e_icon(if (i <= length(fam)) herp_icon(fam[i]) else "circle")))
    e$x$opts$legend$itemWidth <- 14; e$x$opts$legend$itemHeight <- 14
    e
  })
  # ---- interactive herp charts: grouping (species / family / order) + click-to-isolate ----
  iso <- shiny::reactiveValues(sub = NULL, h = NULL, lt = NULL)
  for (k in c("sub", "h", "lt")) local({ k <- k
    shiny::observeEvent(input[[paste0("he_mod_", k)]], { iso[[k]] <- NULL }, ignoreInit = TRUE)       # new grouping: show all
    shiny::observeEvent(input[[paste0("he_iso_", k, "_reset")]], { iso[[k]] <- NULL })
    output[[paste0("he_iso_", k)]] <- shiny::renderUI({
      v <- iso[[k]]; if (is.null(v)) return(NULL)
      lab <- if (startsWith(v, "clu|")) trv(sub("^clu\\|", "", v), "sub", l()) else spn()(sub("^grp\\|", "", v))
      htmltools::tags$button(class = "iso-chip", onclick = sprintf("Shiny.setInputValue('he_iso_%s_reset', Date.now())", k),
                             title = tr("show_all", l()), lab, bsicons::bs_icon("x-lg"))
    })
  })
  shiny::observeEvent(input$he_sub_click, { iso$sub <- input$he_sub_click })
  shiny::observeEvent(input$he_h_click, { iso$h <- paste0("grp|", input$he_h_click) })
  shiny::observeEvent(input$he_lt_click, { iso$lt <- paste0("grp|", input$he_lt_click) })
  herp_grouped <- function(h, mod, iso_v) {
    h$grp <- h[[mod %||% "family"]]
    h <- h[!is.na(h$grp), ]
    if (!is.null(iso_v) && startsWith(iso_v, "grp|")) h <- h[h$grp == sub("^grp\\|", "", iso_v), ]
    h
  }

  output$c_he_substrate <- echarts4r::renderEcharts4r({
    lang <- l(); mod <- input$he_mod_sub %||% "family"
    h <- herp_grouped(hp(), mod, iso$sub); h <- h[!is.na(h$substrate), ]
    if (!is.null(iso$sub) && startsWith(iso$sub, "clu|")) h <- h[h$substrate == sub("^clu\\|", "", iso$sub), ]
    if (!nrow(h)) return(empty_chart(lang))
    f <- count_by(h, substrate, grp); names(f) <- c("clu", "grp", "n")
    pack_chart(f, lang, grp_icons = herp_group_icons(h, mod), italic = mod == "binomial", click_input = "he_sub_click", nm = spn())
  })
  output$c_he_snakes <- echarts4r::renderEcharts4r({
    h <- hp(); lang <- l()
    s <- h[grepl("serpentes", tolower(h$suborder)) | h$family %in% c("Colubridae", "Viperidae", "Elapidae", "Boidae"), ]
    if (!nrow(s)) return(empty_chart(lang))
    s$ven <- ifelse(s$family %in% c("Viperidae", "Elapidae"), tr("venomous", lang), tr("non_venomous", lang))
    s$binomial[is.na(s$binomial)] <- "?"
    f <- count_by(s, ven, binomial)
    # colour follows the category: venomous = terracotta with a warning sign, non-venomous = green
    tree <- relabel_leaves(sunburst_tree(f, c("ven", "binomial")), spn())
    for (i in seq_along(tree)) {
      ven <- tree[[i]]$name == tr("venomous", lang)
      tree[[i]]$itemStyle <- list(color = if (ven) PAL[8] else PAL[1])
      tree[[i]]$label <- list(rotate = 0, formatter = if (ven) "{w|}\n{b}" else "{s|}\n{b}", rich = list(
        w = list(width = 18, height = 18, backgroundColor = list(image = icon_uri("triangle-exclamation", "#FFFFFF"))),
        s = list(width = 18, height = 18, backgroundColor = list(image = icon_uri("snake", "#FFFFFF")))))
    }
    e_sunburst_tree(tree, radius = c("15%", "95%"))
  })
  herp_violins <- function(h, v, mod, lang, ...) {
    h <- h[!is.na(h[[v]]) & !(mod == "binomial" & grepl(" sp\\.$", h$grp)), ]
    top <- names(utils::head(sort(table(h$grp), decreasing = TRUE), 8)); h <- h[h$grp %in% top, ]
    violin_chart(h$grp, h[[v]], lang, italic = mod == "binomial", unit = "cm", nm = spn(), ...)
  }
  output$c_he_height <- echarts4r::renderEcharts4r({
    lang <- l(); mod <- input$he_mod_h %||% "family"
    h <- herp_grouped(hp(), mod, iso$h); h <- h[!is.na(h$height) & h$height < 1000, ]
    herp_violins(h, "height", mod, lang, icons = herp_group_icons(h, mod), click_input = "he_h_click")
  })
  output$c_he_size <- echarts4r::renderEcharts4r({
    lang <- l(); mod <- input$he_mod_lt %||% "binomial"
    h <- herp_grouped(hp(), mod, iso$lt); h <- h[!is.na(h$lt) & h$lt < 300, ]
    # silhouette height = median body length of the group (in axis units, standing on 0)
    herp_violins(h, "lt", mod, lang, silhouettes = herp_group_icons(h, mod, upright = TRUE), outlier_mult = 3, sil = "median",
                 click_input = "he_lt_click")
  })

  # Sex ratio butterfly: males left, females right, per species
  output$c_he_sex <- echarts4r::renderEcharts4r({
    h <- hp(); lang <- l(); h <- h[!is.na(h$sex) & !is.na(h$binomial) & !grepl(" sp\\.$", h$binomial), ]
    if (nrow(h) < 5) return(empty_chart(lang))
    sp <- rev(top_species(h, 8)); h <- h[h$binomial %in% sp, ]
    m <- as.integer(table(factor(h$binomial[h$sex == "Macho"], sp))); f <- as.integer(table(factor(h$binomial[h$sex == "Hembra"], sp)))
    info <- sprintf("<i>%s</i><br>♂ %d · ♀ %d · %s", spn()(sp), m, f, ifelse(f > 0, sprintf("%.1f ♂ : 1 ♀", m / pmax(f, 1)), "—"))
    e_raw(list(
      grid = list(left = 8, right = 24, top = 30, bottom = 44, containLabel = TRUE),
      legend = list(bottom = 0, itemWidth = 16, itemHeight = 16, textStyle = list(color = COL$muted),
                    data = list(list(name = tr("males", lang), icon = e_icon("mars")), list(name = tr("females", lang), icon = e_icon("venus")))),
      tooltip = list(formatter = htmlwidgets::JS(sprintf("function(p){ return %s[p.dataIndex]; }", js_array(info)))),
      xAxis = list(type = "value", min = -max(m, f), max = max(m, f), axisLabel = list(formatter = htmlwidgets::JS("function(v){ return Math.abs(v); }")),
                   splitLine = list(lineStyle = list(color = COL$line))),
      yAxis = list(type = "category", data = spn()(sp), axisTick = list(show = FALSE), axisLabel = italic_axis),
      series = list(
        list(type = "bar", name = tr("males", lang), stack = "s", data = as.list(-m), barMaxWidth = 18,
             itemStyle = list(color = PAL[3], borderRadius = c(9, 0, 0, 9)),
             label = list(show = TRUE, position = "left", fontSize = 10, color = COL$muted, formatter = htmlwidgets::JS("function(p){ return p.value ? Math.abs(p.value) : ''; }"))),
        list(type = "bar", name = tr("females", lang), stack = "s", data = as.list(f), barMaxWidth = 18,
             itemStyle = list(color = PAL[8], borderRadius = c(0, 9, 9, 0)),
             label = list(show = TRUE, position = "right", fontSize = 10, color = COL$muted, formatter = htmlwidgets::JS("function(p){ return p.value ? p.value : ''; }"))))))
  })
  output$he_sex_ratio <- shiny::renderUI({
    h <- hp(); m <- sum(h$sex == "Macho", na.rm = TRUE); f <- sum(h$sex == "Hembra", na.rm = TRUE); if (!f) return(NULL)
    htmltools::span(class = "ratio-chip", html_icon("mars", PAL[3]), sprintf(" %.1f : 1 ", m / f), html_icon("venus", PAL[8]))
  })

  # Juvenile recruitment calendar: share of juveniles per month (frog size = share)
  output$he_recruit <- shiny::renderUI({
    h <- hp(); lang <- l(); h <- h[!is.na(h$age) & h$age %in% c("Adulto", "Juvenil") & !is.na(h$date), ]
    if (!nrow(h)) return(htmltools::p(class = "text-muted", tr("no_data", lang)))
    mo <- as.integer(format(h$date, "%m"))
    n <- tabulate(mo, 12); j <- tabulate(mo[h$age == "Juvenil"], 12); pct <- ifelse(n > 0, j / n, NA)
    mn <- min(pct, na.rm = TRUE); mx <- max(pct, na.rm = TRUE); rel <- (pct - mn) / max(1e-9, mx - mn)  # stretch: monthly shares differ by a few points
    neo <- sum(hp()$age == "Neonato", na.rm = TRUE)
    htmltools::tagList(
      htmltools::div(class = "recruit", lapply(1:12, function(i) {
        sz <- if (is.na(pct[i])) 0 else 16 + 32 * rel[i]
        htmltools::div(class = "rc-tile", title = sprintf("%s: %d / %d %s", tr(sprintf("m%02d", i), lang), j[i], n[i], tr("records", lang)),
                       style = sprintf("background:rgba(31,127,168,%.2f)", if (is.na(pct[i])) 0 else 0.05 + 0.3 * rel[i]),
          htmltools::div(class = "rc-ico", if (sz > 0) html_icon("tadpole", PAL[3], sprintf("%.0fpx", sz))),
          htmltools::div(class = "rc-pct", if (is.na(pct[i])) "–" else sprintf("%.0f%%", 100 * pct[i])),
          htmltools::div(class = "rc-m", tr(sprintf("m%02d", i), lang)))
      })),
      htmltools::p(class = "small text-muted mt-2", sprintf(tr("recruit_note", lang), neo)))
  })

  # Herp nights by weather
  he_nights <- shiny::reactive(herp_nights(hp()))
  he_wx_tests <- shiny::reactive({ nt <- he_nights(); if (is.null(nt)) NULL else herp_wx_tests(nt) })
  output$c_he_wx <- echarts4r::renderEcharts4r({
    nt <- he_nights(); if (is.null(nt)) return(empty_chart(l()))
    herp_wx_chart(nt, l())
  })
  # discreet test line under the chart; the factor shown in the chart is emphasised
  output$he_wx_stats <- shiny::renderUI({
    r <- he_wx_tests(); lang <- l(); if (is.null(r)) return(NULL)
    best <- attr(r, "best")
    lab <- c(temp = tr("temperature", lang), hum = tr("humidity_short", lang), sky = tr("sky", lang))
    txt <- ifelse(r$var == "sky", sprintf("%s · Kruskal-Wallis %s", lab[r$var], p_label(r$p)),
                  sprintf("%s · Spearman r = %s, %s", lab[r$var], ifelse(is.na(r$rho), "–", sprintf("%+.2f", r$rho)), p_label(r$p)))
    htmltools::div(class = "wx-tests", htmltools::span(tr("wx_tests", lang)),
      lapply(seq_len(nrow(r)), function(i) htmltools::span(class = paste("wx-test", if (identical(r$var[i], best)) "best",
                                                                         if (isTRUE(r$p[i] < 0.05)) "sig"), txt[i])))
  })
  output$he_wx_strip <- shiny::renderUI({
    nt <- he_nights(); lang <- l(); if (is.null(nt)) return(NULL)
    nt <- nt[!is.na(nt$sky), ]; if (!nrow(nt)) return(NULL)
    avg <- tapply(nt$n, factor(nt$sky, SKY_LEVELS), mean); cnt <- table(factor(nt$sky, SKY_LEVELS)); mx <- max(avg, na.rm = TRUE)
    htmltools::div(class = "wx-strip", htmltools::div(class = "wxs-title", tr("per_night_by_sky", lang)),
      lapply(names(sort(avg[cnt > 0], decreasing = TRUE)), function(k) htmltools::div(class = "wxs-row",
        html_icon(SKY_ICON[[k]], SKY_COL[[k]], "1.6em"),
        htmltools::div(class = "wxs-body", htmltools::div(class = "wxs-lab", trv(k, "sky", lang), htmltools::tags$small(sprintf(" · %d %s", cnt[[k]], tr("nights", lang)))),
          htmltools::div(class = "wxs-bar", htmltools::span(style = sprintf("width:%.0f%%;background:%s", 100 * avg[[k]] / mx, SKY_COL[[k]])))),
        htmltools::tags$b(sprintf("%.1f", avg[[k]])))))
  })

  # ---------------- Tracks ----------------
  tk <- shiny::reactive({ t <- src()$tracks; t[!is.na(t$binomial), ] })
  glyph_axis <- function(sp, nm) {  # species axis labels (display names) followed by their track glyph
    g <- track_icon(sp); u <- unique(g)
    rich <- stats::setNames(lapply(u, function(i) list(width = 14, height = 14, backgroundColor = list(image = icon_uri(i, COL$muted)))), u)
    rich$t <- list(fontStyle = "italic", fontSize = 11, color = COL$muted, padding = c(0, 0, 0, 4))
    list(interval = 0, rich = rich, formatter = htmlwidgets::JS(sprintf("function(v){ var g=%s; return '{t|'+v+'} {'+(g[v]||'paw')+'|}'; }",
                                                                        jsonlite::toJSON(as.list(stats::setNames(g, nm(sp))), auto_unbox = TRUE))))
  }
  track_heat <- function(f, xcol, xlev, sp, lang) {
    f <- tidyr::complete(f, !!rlang::sym(xcol) := xlev, binomial = rev(sp), fill = list(n = 0))
    nm <- spn(); f[[xcol]] <- factor(f[[xcol]], xlev); f$binomial <- factor(nm(f$binomial), nm(rev(sp))); f <- f[order(f[[xcol]], f$binomial), ]
    f |> echarts4r::e_charts_(xcol, reorder = FALSE) |> echarts4r::e_heatmap(binomial, n, name = tr("records", lang),
                                                                            itemStyle = list(borderColor = COL$surface, borderWidth = 2)) |>
      echarts4r::e_visual_map(n, inRange = list(color = heat_colors), orient = "vertical", right = 0, top = "middle", itemHeight = 110, itemWidth = 10,
                              textStyle = list(color = COL$muted)) |>
      e_bigal(legend = FALSE) |> grid_set(right = 72, bottom = 16) |> cat_axis(rotate = 35) |>
      echarts4r::e_y_axis(axisLabel = glyph_axis(sp, nm))
  }
  output$c_tr_trail <- echarts4r::renderEcharts4r({
    t <- tk(); lang <- l(); t <- t[!is.na(t$trail), ]; if (!nrow(t)) return(empty_chart(lang))
    sp <- top_species(t, 12); trails <- names(sort(table(t$trail), decreasing = TRUE))
    track_heat(count_by(t[t$binomial %in% sp, ], trail, binomial), "trail", trails, sp, lang)
  })
  output$c_tr_season <- echarts4r::renderEcharts4r({
    t <- tk(); lang <- l(); t <- t[!is.na(t$date), ]; if (!nrow(t)) return(empty_chart(lang))
    months <- vapply(sprintf("m%02d", 1:12), tr, "", lang = lang)
    t$mo <- months[as.integer(format(t$date, "%m"))]
    sp <- top_species(t, 10)
    track_heat(count_by(t[t$binomial %in% sp, ], mo, binomial), "mo", unname(months), sp, lang) |> cat_axis()
  })
  output$c_tr_size <- echarts4r::renderEcharts4r({
    t <- tk(); lang <- l(); t <- t[!is.na(t$len) & !is.na(t$wid) & t$len < 40 & t$wid < 40, ]
    if (!nrow(t)) return(empty_chart(lang))
    sp <- top_species(t[!t$verify, ], 7)
    pts <- function(d) lapply(seq_len(nrow(d)), function(i) c(d$len[i], d$wid[i]))
    # faint prints + one large, outlined print at each species' centroid (mean length × width)
    ser <- lapply(seq_along(sp), function(k) { d <- t[!t$verify & t$binomial == sp[k], ]
      list(type = "scatter", name = spn()(sp[k]), symbol = e_icon(track_icon(sp[k])), symbolSize = 11,
           itemStyle = list(color = PAL[k], opacity = 0.25), data = pts(d),
           markPoint = list(symbol = e_icon(track_icon(sp[k])), symbolSize = 30,
                            itemStyle = list(color = PAL[k], opacity = 1, borderColor = COL$ink, borderWidth = 1.5),
                            label = list(show = FALSE),
                            data = list(list(name = tr("centroid", lang), coord = round(c(mean(d$len), mean(d$wid)), 1))))) })
    oth <- t[!t$verify & !t$binomial %in% sp, ]; ver <- t[t$verify, ]
    if (nrow(oth)) ser <- c(ser, list(list(type = "scatter", name = tr("other", lang), symbolSize = 7, itemStyle = list(color = OTHER_COL, opacity = 0.35), data = pts(oth))))
    if (nrow(ver)) ser <- c(ser, list(list(type = "scatter", name = tr("to_verify", lang), symbol = "emptyCircle", symbolSize = 12,
                                           itemStyle = list(color = "#C4502F", borderWidth = 2), data = pts(ver))))
    e_raw(list(
      grid = list(left = 40, right = 56, top = 36, bottom = 56, containLabel = TRUE),
      legend = list(bottom = 0, textStyle = list(color = COL$muted, fontSize = 10, fontStyle = "italic"), itemWidth = 14, itemHeight = 14),
      tooltip = list(formatter = htmlwidgets::JS("function(p){ var v = p.componentType === 'markPoint' ? p.data.coord : p.value; return '<i>'+p.seriesName+'</i>'+(p.componentType === 'markPoint' ? ' · <b>'+p.name+'</b>' : '')+'<br>'+v[0]+' × '+v[1]+' cm'; }")),
      xAxis = list(type = "value", name = tr("length_cm", lang), nameLocation = "end", splitLine = list(lineStyle = list(color = COL$line))),
      yAxis = list(type = "value", name = tr("width_cm", lang), nameTextStyle = list(align = "left"), splitLine = list(lineStyle = list(color = COL$line))),
      series = ser))
  })

  # ---------------- Data quality ----------------
  issues <- shiny::reactive(quality_issues(db(), stations()))
  output$load_errors <- shiny::renderUI({
    e <- db()$errors; if (!length(e)) return(NULL)
    htmltools::div(class = "alert alert-warning", bsicons::bs_icon("exclamation-triangle"), htmltools::tags$b(tr("load_errors", l())),
                   htmltools::tags$ul(lapply(e, htmltools::tags$li)))
  })
  output$q_boxes <- shiny::renderUI({
    q <- issues(); lang <- l(); tb <- sort(table(q$issue), decreasing = TRUE)
    bslib::layout_column_wrap(width = "200px", fill = FALSE, !!!lapply(names(tb), function(k)
      bslib::value_box(title = tr(paste0("iss_", k), lang), value = as.integer(tb[[k]]), class = "kpi kpi-sm",
                       theme = bslib::value_box_theme(bg = COL$surface, fg = COL$ink))))
  })
  output$q_table <- reactable::renderReactable({
    q <- issues(); lang <- l()
    q$issue <- trv(q$issue, "iss", lang)
    names(q) <- vapply(c("file", "sheet", "row", "field", "value", "issue"), tr, "", lang = lang)
    reactable::reactable(q, filterable = TRUE, searchable = TRUE, compact = TRUE, striped = TRUE, defaultPageSize = 15, language = rt_lang(lang),
                         groupBy = names(q)[6], columns = stats::setNames(list(reactable::colDef(minWidth = 240), reactable::colDef(minWidth = 260)), names(q)[c(1, 6)]))
  })
  output$q_download <- shiny::downloadHandler(
    filename = function() paste0("bigal_calidad_", Sys.Date(), ".csv"),
    content = function(f) utils::write.csv(issues(), f, row.names = FALSE, fileEncoding = "UTF-8")
  )
}
