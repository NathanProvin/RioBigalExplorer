# Icon registry: SVG path data for echarts symbols ("path://…") and inline HTML icons.
# Font Awesome paths come from the installed fontawesome package; shapes FA lacks are generated below
# from primitives (100x100 box, y down) so no image assets are needed.

fa_path <- function(name) sub('.*<path d="([^"]+)".*', "\\1", as.character(fontawesome::fa(name)))
fa_box <- function(name) as.numeric(strsplit(sub('.*viewBox="([^"]+)".*', "\\1", as.character(fontawesome::fa(name))), " ")[[1]])

# ---- primitives -------------------------------------------------------------------------
f1 <- function(x) formatC(x, format = "f", digits = 1)
circle_p <- function(cx, cy, r) sprintf("M%s %sa%s %s 0 1 0 %s 0a%s %s 0 1 0 %s 0Z", f1(cx - r), f1(cy), f1(r), f1(r), f1(2 * r), f1(r), f1(r), f1(-2 * r))
ellipse_p <- function(cx, cy, rx, ry, rot = 0) {
  a <- rot * pi / 180; dx <- rx * cos(a); dy <- rx * sin(a)
  sprintf("M%s %sA%s %s %s 1 0 %s %sA%s %s %s 1 0 %s %sZ", f1(cx - dx), f1(cy - dy), f1(rx), f1(ry), f1(rot),
          f1(cx + dx), f1(cy + dy), f1(rx), f1(ry), f1(rot), f1(cx - dx), f1(cy - dy))
}
# Thick polyline with (tapering) width w along a smooth centreline through the control points
thick_curve <- function(x, y, w, n = 40) {
  t <- seq(0, 1, length.out = n)
  sx <- stats::spline(seq_along(x), x, n = n)$y; sy <- stats::spline(seq_along(y), y, n = n)$y
  w <- stats::approx(seq(0, 1, length.out = length(w)), w, t)$y / 2
  dx <- c(diff(sx), diff(sx)[n - 1]); dy <- c(diff(sy), diff(sy)[n - 1]); len <- sqrt(dx^2 + dy^2)
  nx <- -dy / len; ny <- dx / len
  px <- c(sx + nx * w, rev(sx - nx * w)); py <- c(sy + ny * w, rev(sy - ny * w))
  paste0("M", paste(f1(px), f1(py), collapse = "L"), "Z")
}

# ---- generated silhouettes -----------------------------------------------------------------
SHAPES <- list(
  snake = paste(thick_curve(c(6, 22, 42, 60, 76, 86), c(70, 38, 66, 32, 58, 40), c(4, 13, 16, 16, 14)),
                ellipse_p(90, 36, 10, 8, -35)),
  snake_v = paste(thick_curve(c(70, 38, 66, 32, 58, 40), c(94, 78, 58, 40, 24, 14), c(4, 13, 16, 16, 14)),  # upright, head on top
                  ellipse_p(40, 9, 8, 10, 55)),
  lizard = paste(ellipse_p(50, 50, 11, 22), ellipse_p(50, 22, 9, 11),
                 thick_curve(c(50, 53, 62, 70), c(68, 82, 92, 98), c(11, 6, 2)),
                 thick_curve(c(43, 30, 22), c(40, 34, 24), c(7, 6, 5)), thick_curve(c(57, 70, 78), c(40, 34, 24), c(7, 6, 5)),
                 thick_curve(c(43, 30, 22), c(60, 67, 77), c(7, 6, 5)), thick_curve(c(57, 70, 78), c(60, 67, 77), c(7, 6, 5))),
  monkey = paste(circle_p(50, 27, 13), circle_p(35, 25, 6), circle_p(65, 25, 6), ellipse_p(50, 60, 15, 20),
                 thick_curve(c(38, 29, 36), c(48, 64, 78), c(6, 5, 5)), thick_curve(c(62, 71, 64), c(48, 64, 78), c(6, 5, 5)),
                 ellipse_p(40, 82, 10, 6), ellipse_p(60, 82, 10, 6),
                 thick_curve(c(64, 80, 90, 86, 78), c(76, 80, 66, 54, 56), c(5, 4, 3, 2))),
  hoof = paste(ellipse_p(39, 45, 10, 25, -8), ellipse_p(61, 45, 10, 25, 8), circle_p(30, 84, 4.5), circle_p(70, 84, 4.5)),
  tapir = paste(ellipse_p(50, 28, 11, 15), ellipse_p(26, 52, 9, 13, -25), ellipse_p(74, 52, 9, 13, 25), ellipse_p(50, 70, 17, 12)),
  branch = paste(thick_curve(c(5, 35, 60, 90), c(88, 62, 45, 22), c(14, 10, 6)), thick_curve(c(45, 52, 56), c(55, 68, 78), c(7, 5)),
                 ellipse_p(58, 86, 8, 13, -20), ellipse_p(90, 15, 8, 13, 40), ellipse_p(72, 26, 7, 12, -50)),
  roots = paste(thick_curve(c(50, 50, 50), c(2, 22, 42), c(18, 15)), thick_curve(c(50, 32, 14), c(40, 62, 92), c(10, 4)),
                thick_curve(c(50, 53, 48), c(40, 70, 98), c(10, 4)), thick_curve(c(50, 68, 86), c(40, 60, 88), c(10, 4))),
  vine = paste(thick_curve(c(50, 30, 68, 32, 58), c(2, 25, 50, 75, 98), c(8, 7, 6)),
               ellipse_p(22, 32, 9, 14, 40), ellipse_p(78, 55, 9, 14, -40), ellipse_p(22, 82, 9, 14, 40))
)

FA_ICONS <- c("camera", "binoculars", "frog", "paw", "leaf", "tree", "mound", "water", "hill-rockslide", "seedling", "spa",
              "feather", "circle", "utensils", "arrows-left-right", "person-running", "volume-high", "bed", "face-smile",
              "ruler-vertical", "moon", "sun", "triangle-exclamation", "cat", "shuffle", "cloud-sun", "cloud", "cloud-rain", "smog",
              "route", "gem", "droplet", "shoe-prints", "mars", "venus", "egg", "temperature-half", "mountain-sun", "xmark", "sitemap")
ICON_PATH <- c(stats::setNames(vapply(FA_ICONS, fa_path, ""), FA_ICONS), unlist(SHAPES))
ICON_BOX <- c(stats::setNames(lapply(FA_ICONS, fa_box), FA_ICONS), lapply(SHAPES, function(s) c(0, 0, 100, 100)))
ICON_BOX[c("monkey", "hoof", "tapir")] <- list(c(18, 8, 80, 84), c(20, 16, 60, 76), c(12, 10, 76, 76))  # tight crops

e_icon <- function(name) paste0("path://", ICON_PATH[[name]])
html_icon <- function(name, col = "currentColor", size = "1em", class = "ico") {
  b <- ICON_BOX[[name]]
  htmltools::HTML(sprintf('<svg class="%s" viewBox="%s" style="height:%s;width:%s;fill:%s;vertical-align:-0.125em"><path d="%s"/></svg>',
                          class, paste(b, collapse = " "), size, size, col, ICON_PATH[[name]]))
}
# Data-URI image of an icon (for echarts rich text / image symbols and CSS backgrounds)
icon_uri <- function(name, col = "#12301B") {
  b <- ICON_BOX[[name]]
  svg <- sprintf("<svg xmlns='http://www.w3.org/2000/svg' viewBox='%s'><path fill='%s' d='%s'/></svg>", paste(b, collapse = " "), col, ICON_PATH[[name]])
  paste0("data:image/svg+xml;charset=utf-8,", utils::URLencode(svg, reserved = TRUE))
}

# Which icon stands for what
SOURCE_ICON <- c(photo = "camera", monkey = "binoculars", herp = "frog", track = "paw")
SUBSTRATE_ICON <- c("Hoja" = "leaf", "Rama" = "branch", "Suelo" = "mound", "Tronco" = "tree", "Tallo" = "seedling",
                    "Raíz" = "roots", "Bromelia" = "spa", "Helecho" = "feather", "Liana" = "vine", "Agua" = "water",
                    "Piedra" = "hill-rockslide", "Otro" = "circle")
SKY_ICON <- c("Despejado" = "sun", "Parcial" = "cloud-sun", "Nublado" = "cloud", "Lluvia" = "cloud-rain", "Niebla" = "smog")
SKY_COL <- c("Despejado" = "#E8B84A", "Parcial" = "#9DB4C0", "Nublado" = "#7C8B99", "Lluvia" = "#3C6E9F", "Niebla" = "#A9A39A")
SITE_ICON <- c("Sendero de animales" = "route", "Saladero" = "gem", "Estero" = "water", "Baño de lodo" = "droplet",
               "Lugar de paso" = "shoe-prints")
# IUCN Red List official colours
IUCN_COL <- c(LC = "#60C659", NT = "#CCE226", VU = "#F9E814", EN = "#FC7F3F", CR = "#D81E05", EW = "#542344", EX = "#000000",
              DD = "#D1D1C6", NE = "#FFFFFF")
IUCN_THREAT <- c("VU", "EN", "CR")
iucn_pill <- function(code, lang = "es") {
  if (is.null(code) || is.na(code) || !code %in% names(IUCN_COL)) return(NULL)
  dark <- code %in% c("EN", "CR", "EW", "EX")
  htmltools::span(class = "iucn-pill", title = paste("IUCN:", tr(paste0("iucn_", code), lang)),
                  style = sprintf("background:%s;color:%s", IUCN_COL[[code]], if (dark) "#fff" else "#1F2A22"), code)
}
ACTIVITY_ICON <- c("Comiendo" = "utensils", "Desplazándose" = "arrows-left-right", "Vocalizando" = "volume-high",
                   "Descansando" = "bed", "Jugando" = "face-smile", "Huyendo" = "person-running")
# Track print by genus (cats/dogs/procyonids/mustelids leave paws; deer & peccaries cloven hooves; tapir 3 toes)
track_icon <- function(binomial) {
  g <- sub(" .*", "", binomial)
  ifelse(g %in% c("Mazama", "Tayassu", "Pecari"), "hoof", ifelse(g == "Tapirus", "tapir", "paw"))
}
ORDER_ICON <- c(Anura = "frog", Caudata = "lizard", Squamata = "snake", Gymnophiona = "snake")
# Icon for each herp group value, named by value; mod = "binomial" | "family" | "order"; upright = snake drawn vertically
herp_group_icons <- function(h, mod, upright = FALSE) {
  v <- unique(stats::na.omit(h[[mod]]))
  ico <- switch(mod,
    order = unname(ORDER_ICON[v]) %||% rep("frog", length(v)),
    family = herp_icon(v),
    binomial = herp_icon(h$family[match(v, h$binomial)]))
  ico[is.na(ico)] <- "frog"
  if (upright) ico[ico == "snake"] <- "snake_v"
  stats::setNames(ico, v)
}
herp_icon <- function(family, suborder = NA) {
  snakes <- c("Colubridae", "Viperidae", "Elapidae", "Boidae", "Dipsadidae")
  lizards <- c("Iguanidae", "Gymnophthalmidae", "Dactyloidae", "Polychrotidae", "Teiidae", "Sphaerodactylidae", "Tropiduridae", "Hoplocercidae")
  ifelse(family %in% snakes, "snake", ifelse(family %in% lizards, "lizard", "frog"))
}

# ---- moon phases: circle + terminator ellipse, k = 0 (new) .. 7 (waning crescent) ----------
moon_svg <- function(k, lit = "#F1E3AE", dark = "#34453A", ring = "#8FA88A") {
  r <- 10; cx <- 12; cy <- 12; p <- 2 * pi * k / 8
  rx <- f1(abs(cos(p)) * r)
  top <- sprintf("%s %s", cx, cy - r); bot <- sprintf("%s %s", cx, cy + r)
  lit_path <- if (k == 0) "" else if (k == 4) circle_p(cx, cy, r) else if (k < 4)
    sprintf("M%sA%s %s 0 0 1 %sA%s %s 0 0 %d %sZ", top, r, r, bot, rx, r, as.integer(k >= 2), top) else
    sprintf("M%sA%s %s 0 0 0 %sA%s %s 0 0 %d %sZ", top, r, r, bot, rx, r, as.integer(k >= 6), top)
  svg <- sprintf("<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'><circle cx='12' cy='12' r='10.4' fill='%s' stroke='%s' stroke-width='0.8'/><path fill='%s' d='%s'/></svg>",
                 dark, ring, lit, lit_path)
  paste0("data:image/svg+xml;charset=utf-8,", utils::URLencode(svg, reserved = TRUE))
}
MOON_URI <- vapply(0:7, moon_svg, "")

# echarts axis label with a moon glyph above the phase name (rich text); `names` = translated names in MOON_ES order
moon_axis_label <- function(names) {
  rich <- stats::setNames(lapply(seq_along(MOON_URI), function(i) list(height = 20, width = 20, align = "center",
                                                                         backgroundColor = list(image = MOON_URI[i]))),
                          paste0("m", 1:8))
  # glyph only: the 8 phase names don't fit side by side; they appear in the tooltips
  js <- sprintf("function(v){ var n = %s; var i = n.indexOf(v); return i < 0 ? v : '{m' + (i + 1) + '|}'; }", jsonlite::toJSON(unname(names)))
  list(formatter = htmlwidgets::JS(js), rich = rich, interval = 0)
}
