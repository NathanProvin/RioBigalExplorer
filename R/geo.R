# Coordinate parsing and trail geometry. Reserve sits in UTM zone 18 South (EPSG:32718).

# "18 M # 023 0108 # 9940 583" / "#0229372" / "# 022 9337" -> numeric metres
utm_num <- function(x) suppressWarnings(as.numeric(gsub("\\D", "", x)))

# "00°32.345´" -> -0.539 (reserve is S / W, so always negative)
parse_dms <- function(x) {
  m <- regmatches(x, regexec("(\\d+)\\s*°\\s*([0-9.]+)", x))
  vapply(m, function(v) if (length(v) == 3) -(as.numeric(v[2]) + as.numeric(v[3]) / 60) else NA_real_, numeric(1))
}

utm_to_lonlat <- function(e, n) {
  out <- matrix(NA_real_, length(e), 2)
  ok <- !is.na(e) & !is.na(n) & e > 1e5 & e < 9e5 & n > 9e6 & n < 1e7
  if (any(ok)) {
    p <- sf::st_as_sf(data.frame(e = e[ok], n = n[ok]), coords = c("e", "n"), crs = 32718)
    out[ok, ] <- sf::st_coordinates(sf::st_transform(p, 4326))
  }
  data.frame(lon = out[, 1], lat = out[, 2])
}

# Read the balise workbook: blocks of "<TRAIL NAME>" / "Points GPS" header / rows of
# code | zone "18 M" | "# 022 9987" (E) | "# 994 0464" (N) | altitude, laid out side by side from columns 1 and 8.
read_balises_xlsx <- function(path) {
  g <- as.data.frame(readxl::read_excel(path, sheet = 1, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  out <- list()
  for (c0 in c(1, 8)) {
    if (ncol(g) < c0 + 4) next
    trail <- NA
    for (i in seq_len(nrow(g))) {
      a <- g[i, c0]; e <- g[i, c0 + 2]
      if (is.na(a) || grepl("Points GPS", a)) next
      if (is.na(e)) { trail <- a; next }
      out[[length(out) + 1]] <- data.frame(code = a, trail_xlsx = trail, e = utm_num(e), n = utm_num(g[i, c0 + 3]),
                                           alt = suppressWarnings(as.numeric(g[i, c0 + 4])))
    }
  }
  b <- do.call(rbind, out)
  cbind(b, utm_to_lonlat(b$e, b$n))[, c("code", "trail_xlsx", "alt", "lon", "lat")]
}

# Field-sheet spellings of balise prefixes -> PDF prefixes
CODE_ALIAS <- c(DAM = "DAN", PNS = "SAL", PNST = "SAL", PALM = "BEN", BRRS = "VAL")

# "SAL 1", "PNS13", "Bam 3" -> "SAL 1", "SAL 13", "BAM 3"
norm_code <- function(x) {
  x <- toupper(trimws(x))
  m <- regmatches(x, regexec("^([A-Z]{2,4})\\s*(\\d{1,3})$", x))
  vapply(m, function(v) {
    if (length(v) != 3) return(NA_character_)
    p <- if (v[2] %in% names(CODE_ALIAS)) CODE_ALIAS[[v[2]]] else v[2]
    paste(p, as.integer(v[3]))
  }, character(1))
}

# Balises = PDF-digitized (ref/balises_extra.csv) overridden by GPS-measured xlsx points.
build_balises <- function(xlsx_path, extra_csv = "ref/balises_extra.csv") {
  x <- read_balises_xlsx(xlsx_path)
  x$code <- ifelse(is.na(norm_code(x$code)), toupper(x$code), norm_code(x$code))
  e <- utils::read.csv(extra_csv, stringsAsFactors = FALSE)
  x$trail <- e$trail[match(sub(" \\d+$", "", x$code), sub(" \\d+$", "", e$code))]
  x$trail[is.na(x$trail)] <- x$trail_xlsx[is.na(x$trail)]
  x$src <- "gps"
  e$src <- "pdf"; e$alt <- NA
  b <- rbind(x[, c("code", "trail", "alt", "lon", "lat", "src")],
             e[!e$code %in% x$code, c("code", "trail", "alt", "lon", "lat", "src")])
  b$num <- as.integer(sub("^\\D+", "", b$code))
  b[order(b$trail, b$code, b$num), ]
}

# One polyline per code prefix (a trail can have several prefixes), balises connected in numeric order.
trail_lines <- function(balises) {
  b <- balises[!is.na(balises$num) & !is.na(balises$trail), ]
  b$prefix <- sub(" \\d+$", "", b$code)
  split(b[order(b$prefix, b$num), c("lon", "lat", "trail", "prefix")], b$prefix[order(b$prefix, b$num)])
}
