# Readers for the five workbooks -> tidy data frames, plus the shared normalizers.
# Every sheet is read as text and parsed here, so type drift in edited xlsx files can't break readxl.

NA_TOKENS <- c("", "?", "??", "???", "n/a", "na", "nd", "-", "_", ".", "x?", "sp. sp.", "sin avistamiento")

strip_accents <- function(x) iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "")
norm_key <- function(x) gsub("[^a-z0-9]", "", tolower(strip_accents(x)))

clean_chr <- function(x) {
  x <- trimws(gsub("[\\s ]+", " ", sub("^[¿?]+ *", "", as.character(x)), perl = TRUE))
  x[tolower(x) %in% NA_TOKENS] <- NA
  x
}
num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x)))

# Excel serial ("41624"), ISO ("2023-12-06") or d/m/Y with stray slashes ("06//12/2023") -> Date
as_date_any <- function(x) {
  x <- clean_chr(x)
  n <- num(x)
  d <- as.Date(ifelse(!is.na(n) & n > 20000 & n < 80000, n, NA), origin = "1899-12-30")
  s <- is.na(d) & !is.na(x)
  if (any(s)) {
    y <- gsub("/+", "/", x[s])
    p <- as.Date(y, format = "%d/%m/%Y")
    p[is.na(p)] <- as.Date(y[is.na(p)], format = "%Y-%m-%d")
    d[s] <- p
  }
  d[!is.na(d) & (d < as.Date("2000-01-01") | d > Sys.Date() + 1)] <- NA
  d
}

# Excel day fraction ("0.5847"), Excel datetime serial ("1.4791"), decimal hours ("13.5") or "13:30" -> decimal hour
as_hour <- function(x) {
  x <- clean_chr(x)
  n <- num(x)
  h <- rep(NA_real_, length(x))
  frac <- !is.na(n) & (n < 1 | (n < 2 & nchar(sub("^[^.]*\\.?", "", x)) > 4))
  h[frac] <- (n[frac] %% 1) * 24
  dec <- !is.na(n) & !frac & n < 24
  h[dec] <- n[dec]
  hm <- is.na(h) & grepl("^\\d{1,2}[:h]\\d{2}", x %||% "")
  if (any(hm)) {
    p <- strsplit(sub("^(\\d{1,2})[:h](\\d{2}).*", "\\1 \\2", x[hm]), " ")
    h[hm] <- vapply(p, function(v) as.numeric(v[1]) + as.numeric(v[2]) / 60, 0)
  }
  h[!is.na(h) & h >= 24] <- NA
  h
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# "dasyprocta Fuliginosa", "Tayassu pecari pecari", "Mazama ?", "Prismantis cf. lanthanites" -> species-level binomial
clean_binomial <- function(x, synonyms = NULL) {
  x <- clean_chr(x)
  x <- gsub("\\b(cf|aff)\\.?\\s", "", x, ignore.case = TRUE)
  w <- strsplit(tolower(x), " ")
  out <- vapply(w, function(v) {
    v <- v[v != ""]
    if (!length(v) || is.na(v[1]) || v[1] %in% c("sp", "sp.", "?")) return(NA_character_)
    g <- paste0(toupper(substr(v[1], 1, 1)), substr(v[1], 2, nchar(v[1])))
    if (length(v) == 1 || v[2] %in% c("sp", "sp.", "spe", "?", "spp", "spp.")) return(paste(g, "sp."))
    paste(g, gsub("[?]", "", v[2]))
  }, "")
  if (!is.null(synonyms)) {
    k <- match(out, synonyms$raw)
    out[!is.na(k)] <- synonyms$accepted[k[!is.na(k)]]
  }
  out[out %in% c("", "NA")] <- NA
  out
}

canon <- function(x, rules, default = NA_character_) {
  k <- tolower(strip_accents(clean_chr(x)))
  out <- rep(default, length(k))
  for (nm in rev(names(rules))) out[!is.na(k) & grepl(rules[[nm]], k)] <- nm
  out
}

SITE_RULES <- c("Sendero de animales" = "s[ae]nderos? de animales", "Saladero" = "saladero",
                "Estero" = "^estero", "Baño de lodo" = "bano de lodo", "Lugar de paso" = "lieu de passage")
AGE_RULES  <- c("Adulto + juvenil" = "adult.*(juv|bebe|petit|jeune)|(juv|bebe).*adult",
                "Cría" = "bebe|cria|petit", "Juvenil" = "juv|jeune", "Adulto" = "adult")
SEX_RULES  <- c("Macho" = "^(m|macho|male)$", "Hembra" = "^(h|f|hembra|female|femelle)$")

# Canonical trail names match the PDF legend so they join with the balise polylines.
TRAIL_RULES <- c("PNS Trail" = "pn ?s|parque|sumaco", "Bigal Trail" = "bigal|bigual|bigail",
                 "Palms Trail" = "palm", "Bamboo Trail" = "bamb|bandoo", "Payamino Trail" = "payamin",
                 "Suno Trail" = "suno", "BRRS Trail" = "brrs|\\bval\\b", "Piha Trail" = "piha|\\bpia\\b",
                 "Jacob's Trail" = "jacob|jaco", "Hot Lip Loop" = "hot ?lip", "Palestina Trail" = "palestin",
                 "Guadua Trail" = "guadua", "Cyrilo's Trail" = "cyril|ciril", "Camp Loop" = "camp",
                 "Entrance Trail" = "entra")
# First trail mentioned in free text such as "Bamboo Trail + Payamino Trail"
canon_trail <- function(x) {
  k <- tolower(strip_accents(clean_chr(x)))
  pos <- vapply(TRAIL_RULES, function(p) { r <- regexpr(p, k); ifelse(is.na(r) | r < 0, Inf, r) }, numeric(length(k)))
  pos <- matrix(pos, nrow = length(k))
  best <- apply(pos, 1, which.min)
  ifelse(is.finite(pos[cbind(seq_along(k), best)]), names(TRAIL_RULES)[best], NA_character_)
}

# Sky state from free-text field notes ("Semi nublado", "Lluviendo", "Sol"...) -> 5 states
# (earlier rules win: "Semi despejado" is partly cloudy, "Lluvia ligera" is rain)
SKY_RULES <- c("Parcial" = "semi|variable|parcial", "Lluvia" = "lluv|llov|lluen", "Niebla" = "neblin|niebla|bruma",
               "Despejado" = "despej|^sol|soleado|solei", "Nublado" = "nubla|nuebl|cubiert|covier|muglad|nuage")
SKY_LEVELS <- c("Despejado", "Parcial", "Nublado", "Lluvia", "Niebla")
canon_sky <- function(x) canon(x, SKY_RULES)

# 8 moon phases computed from the date (field notes use 30+ spellings; the astronomical value is consistent).
MOON_ES <- c("New" = "Nueva", "Waxing crescent" = "Creciente", "First quarter" = "Cuarto creciente",
             "Waxing gibbous" = "Gibosa creciente", "Full" = "Llena", "Waning gibbous" = "Gibosa menguante",
             "Last quarter" = "Cuarto menguante", "Waning crescent" = "Menguante")
moon_phase <- function(d) {
  out <- rep(NA_character_, length(d)); ok <- !is.na(d)
  if (any(ok)) out[ok] <- MOON_ES[as.character(lunar::lunar.phase(d[ok], name = 8))]
  factor(out, levels = MOON_ES)
}

# Map normalized header -> our column names. spec: c(new = "regex on norm_key(header)"); `need` must be found.
pick <- function(d, spec, need, file) {
  keys <- norm_key(names(d))
  cols <- lapply(spec, function(p) { j <- grep(p, keys)[1]; if (is.na(j)) NULL else j })
  miss <- intersect(need, names(spec)[vapply(cols, is.null, TRUE)])
  if (length(miss)) stop(sprintf("%s: columnas faltantes / missing columns: %s", basename(file), paste(miss, collapse = ", ")), call. = FALSE)
  out <- as.data.frame(lapply(cols, function(j) if (is.null(j)) rep(NA_character_, nrow(d)) else as.character(d[[j]])),
                       stringsAsFactors = FALSE)
  names(out) <- names(spec)
  out
}

read_text <- function(path, sheet, skip = 0) {
  d <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_types = "text", skip = skip, .name_repair = "unique_quiet"))
  d$.row <- seq_len(nrow(d)) + skip + 1  # Excel row number, for the data-quality page
  d
}

# Pick the sheet whose name matches `pattern`, otherwise the first one (sheet names carry dates that change).
find_sheet <- function(path, pattern) {
  s <- readxl::excel_sheets(path)
  s[grep(pattern, s, ignore.case = TRUE)[1]] %||% s[1]
}

with_common <- function(x, file, sheet, d) {
  x$.row <- d$.row; x$.file <- basename(file); x$.sheet <- sheet
  x
}

read_photos <- function(path, syn) {
  sh <- find_sheet(path, "global|base")
  d <- read_text(path, sh)
  x <- pick(d, c(camera = "^camara", common_es = "^commonnamespanish", date = "^fechacaptura", time = "^horacaptura",
                 temp_c = "ambiente.?c$", class = "^clase", genus_raw = "^genero", binomial_raw = "^binomial",
                 n = "animales$", age = "^edad", sex = "^sexo", site = "^typedesite", notes = "^notes"),
            need = c("camera", "date", "binomial_raw"), path)
  x <- with_common(x, path, sh, d)
  x <- x[rowSums(!is.na(x[, c("camera", "date", "binomial_raw")])) > 0, ]
  x$camera <- toupper(clean_chr(x$camera))
  x$date <- as_date_any(x$date); x$hour <- as_hour(x$time); x$time <- NULL
  x$binomial <- clean_binomial(x$binomial_raw, syn)
  x$common_es <- clean_chr(x$common_es)
  x$temp_c <- num(x$temp_c)
  x$n <- num(x$n)
  x$age <- canon(x$age, AGE_RULES); x$sex <- canon(x$sex, SEX_RULES); x$site <- canon(x$site, SITE_RULES)
  x$event <- independent_events(x$camera, x$binomial, x$date, x$hour)
  x
}

# New event when > gap_min minutes since the previous photo of the same species at the same camera.
independent_events <- function(camera, binomial, date, hour, gap_min = 30) {
  t <- as.numeric(date) * 1440 + ifelse(is.na(hour), 0, hour * 60)
  o <- order(camera, binomial, t)
  key <- paste(camera, binomial)[o]
  new <- c(TRUE, key[-1] != key[-length(key)] | is.na(t[o][-1]) | diff(t[o]) > gap_min)
  ev <- integer(length(t)); ev[o] <- cumsum(new)
  ev
}

read_monkeys <- function(path, syn) {
  sh <- find_sheet(path, "^base$")
  d <- read_text(path, sh)
  x <- pick(d, c(trail_raw = "^transecto", date = "^fechas", time = "^hora$", common_es = "^animalspanish",
                 common_en = "^animalenglish", binomial_raw = "^binomial", dist = "^distancia", height = "^altura",
                 group = "grupo$", activity = "^actividad", gps_y = "gpsutmy", gps_x = "gpsutmx", alt = "^altitud",
                 sky_raw = "^clima$", temp = "^c.?$", hum = "^humedad"),
            need = c("trail_raw", "date", "binomial_raw"), path)
  x <- with_common(x, path, sh, d)
  x <- x[!is.na(clean_chr(x$binomial_raw)) | !is.na(clean_chr(x$common_es)), ]
  x$date <- as_date_any(x$date); x$hour <- as_hour(x$time); x$time <- NULL
  x$binomial <- clean_binomial(x$binomial_raw, syn)
  x$trail <- canon_trail(x$trail_raw)
  for (v in c("dist", "height", "group", "alt", "temp", "hum")) x[[v]] <- num(x[[v]])
  x$hum <- ifelse(!is.na(x$hum) & x$hum <= 1, x$hum * 100, x$hum)  # sheet stores humidity as a fraction
  x$temp[!is.na(x$temp) & (x$temp < 5 | x$temp > 45)] <- NA
  x$sky <- canon_sky(x$sky_raw)
  x$activity <- canon(x$activity, c("Comiendo" = "comi|aliment", "Desplazándose" = "despla|movi|pasa|salt|escal",
                                    "Vocalizando" = "vocal|grit|cant", "Descansando" = "descan|durm",
                                    "Jugando" = "jug", "Huyendo" = "huy|fug"), default = NA)
  # GPS: DMS ("00°32.345´") or UTM ("#9941174", "# 022 9337")
  lat <- parse_dms(x$gps_y); lon <- parse_dms(x$gps_x)
  u <- is.na(lat) | is.na(lon)
  ll <- utm_to_lonlat(utm_num(x$gps_x[u]), utm_num(x$gps_y[u]))
  lat[u] <- ll$lat; lon[u] <- ll$lon
  x$lat <- lat; x$lon <- lon
  x
}

read_herps <- function(path, syn) {
  sh <- find_sheet(path, "^base$")
  d <- read_text(path, sh, skip = 2)
  x <- pick(d, c(trail_raw = "^transectono", date = "^fechas", time = "^hora$", family = "^familia",
                 binomial_raw = "^geniusandspecies", substrate = "^sustrato", height = "^alturasp",
                 lt = "^lt$", suborder = "^surborder", alt = "^altitud",
                 age_a = "^a$", age_j = "^j$", age_h = "^h$", sex_m = "^m$", sex_h = "^he$",
                 sky_raw = "^clima$", temp = "^temp", hum = "^hygro", disturb = "^gradodeintervencion"),
            need = c("trail_raw", "date", "family", "binomial_raw"), path)
  x <- with_common(x, path, sh, d)
  x <- x[!is.na(clean_chr(x$binomial_raw)) | !is.na(clean_chr(x$family)), ]
  x$date <- as_date_any(x$date); x$hour <- as_hour(x$time); x$time <- NULL
  x$binomial <- clean_binomial(x$binomial_raw, syn)
  x$family <- clean_chr(x$family)
  x$family <- ifelse(is.na(x$family), NA, paste0(toupper(substr(x$family, 1, 1)), tolower(substring(x$family, 2))))
  x$family[x$family %in% c("Leuperidae")] <- "Leiuperidae"
  x$family[x$family %in% c("Gymnophtalmidae")] <- "Gymnophthalmidae"
  x$family[x$family %in% c("Colibride", "Calubridae")] <- "Colubridae"
  x$trail <- canon_trail(x$trail_raw)
  x$substrate <- canon(x$substrate, c("Hoja" = "hoja|leaf", "Rama" = "rama|branch", "Suelo" = "suelo|tierra|ground",
                                      "Tronco" = "tronco|trunk|arbol|palo", "Tallo" = "tallo|stem", "Raíz" = "raiz",
                                      "Bromelia" = "bromel", "Helecho" = "helecho", "Liana" = "liana|bejuco",
                                      "Agua" = "agua|water|rio|quebrada", "Piedra" = "piedra|roca"), default = "Otro")
  x$height <- num(x$height); x$lt <- num(x$lt); x$alt <- num(x$alt)
  x$temp <- num(x$temp); x$hum <- num(x$hum); x$disturb <- num(x$disturb)
  x$temp[!is.na(x$temp) & (x$temp < 5 | x$temp > 45)] <- NA
  x$hum[!is.na(x$hum) & (x$hum < 1 | x$hum > 100)] <- NA
  x$sky <- canon_sky(x$sky_raw)
  mark <- function(v) !is.na(clean_chr(v))
  # field-sheet codes: A = adult, J = juvenile, H = egg / hatchling; M = male, He = female
  x$age <- ifelse(mark(x$age_a), "Adulto", ifelse(mark(x$age_j), "Juvenil", ifelse(mark(x$age_h), "Neonato", NA)))
  x$sex <- ifelse(mark(x$sex_m), "Macho", ifelse(mark(x$sex_h), "Hembra", NA))
  x$class <- ifelse(grepl("serpentes|sauria|amphisb", tolower(x$suborder)) |
                      x$family %in% c("Colubridae", "Viperidae", "Elapidae", "Boidae", "Iguanidae", "Gymnophthalmidae",
                                      "Dactyloidae", "Polychrotidae", "Teiidae", "Sphaerodactylidae", "Tropiduridae"),
                    "Reptil", "Anfibio")
  x[, c("age_a", "age_j", "age_h", "sex_m", "sex_h")] <- NULL
  x
}

read_tracks <- function(path, syn) {
  sh <- find_sheet(path, "data$|^2\\. ?data$")
  d <- read_text(path, sh)
  x <- pick(d, c(trail_raw = "^transect$", km = "^kmtransect", common_es = "^name$|^commonnamespanish",
                 date = "^fecha", genus_raw = "^genero", species_raw = "^especi", len = "^longueur",
                 wid = "^largeur", verify = "^averifier", notes = "^notes"),
            need = c("date", "genus_raw", "species_raw"), path)
  x <- with_common(x, path, sh, d)
  x <- x[!is.na(clean_chr(x$genus_raw)) | !is.na(clean_chr(x$common_es)), ]
  x$date <- as_date_any(x$date)
  x$binomial_raw <- ifelse(is.na(x$genus_raw), NA, paste(x$genus_raw, ifelse(is.na(x$species_raw), "sp.", x$species_raw)))
  x$binomial <- clean_binomial(x$binomial_raw, syn)
  x$common_es <- clean_chr(x$common_es)
  x$trail <- canon_trail(x$trail_raw)
  x$balise <- norm_code(x$km)
  x$len <- num(sub("\\s*cm.*", "", x$len)); x$wid <- num(sub("\\s*cm.*", "", x$wid))
  x$verify <- !is.na(clean_chr(x$verify))
  x
}

load_ref <- function(dir = "ref") {
  syn <- utils::read.csv(file.path(dir, "synonyms.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  tax <- utils::read.csv(file.path(dir, "taxonomy.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  fam <- utils::read.csv(file.path(dir, "family_order.csv"), stringsAsFactors = FALSE, encoding = "UTF-8")
  wf <- file.path(dir, "weather_hourly.csv.gz")  # ERA5 reanalysis, tools/fetch_weather.R
  wx <- if (file.exists(wf)) utils::read.csv(gzfile(wf), stringsAsFactors = FALSE) else NULL
  list(syn = syn, tax = tax, fam = fam, wx = wx)
}

# Locate each workbook in a folder by name pattern (friends may bump version suffixes like "PU 61" -> "PU 62").
FILE_PATTERNS <- c(photos = "global", monkeys = "monos", herps = "herpeto", tracks = "tracks|huellas", balises = "balise")
find_files <- function(dir) {
  f <- list.files(dir, pattern = "\\.xlsx$", full.names = TRUE)
  f <- f[!grepl("^~\\$", basename(f))]
  vapply(FILE_PATTERNS, function(p) f[grep(p, basename(f), ignore.case = TRUE)[1]] %||% NA_character_, "")
}
