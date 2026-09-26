# Where the xlsx files come from (local folder or shared Google Drive folder), per-process parse cache,
# camera-station persistence, the unified observation table and data-quality issues.
#
# Env vars:
#   DATA_SOURCE      "local" (default, reads ./data) or "drive"
#   GDRIVE_FOLDER_ID id of the shared Drive folder (drive mode)
#   GDRIVE_SA_JSON   service-account key: a file path or the JSON text itself (drive mode)

DATA_SOURCE <- Sys.getenv("DATA_SOURCE", "local")
LOCAL_DIR   <- "data"
CACHE_DIR   <- file.path(tempdir(), "bigal_drive")
STATIONS    <- "stations.csv"

drive_ready <- local({
  done <- FALSE
  function() {
    if (done) return(invisible(TRUE))
    key <- Sys.getenv("GDRIVE_SA_JSON")
    if (!file.exists(key)) { f <- tempfile(fileext = ".json"); writeLines(key, f); key <- f }
    googledrive::drive_auth(path = key)
    done <<- TRUE
  }
})
drive_folder <- function() googledrive::as_id(Sys.getenv("GDRIVE_FOLDER_ID"))

# Returns the local directory holding the current xlsx files. In drive mode only changed files are downloaded.
sync_files <- function() {
  if (DATA_SOURCE != "drive") return(LOCAL_DIR)
  drive_ready()
  dir.create(CACHE_DIR, showWarnings = FALSE)
  ls <- googledrive::drive_ls(drive_folder(), pattern = "\\.xlsx$")
  for (i in seq_len(nrow(ls))) {
    dest <- file.path(CACHE_DIR, ls$name[i])
    remote <- as.POSIXct(ls$drive_resource[[i]]$modifiedTime, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
    if (!file.exists(dest) || file.mtime(dest) < remote) {
      googledrive::drive_download(ls[i, ], path = dest, overwrite = TRUE)
      Sys.setFileTime(dest, remote)
    }
  }
  CACHE_DIR
}

# Camera stations: camera_id, lat, lon, set_at
read_stations <- function() {
  empty <- data.frame(camera_id = character(), lat = numeric(), lon = numeric(), set_at = character())
  path <- if (DATA_SOURCE == "drive") file.path(CACHE_DIR, STATIONS) else file.path(LOCAL_DIR, STATIONS)
  if (DATA_SOURCE == "drive") {
    drive_ready()
    f <- googledrive::drive_ls(drive_folder(), pattern = paste0("^", STATIONS, "$"))
    if (!nrow(f)) return(empty)
    dir.create(CACHE_DIR, showWarnings = FALSE)
    googledrive::drive_download(f[1, ], path = path, overwrite = TRUE)
  }
  if (!file.exists(path)) return(empty)
  utils::read.csv(path, stringsAsFactors = FALSE)
}

# ponytail: last write wins; add a modifiedTime check before writing if several people place cameras at once.
write_stations <- function(st) {
  path <- if (DATA_SOURCE == "drive") file.path(CACHE_DIR, STATIONS) else file.path(LOCAL_DIR, STATIONS)
  utils::write.csv(st, path, row.names = FALSE)
  if (DATA_SOURCE == "drive") googledrive::drive_put(path, path = drive_folder(), name = STATIONS)
  invisible(st)
}

.parsed <- new.env()
# Parse one file, memoised on path + mtime + size so a refresh only re-reads files that changed.
parse_cached <- function(reader, path, ...) {
  key <- paste(path, file.mtime(path), file.size(path))
  if (!is.null(.parsed[[key]])) return(.parsed[[key]])
  .parsed[[key]] <- reader(path, ...)
}

READERS <- list(photos = read_photos, monkeys = read_monkeys, herps = read_herps, tracks = read_tracks)

# Build the whole database. A file that fails to parse (missing columns, unreadable) keeps its previous
# good version from `prev` and the error is reported instead of crashing the app.
load_db <- function(prev = NULL) {
  dir <- sync_files()
  files <- find_files(dir)
  ref <- load_ref()
  db <- list(errors = character(), tax = ref$tax, fam = ref$fam, wx = ref$wx)
  for (src in names(READERS)) {
    res <- tryCatch({
      if (is.na(files[[src]])) stop(sprintf("no se encontró el archivo / file not found (%s)", FILE_PATTERNS[[src]]), call. = FALSE)
      parse_cached(READERS[[src]], files[[src]], ref$syn)
    }, error = function(e) e)
    if (inherits(res, "error")) {
      db$errors <- c(db$errors, conditionMessage(res))
      res <- prev[[src]]
    }
    db[[src]] <- res
  }
  db$balises <- tryCatch(parse_cached(build_balises, files[["balises"]]), error = function(e) {
    db$errors <<- c(db$errors, conditionMessage(e)); prev$balises
  })
  db$synced <- Sys.time()
  db
}

# ---- unified observation table -------------------------------------------------------------

jitter_m <- function(n, m) stats::rnorm(n, 0, m / 111000)

# Point location for records without GPS: balise code -> random balise of the trail -> camp.
place <- function(trail, balise, balises) {
  n <- length(trail)
  out <- data.frame(lat = rep(NA_real_, n), lon = rep(NA_real_, n), prec = rep(NA_character_, n))
  set.seed(42)  # stable positions between refreshes
  bi <- match(balise, balises$code)
  hit <- !is.na(bi)
  out$lat[hit] <- balises$lat[bi[hit]] + jitter_m(sum(hit), 15)
  out$lon[hit] <- balises$lon[bi[hit]] + jitter_m(sum(hit), 15)
  out$prec[hit] <- "balise"
  by_trail <- split(seq_len(nrow(balises)), balises$trail)
  need <- which(!hit & trail %in% names(by_trail))
  j <- vapply(need, function(i) { cand <- by_trail[[trail[i]]]; cand[sample.int(length(cand), 1)] }, 1L)
  out$lat[need] <- balises$lat[j] + jitter_m(length(j), 40)
  out$lon[need] <- balises$lon[j] + jitter_m(length(j), 40)
  out$prec[need] <- "trail"
  camp <- balises[grepl("^CAMP", balises$code), ][1, ]
  rest <- is.na(out$prec) & !is.na(trail) & !is.na(camp$lat)
  out$lat[rest] <- camp$lat + jitter_m(sum(rest), 120)
  out$lon[rest] <- camp$lon + jitter_m(sum(rest), 120)
  out$prec[rest] <- "camp"
  out
}

# genus -> family/order/class for mammals (taxonomy.csv), family -> order for herps (family_order.csv)
add_taxonomy <- function(o, tax, fam = NULL) {
  o$genus <- sub(" .*", "", o$binomial)
  k <- match(o$genus, tax$genus)
  o$family <- ifelse(is.na(o$family), tax$family[k], o$family)
  o$class <- ifelse(is.na(o$class), tax$class[k], o$class)
  o$order <- tax$order[k]
  if (!is.null(fam)) { j <- match(o$family, fam$family); o$order <- ifelse(is.na(o$order), fam$order[j], o$order) }
  o
}

# ERA5 sky state from hourly precipitation (mm) and cloud cover (%)
era5_sky <- function(precip, cloud) ifelse(is.na(cloud), NA, ifelse(precip > 0.2, "Lluvia", ifelse(cloud < 25, "Despejado", ifelse(cloud < 70, "Parcial", "Nublado"))))

build_obs <- function(db, stations) {
  b <- db$balises
  cols <- c("source", "date", "hour", "class", "family", "binomial", "common_es", "trail", "station", "lat", "lon", "prec", "n", "event",
            "sky", "temp", "hum", "wx_src")
  p <- db$photos
  si <- match(p$camera, stations$camera_id)
  # cameras only log temperature: sky and humidity come from ERA5 reanalysis at the photo's hour
  wx <- db$wx
  wi <- if (!is.null(wx)) match(paste(format(p$date, "%Y-%m-%d"), floor(p$hour)), paste(wx$date, wx$hour)) else rep(NA_integer_, nrow(p))
  photo <- data.frame(source = "photo", date = p$date, hour = p$hour, class = "Mamífero", family = NA, binomial = p$binomial,
                      common_es = p$common_es, trail = NA, station = p$camera, lat = stations$lat[si], lon = stations$lon[si],
                      prec = ifelse(is.na(si), NA, "station"), n = p$n, event = p$event,
                      sky = if (!is.null(wx)) era5_sky(wx$precip[wi], wx$cloud[wi]) else NA,
                      temp = ifelse(is.na(p$temp_c), if (!is.null(wx)) wx$temp[wi] else NA, p$temp_c),
                      hum = if (!is.null(wx)) wx$hum[wi] else NA, wx_src = "era5")
  m <- db$monkeys
  pl <- place(m$trail, rep(NA, nrow(m)), b)
  gps <- !is.na(m$lat)
  monkey <- data.frame(source = "monkey", date = m$date, hour = m$hour, class = "Mamífero", family = NA, binomial = m$binomial,
                       common_es = m$common_es, trail = m$trail, station = NA, lat = ifelse(gps, m$lat, pl$lat),
                       lon = ifelse(gps, m$lon, pl$lon), prec = ifelse(gps, "gps", pl$prec), n = m$group, event = NA,
                       sky = m$sky, temp = m$temp, hum = m$hum, wx_src = "field")
  h <- db$herps
  pl <- place(h$trail, rep(NA, nrow(h)), b)
  herp <- data.frame(source = "herp", date = h$date, hour = h$hour, class = h$class, family = h$family, binomial = h$binomial,
                     common_es = NA, trail = h$trail, station = NA, lat = pl$lat, lon = pl$lon, prec = pl$prec, n = 1, event = NA,
                     sky = h$sky, temp = h$temp, hum = h$hum, wx_src = "field")
  t <- db$tracks
  pl <- place(t$trail, t$balise, b)
  track <- data.frame(source = "track", date = t$date, hour = NA, class = "Mamífero", family = NA, binomial = t$binomial,
                      common_es = t$common_es, trail = t$trail, station = NA, lat = pl$lat, lon = pl$lon, prec = pl$prec, n = 1, event = NA,
                      sky = NA, temp = NA, hum = NA, wx_src = NA)
  o <- rbind(photo[cols], monkey[cols], herp[cols], track[cols])
  o <- add_taxonomy(o, db$tax, db$fam)
  o$iucn <- if (exists("SPECIES_MEDIA") && !is.null(SPECIES_MEDIA$iucn)) SPECIES_MEDIA$iucn[match(o$binomial, SPECIES_MEDIA$binomial)] else NA
  o$year <- as.integer(format(o$date, "%Y"))
  o$month <- as.integer(format(o$date, "%m"))
  o$ym <- as.Date(format(o$date, "%Y-%m-01"))
  o$moon <- moon_phase(o$date)
  o$rid <- seq_len(nrow(o))
  o
}

# ---- data quality -------------------------------------------------------------------------

quality_issues <- function(db, stations) {
  tax <- db$tax$genus
  one <- function(d, which, field, value, issue) {
    if (!any(which, na.rm = TRUE)) return(NULL)
    w <- which(which)
    data.frame(file = d$.file[w], sheet = d$.sheet[w], row = d$.row[w], field = field, value = as.character(value[w]), issue = issue)
  }
  unresolved <- function(d, mammal) {
    g <- sub(" .*", "", d$binomial)
    named <- !is.na(d$binomial_raw) & nzchar(gsub("\\b(sp|spe|spp|na)\\b|[^a-z]", "", tolower(d$binomial_raw %||% ""))) # not just "sp sp" / "?"
    rbind(one(d, is.na(d$binomial) & named, "binomial", d$binomial_raw, "species_unrecognized"),
          if (mammal) one(d, !is.na(g) & !g %in% tax, "binomial", d$binomial_raw, "genus_no_taxonomy"),
          one(d, is.na(d$date), "date", rep("", nrow(d)), "date_invalid"))
  }
  out <- list(
    unresolved(db$photos, TRUE),
    one(db$photos, is.na(db$photos$hour), "hour", rep("", nrow(db$photos)), "time_invalid"),
    unresolved(db$monkeys, TRUE),
    one(db$monkeys, is.na(db$monkeys$lat) & !is.na(clean_chr(db$monkeys$gps_y)), "GPS", paste(db$monkeys$gps_y, db$monkeys$gps_x), "gps_unparsed"),
    one(db$monkeys, is.na(db$monkeys$trail) & !is.na(clean_chr(db$monkeys$trail_raw)), "trail", db$monkeys$trail_raw, "trail_unrecognized"),
    one(db$monkeys, !is.na(db$monkeys$height) & db$monkeys$height > 60, "Altura", db$monkeys$height, "value_out_of_range"),
    unresolved(db$herps, FALSE),
    one(db$herps, is.na(db$herps$trail) & !is.na(clean_chr(db$herps$trail_raw)), "trail", db$herps$trail_raw, "trail_unrecognized"),
    # body length > 3x the species median: most likely mm typed in the cm column
    one(db$herps, !is.na(db$herps$lt) & db$herps$lt > 3 * stats::ave(db$herps$lt, db$herps$binomial, FUN = function(x) stats::median(x, na.rm = TRUE)),
        "LT", db$herps$lt, "value_out_of_range"),
    unresolved(db$tracks, TRUE),
    one(db$tracks, db$tracks$verify, "A verifier", db$tracks$binomial_raw, "flagged_verify"),
    one(db$tracks, !is.na(db$tracks$balise) & !db$tracks$balise %in% db$balises$code, "KM transect", db$tracks$km, "balise_unknown"),
    one(db$tracks, is.na(db$tracks$trail) & !is.na(clean_chr(db$tracks$trail_raw)), "trail", db$tracks$trail_raw, "trail_unrecognized")
  )
  q <- do.call(rbind, out)
  cams <- setdiff(unique(stats::na.omit(db$photos$camera)), stations$camera_id)
  if (length(cams)) q <- rbind(q, data.frame(file = db$photos$.file[1], sheet = "", row = NA,
                                             field = "Cámara Nº", value = cams, issue = "camera_unplaced"))
  q
}
