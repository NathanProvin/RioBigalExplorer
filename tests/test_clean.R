# Run from project root: Rscript tests/test_clean.R
Sys.setenv(DATA_SOURCE = "local")  # checks run on ./data, whatever .Renviron says
source("R/geo.R"); source("R/data_clean.R"); source("R/data_sync.R")
near <- function(a, b, tol = 1e-3) isTRUE(all(abs(a - b) < tol))

# coordinates
ll <- utm_to_lonlat(230108, 9940583)
stopifnot(near(ll$lat, -0.53708, 1e-4), near(ll$lon, -77.42483, 1e-4))
stopifnot(utm_num("# 022 9337") == 229337, utm_num("#9941174") == 9941174)
stopifnot(near(parse_dms("00°32.345´"), -0.539083), near(parse_dms("077°25.812´"), -77.4302), is.na(parse_dms("#9941174")))
stopifnot(identical(norm_code(c("PNS13", "Bam 3", "SAL 1", "300 m de la Y")), c("SAL 13", "BAM 3", "SAL 1", NA)))

# dates and times
stopifnot(identical(as_date_any(c("41624", "06//12/2023", "2023-12-06", "?")),
                    as.Date(c("2013-12-16", "2023-12-06", "2023-12-06", NA))))
h <- as_hour(c("0.58472222222222225", "13.5", "1.4791666666666667", "13:30", "?"))
stopifnot(near(h[1:4], c(14.0333, 13.5, 11.5, 13.5)), is.na(h[5]))

# names
syn <- data.frame(raw = c("Dasyprocta fulginosa", "Homo erectus"), accepted = c("Dasyprocta fuliginosa", "Homo sapiens"))
stopifnot(identical(clean_binomial(c("dasyprocta Fulginosa", "Tayassu pecari pecari", "Mazama ?", "Prismantis cf. lanthanites",
                                     "Sp. Sp.", "?", "Bolitoglossa spe", "Homo erectus"), syn),
                    c("Dasyprocta fuliginosa", "Tayassu pecari", "Mazama sp.", "Prismantis lanthanites",
                      NA, NA, "Bolitoglossa sp.", "Homo sapiens")))
stopifnot(identical(canon_trail(c("Bamboo Trail + Payamino Trail", "PNS y Limite parque", "hot lip loop", "xyz", NA)),
                    c("Bamboo Trail", "PNS Trail", "Hot Lip Loop", NA, NA)))
stopifnot(identical(canon(c("Sandero de animales", "bano de lodo", "adulto y juvenile"), c(SITE_RULES, AGE_RULES)),
                    c("Sendero de animales", "Baño de lodo", "Adulto + juvenil")))
stopifnot(as.character(moon_phase(as.Date("2024-01-11"))) == "Nueva")

# independent events: same cam+species within 30 min = one event
ev <- independent_events(c("C1", "C1", "C1", "C2"), rep("Tapirus terrestris", 4),
                         as.Date(rep("2020-01-01", 4)), c(10, 10.25, 11, 10))
stopifnot(length(unique(ev)) == 3, ev[1] == ev[2], ev[3] != ev[2])

# icons, moon glyphs and translations of every data category (ES and EN)
source("R/i18n.R"); source("R/icons.R"); source("R/media.R")
stopifnot(length(unique(MOON_URI)) == 8, all(nzchar(ICON_PATH)))
cats <- list(moon = unname(MOON_ES), site = names(SITE_RULES), age = names(AGE_RULES), sub = names(SUBSTRATE_ICON),
             act = names(ACTIVITY_ICON), cls = c("Mamífero", "Anfibio", "Reptil"))
for (p in names(cats)) for (lg in c("es", "en")) {
  miss <- cats[[p]][is.na(DICT[[lg]][paste0(p, "_", cats[[p]])])]
  if (length(miss)) stop(sprintf("missing %s translation for %s_: %s", lg, p, paste(miss, collapse = ", ")))
}
stopifnot(trv("Llena", "moon", "en") == "Full", trv("unknown value", "moon", "en") == "unknown value")

# real data (skipped if the xlsx folder is absent)
if (dir.exists("data") && length(list.files("data", "xlsx$"))) {
  db <- load_db()
  stopifnot(!length(db$errors), nrow(db$photos) == 7038, nrow(db$herps) > 4700, nrow(db$monkeys) > 700, nrow(db$tracks) > 1400)
  stopifnot(nrow(db$balises) > 200, all(db$balises$lat > -0.6 & db$balises$lat < -0.49, na.rm = TRUE))
  o <- build_obs(db, read_stations())
  stopifnot(nrow(o) == nrow(db$photos) + nrow(db$monkeys) + nrow(db$herps) + nrow(db$tracks))
  # v1.2: weather, orders, ERA5 join, IUCN
  raw_sky <- c(db$herps$sky_raw, db$monkeys$sky_raw); sky <- c(db$herps$sky, db$monkeys$sky)
  miss_sky <- unique(raw_sky[is.na(sky) & !is.na(clean_chr(raw_sky))])
  if (length(miss_sky)) stop("sky values not mapped: ", paste(miss_sky, collapse = ", "))
  stopifnot(all(db$monkeys$hum >= 0 & db$monkeys$hum <= 100, na.rm = TRUE))
  stopifnot(all(stats::na.omit(unique(db$herps$family)) %in% db$fam$family))
  for (lg in c("es", "en")) stopifnot(!anyNA(DICT[[lg]][paste0("sky_", SKY_LEVELS)]))
  if (!is.null(db$wx)) { ph <- o[o$source == "photo" & !is.na(o$date) & !is.na(o$hour) & o$date < Sys.Date() - 10, ]
    stopifnot(mean(!is.na(ph$sky)) > 0.99) }
  if (file.exists("ref/species_media.csv")) {
    iu <- stats::na.omit(utils::read.csv("ref/species_media.csv")$iucn)
    stopifnot(all(iu %in% c("LC", "NT", "VU", "EN", "CR", "EW", "EX", "DD", "NE")))
  }
  q <- quality_issues(db, read_stations())
  stopifnot("date_invalid" %in% q$issue)
  cat(sprintf("obs %d | located %d | species %d | issues %d\n", nrow(o), sum(!is.na(o$lat)), length(unique(stats::na.omit(o$binomial))), nrow(q)))
  print(table(o$source, o$prec, useNA = "ifany"))
  print(table(q$issue))
}
cat("all checks passed\n")
