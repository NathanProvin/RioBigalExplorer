# One-off (re-runnable): profile picture + English common name for every species in the data, from iNaturalist.
# Only openly licensed photos are kept (CC0 / CC BY / CC BY-SA / CC BY-NC); credit is stored for display.
# Existing rows in ref/species_media.csv are kept, so re-running only fetches new species.
# Run from project root: Rscript tools/fetch_species_images.R
source("R/geo.R"); source("R/data_clean.R"); source("R/data_sync.R")
OK_LICENSES <- c("cc0", "cc-by", "cc-by-sa", "cc-by-nc")
out_csv <- "ref/species_media.csv"
dir.create("www/species", showWarnings = FALSE)

db <- load_db()
sp <- sort(unique(stats::na.omit(c(db$photos$binomial, db$monkeys$binomial, db$herps$binomial, db$tracks$binomial))))
sp <- setdiff(sp, "Homo sapiens")
old <- if (file.exists(out_csv)) utils::read.csv(out_csv, stringsAsFactors = FALSE) else NULL
todo <- setdiff(sp, old$binomial)
cat(length(todo), "species to fetch\n")

api <- function(path) {
  Sys.sleep(1.1)  # iNaturalist asks for <= 1 request / second
  tryCatch(jsonlite::fromJSON(paste0("https://api.inaturalist.org/v1/", path), simplifyVector = FALSE), error = function(e) NULL)
}

rows <- lapply(todo, function(b) {
  genus_only <- grepl(" sp\\.$", b)
  q <- utils::URLencode(sub(" sp\\.$", "", b), reserved = TRUE)
  hit <- api(sprintf("taxa?q=%s&rank=%s&per_page=1", q, if (genus_only) "genus" else "species"))
  row <- data.frame(binomial = b, file = NA, common_en = NA, license = NA, attribution = NA, inat_url = NA)
  if (is.null(hit) || !length(hit$results)) { cat("  -", b, "(not found)\n"); return(row) }
  t <- api(paste0("taxa/", hit$results[[1]]$id))$results[[1]]
  row$common_en <- t$preferred_common_name %||% NA
  row$inat_url <- paste0("https://www.inaturalist.org/taxa/", t$id)
  for (tp in t$taxon_photos) {
    p <- tp$photo
    if (!is.null(p$license_code) && p$license_code %in% OK_LICENSES) {
      f <- paste0(gsub("[^A-Za-z]+", "_", b), ".jpg")
      ok <- tryCatch(utils::download.file(sub("/square\\.", "/small.", p$url), file.path("www/species", f), mode = "wb", quiet = TRUE) == 0,
                     error = function(e) FALSE)
      if (ok) { row$file <- f; row$license <- p$license_code; row$attribution <- p$attribution; break }
    }
  }
  cat(if (is.na(row$file)) "  -" else "  +", b, "\n")
  row
})
res <- rbind(if (!is.null(old)) old[, setdiff(names(old), "iucn"), drop = FALSE], do.call(rbind, rows))
if (!is.null(old$iucn)) res$iucn <- old$iucn[match(res$binomial, old$binomial)] else res$iucn <- NA

# IUCN Red List category through GBIF (free, no key); genus-level "sp." rows have none
gbif <- function(path) { Sys.sleep(0.3); tryCatch(jsonlite::fromJSON(paste0("https://api.gbif.org/v1/", path)), error = function(e) NULL) }
for (i in which(is.na(res$iucn) & !grepl(" sp\\.$", res$binomial))) {
  m <- gbif(paste0("species/match?rank=SPECIES&name=", utils::URLencode(res$binomial[i], reserved = TRUE)))
  if (is.null(m$usageKey) || !identical(m$matchType, "EXACT") && !identical(m$matchType, "FUZZY")) next
  k <- gbif(sprintf("species/%s/iucnRedListCategory", m$acceptedUsageKey %||% m$usageKey))
  res$iucn[i] <- k$code %||% "NE"
  cat("  iucn", res$binomial[i], res$iucn[i], "\n")
}
utils::write.csv(res[order(res$binomial), ], out_csv, row.names = FALSE, fileEncoding = "UTF-8")
cat(sum(!is.na(res$file)), "/", nrow(res), "species with a picture\n")
