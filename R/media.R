# Species pictures + English common names (ref/species_media.csv, built by tools/fetch_species_images.R).
# Pictures live in www/species/ and are served as "species/<file>".

SPECIES_MEDIA <- if (file.exists("ref/species_media.csv"))
  utils::read.csv("ref/species_media.csv", stringsAsFactors = FALSE, encoding = "UTF-8") else
  data.frame(binomial = character(), file = character(), common_en = character(), license = character(), attribution = character(), inat_url = character())

sp_media <- function(binomial) SPECIES_MEDIA[match(binomial, SPECIES_MEDIA$binomial), ]
sp_img <- function(binomial) { f <- sp_media(binomial)$file; ifelse(is.na(f) | !file.exists(file.path("www/species", f)), NA, paste0("species/", f)) }

# Common name in the current language: English from iNaturalist, Spanish from the field sheets
sp_common <- function(binomial, common_es, lang) {
  en <- sp_media(binomial)$common_en
  out <- if (lang == "en") ifelse(is.na(en), common_es, en) else ifelse(is.na(common_es), en, common_es)
  ifelse(is.na(out), "", paste0(toupper(substr(out, 1, 1)), substring(out, 2)))  # sentence case, not Title Case
}

# Round profile picture; falls back to a silhouette of the animal group on a cream disc
sp_avatar <- function(binomial, class = NA, size = 64, ring = NULL, family = NA) {
  src <- sp_img(binomial)
  style <- sprintf("width:%dpx;height:%dpx;%s", size, size, if (!is.null(ring)) sprintf("box-shadow:0 0 0 3px %s;", ring) else "")
  m <- sp_media(binomial)
  if (!is.na(src))
    return(htmltools::tags$img(class = "sp-avatar", src = src, style = style, loading = "lazy",
                               title = if (!is.na(m$attribution)) paste0(m$attribution, " · iNaturalist")))
  ico <- if (identical(class, "Mamífero")) "paw" else herp_icon(family)
  htmltools::span(class = "sp-avatar sp-avatar-empty", style = style, html_icon(ico, "#8FA88A", paste0(round(size * 0.5), "px")))
}
