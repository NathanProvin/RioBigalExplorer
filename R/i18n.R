# ES/EN dictionary (i18n/translation.csv: key, es, en). Static UI uses tt(); server-rendered text uses tr(key, lang).
# The browser swaps [data-i18n] spans on language change (www/app.js); tooltips are updated server-side.

I18N <- utils::read.csv("i18n/translation.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
DICT <- list(es = stats::setNames(I18N$es, I18N$key), en = stats::setNames(I18N$en, I18N$key))

tr <- function(key, lang = "es") {
  v <- unname(DICT[[lang]][key])
  ifelse(is.na(v), key, v)
}
tt <- function(key) htmltools::span(`data-i18n` = key, tr(key))

# Translate data category values (site types, moon phases, activities…): dictionary key "<prefix>_<value>",
# falling back to the value itself so unknown categories still display.
trv <- function(x, prefix, lang = "es") {
  v <- unname(DICT[[lang]][paste0(prefix, "_", x)])
  ifelse(is.na(v), as.character(x), v)
}

# Tooltip registry: every tip() is re-translated on language change via bslib::update_tooltip
TIPS <- new.env()
tip <- function(trigger, key, placement = "auto") {
  id <- paste0("tip_", key, "_", length(ls(TIPS)))
  TIPS[[id]] <- key
  bslib::tooltip(trigger, tr(key), id = id, placement = placement)
}

set_language <- function(session, lang) {
  session$sendCustomMessage("i18n", as.list(DICT[[lang]]))
  for (id in ls(TIPS)) bslib::update_tooltip(id, tr(TIPS[[id]], lang), session = session)
}
