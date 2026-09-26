# Regenerate manifest.json for Posit Connect Cloud (run after adding/updating R packages):
#   Rscript tools/write_manifest.R
# Only the files the app needs are listed, so dev-only packages (webshot2, chromote, pdftools...) used in
# tools/ and tests/ are not installed on the server, and data/, .secrets/, .Renviron are never deployed.
files <- c("app.R", list.files("R", full.names = TRUE), list.files("www", recursive = TRUE, full.names = TRUE),
           list.files("i18n", full.names = TRUE), list.files("ref", full.names = TRUE))
rsconnect::writeManifest(appDir = ".", appFiles = files, appPrimaryDoc = "app.R", appMode = "shiny")
m <- jsonlite::fromJSON("manifest.json")
cat(sprintf("manifest.json: R %s, %d packages, %d files\n", m$platform, length(m$packages), length(m$files)))
