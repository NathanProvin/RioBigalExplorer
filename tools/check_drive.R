# One-shot check of the Google Drive connection. Run from the project root: Rscript tools/check_drive.R
# First run with GDRIVE_AUTH=user opens a browser to sign in (token then cached in .secrets/).
Sys.setenv(DATA_SOURCE = "drive")
options(rlang_interactive = TRUE)  # allow the one-time browser sign-in even under Rscript
source("R/geo.R"); source("R/data_clean.R"); source("R/data_sync.R")
ok <- function(x) cat("  ✓", x, "\n"); ko <- function(x) cat("  ✗", x, "\n")

cat("1. Sign-in\n"); drive_ready()
u <- googledrive::drive_user(); ok(sprintf("signed in as %s", u$emailAddress %||% u$displayName))

cat("2. Folder", Sys.getenv("GDRIVE_FOLDER_ID"), "\n")
ls <- drive_list()
for (i in seq_len(nrow(ls))) {
  r <- ls$drive_resource[[i]]
  cat(sprintf("     %-55s %8s  %s\n", ls$name[i], if (is.null(r$size)) "-" else format(as.numeric(r$size), big.mark = " "), substr(r$modifiedTime, 1, 10)))
}

cat("3. Expected workbooks\n")
xl <- ls$name[grepl("\\.xlsx$", ls$name, ignore.case = TRUE)]
for (s in names(FILE_PATTERNS)) {
  m <- xl[grepl(FILE_PATTERNS[[s]], xl, ignore.case = TRUE)]
  if (!length(m)) ko(sprintf("%-8s no file matching '%s'", s, FILE_PATTERNS[[s]]))
  else if (length(m) > 1) ko(sprintf("%-8s several files match, the first is used: %s", s, paste(m, collapse = " | ")))
  else ok(sprintf("%-8s %s", s, m))
}

cat("4. Download + parse\n")
db <- load_db()
for (s in c("photos", "monkeys", "herps", "tracks", "balises")) ok(sprintf("%-8s %d rows", s, NROW(db[[s]])))
if (length(db$errors)) for (e in db$errors) ko(e) else ok("no parse errors")

cat("5. Camera stations (stations.csv)\n")
st <- read_stations(); ok(sprintf("%d camera position(s) read", nrow(st)))
res <- tryCatch({ write_stations(st); "ok" }, error = function(e) conditionMessage(e))
if (identical(res, "ok")) ok("stations.csv can be written (created if it was missing)") else ko(res)
cat("\nDone.\n")
