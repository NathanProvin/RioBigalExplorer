# Dev helper: screenshot pages of the running app (http://127.0.0.1:4321).
# Usage: Rscript tools/shot.R <outdir> map overview species ...   (optional env LANG_EN=1 to click EN first)
Sys.setenv(CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
a <- commandArgs(TRUE); out <- a[1]; pages <- a[-1]
for (p in pages)
  webshot2::webshot(paste0("http://127.0.0.1:4321/", Sys.getenv("SHOT_Q"), "#", p), file.path(out, paste0("s_", p, ".png")),
                    vwidth = 1600, vheight = as.integer(Sys.getenv("SHOT_H", "1000")), delay = 15, cliprect = "viewport", quiet = TRUE)
