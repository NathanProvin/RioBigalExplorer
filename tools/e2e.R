# Dev check: drives the running app (http://127.0.0.1:4321) with real clicks. Usage: Rscript tools/e2e.R <outdir>
Sys.setenv(CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
out <- commandArgs(TRUE)[1]
new_session <- function(url = "http://127.0.0.1:4321/") {
  b <- chromote::ChromoteSession$new(width = 1600, height = 1000)
  invisible(b$Page$enable())
  invisible(b$Page$addScriptToEvaluateOnNewDocument(
    "window.__errs=[]; window.addEventListener('error', e => window.__errs.push(e.message)); const _ce = console.error; console.error = function(){ window.__errs.push([...arguments].map(String).join(' ')); _ce.apply(console, arguments); };"))
  invisible(b$Page$navigate(url)); Sys.sleep(12)
  b
}
b <- new_session()
js <- function(x) b$Runtime$evaluate(x, awaitPromise = TRUE)$result$value
click <- function(sel) {
  r <- js(sprintf("(() => { const e = document.querySelector(%s); if (!e) return null; const r = e.getBoundingClientRect(); return JSON.stringify([r.x + r.width/2, r.y + r.height/2]); })()",
                  jsonlite::toJSON(sel, auto_unbox = TRUE)))
  if (is.null(r)) stop("not found: ", sel)
  xy <- jsonlite::fromJSON(r)
  for (t in c("mousePressed", "mouseReleased")) b$Input$dispatchMouseEvent(type = t, x = xy[1], y = xy[2], button = "left", clickCount = 1)
}
shot <- function(name) writeBin(jsonlite::base64_dec(b$Page$captureScreenshot()$data), file.path(out, paste0("e2e_", name, ".png")))
errs <- function() js("JSON.stringify(window.__errs)")

# 1. camera placement: open drawer from the count pill, crosshair on the first camera, click the map
before <- if (file.exists("data/stations.csv")) nrow(read.csv("data/stations.csv")) else 0
click(".count-hint"); Sys.sleep(1)
click(".cam-row .btn-icon"); Sys.sleep(1.5)
for (t in c("mousePressed", "mouseReleased")) b$Input$dispatchMouseEvent(type = t, x = 900, y = 480, button = "left", clickCount = 1)
Sys.sleep(6)
cat("stations before/after placing:", before, if (file.exists("data/stations.csv")) nrow(read.csv("data/stations.csv")) else 0, "\n")
click(".drawer .btn-close"); Sys.sleep(1)

# 2. basemap switch
click("#basemap input[value='topo'] + span"); Sys.sleep(2)
cat("topo tiles visible:", js("[...document.querySelectorAll('.leaflet-tile')].some(t => t.src.includes('World_Topo_Map') && getComputedStyle(t.closest('.leaflet-layer')).display !== 'none')"), "\n")
click("#basemap input[value='sat'] + span"); Sys.sleep(1)

# 3. every colour-by option shows a legend, without JS errors
for (v in c("source", "species", "class", "family", "trail", "moon", "hour", "year", "date", "prec")) {
  js(sprintf("(() => { const s = document.getElementById('color_by'); s.value = '%s'; s.dispatchEvent(new Event('change', {bubbles: true})); })()", v))
  Sys.sleep(6)
  cat(sprintf("colour by %-8s legend rows: %s\n", v, js("document.querySelectorAll('.map-legend .leg-row, .map-legend .leg-ramp').length")))
  if (v %in% c("species", "moon")) shot(paste0("color_", v))
}

# 4. timeline on
click("#anim_on"); Sys.sleep(5)
cat("timeline pill:", js("document.querySelector('.count-date') ? document.querySelector('.count-date').textContent : 'none'"), "\n")
click("#anim_on"); Sys.sleep(5)

# 5. filter then reset
js("(() => { $('#f_class')[0].selectize.setValue(['Anfibio']); })()"); Sys.sleep(6)
cat("records with class filter:", js("document.querySelector('.map-count b').textContent"), "\n")
click("#f_reset"); Sys.sleep(6)
cat("records after reset:", js("document.querySelector('.map-count b').textContent"), "\n")
# 5b. v1.2: balise names, tree/sunburst toggle, herp modality + isolation, distance by species
click("#lyr_bal_names"); Sys.sleep(6)
cat("balise name labels:", js("document.querySelectorAll('.bal-label').length"), "\n")
js("Shiny.setInputValue('nav', 'overview')"); js("$('a[data-value=\"overview\"]').tab('show')"); Sys.sleep(5)
cat("tree series:", js("echarts.getInstanceByDom(document.getElementById('c_sunburst')).getOption().series[0].type"), "\n")
click("#taxo_mode input[value='sunburst'] + span"); Sys.sleep(6)
cat("after toggle:", js("echarts.getInstanceByDom(document.getElementById('c_sunburst')).getOption().series[0].type"), "\n")
js("$('a[data-value=\"herps\"]').tab('show')"); Sys.sleep(14)
js("(() => { const s = document.getElementById('he_mod_sub'); s.value = 'order'; s.dispatchEvent(new Event('change', {bubbles: true})); })()"); Sys.sleep(7)
cat("pack groups (order):", js("echarts.getInstanceByDom(document.getElementById('c_he_substrate')).getOption().legend[0].data.map(d => d.name).join(', ')"), "\n")
js("Shiny.setInputValue('he_sub_click', 'clu|Hoja', {priority: 'event'})"); Sys.sleep(10)
cat("isolation chip:", js("(document.querySelector('#he_iso_sub .iso-chip') || {}).textContent"), "\n")
click("#he_iso_sub .iso-chip"); Sys.sleep(6)
cat("chip after reset:", js("document.querySelectorAll('#he_iso_sub .iso-chip').length"), "\n")
js("$('a[data-value=\"primates\"]').tab('show')"); Sys.sleep(10)
js("document.getElementById('pr_dist_sp').click()"); Sys.sleep(8)
cat("distance series by species:", js("echarts.getInstanceByDom(document.getElementById('c_pr_dist')).getOption().series.length"), "\n")
cat("JS errors:", errs(), "\n")
b$close()

# 6. delete the camera placed in step 1 through the confirm popover (cleans the test data)
b <- new_session()
click(".count-hint"); Sys.sleep(1)
click(".cam-row .btn-icon.danger"); Sys.sleep(1)
click(".popover .btn-danger"); Sys.sleep(6)
cat("stations after delete:", if (file.exists("data/stations.csv")) nrow(read.csv("data/stations.csv")) else 0, "\n")
b$close()
