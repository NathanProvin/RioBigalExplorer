# Dev helper: print JS errors raised by a page of the running app. Usage: Rscript tools/console.R overview
Sys.setenv(CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe")
b <- chromote::ChromoteSession$new(width = 1600, height = 1000)
invisible(b$Page$enable()); invisible(b$Runtime$enable())
invisible(b$Page$addScriptToEvaluateOnNewDocument(paste(
  "window.__errs=[]; window.addEventListener('error', e => window.__errs.push(e.message + ' @' + (e.filename||'').split('/').pop() + ':' + e.lineno));",
  "var __t = setInterval(function(){ if (window.jQuery) { clearInterval(__t); jQuery(document).on('shiny:value', function(e){ window.__errs.push('value:' + e.name); }); } }, 10);",
  "const _ce = console.error; console.error = function(){ window.__errs.push('console: ' + [...arguments].map(String).join(' ')); _ce.apply(console, arguments); };")))
invisible(b$Page$navigate(paste0("http://127.0.0.1:4321/", Sys.getenv("SHOT_Q"), "#", commandArgs(TRUE)[1])))
Sys.sleep(22)
r <- b$Runtime$evaluate("JSON.stringify({errs: window.__errs, k: (document.getElementById('k_records')||{}).textContent, empty: [...document.querySelectorAll('.echarts4r')].filter(e => e.offsetParent && !e.querySelector('canvas')).map(e => e.id)})")
writeLines(paste("RESULT", r$result$value))
b$close()
