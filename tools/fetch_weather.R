# One-off (re-runnable): hourly reanalysis weather (ERA5, Open-Meteo archive API, free, no key) for the reserve,
# used to give camera-trap photos a sky state and humidity (the camera database only records temperature).
# Run from project root: Rscript tools/fetch_weather.R
lat <- -0.537; lon <- -77.428
years <- 2011:as.integer(format(Sys.Date(), "%Y"))
chunks <- lapply(years, function(y) {
  end <- min(as.Date(sprintf("%d-12-31", y)), Sys.Date() - 7)  # archive lags ~5 days
  url <- sprintf(paste0("https://archive-api.open-meteo.com/v1/archive?latitude=%s&longitude=%s&start_date=%d-01-01&end_date=%s",
                        "&hourly=temperature_2m,relative_humidity_2m,precipitation,cloud_cover&timezone=America%%2FGuayaquil"),
                 lat, lon, y, end)
  Sys.sleep(1)
  j <- tryCatch(jsonlite::fromJSON(url), error = function(e) { message(y, ": ", conditionMessage(e)); NULL })
  if (is.null(j)) return(NULL)
  h <- j$hourly
  cat(y, length(h$time), "hours\n")
  data.frame(time = h$time, temp = h$temperature_2m, hum = h$relative_humidity_2m, precip = h$precipitation, cloud = h$cloud_cover)
})
w <- do.call(rbind, chunks)
w$date <- substr(w$time, 1, 10); w$hour <- as.integer(substr(w$time, 12, 13)); w$time <- NULL
con <- gzfile("ref/weather_hourly.csv.gz", "w"); utils::write.csv(w, con, row.names = FALSE); close(con)
cat(nrow(w), "hourly rows written\n")
