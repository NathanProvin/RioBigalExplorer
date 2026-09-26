# One-off: georeference balise labels printed on "PUNTOS DE MONITOREO.pdf" -> ref/balises_extra.csv
# Run from project root: Rscript tools/digitize_pdf.R
suppressMessages(library(pdftools))
source("R/geo.R")
d <- as.data.frame(pdf_data("data/PUNTOS DE MONITOREO.pdf")[[1]])

# Graticule labels (e.g. -77°25', -0°32') give a linear pixel -> degree fit (4' x 4' extent, projection curvature negligible)
deg <- function(s) { v <- as.numeric(regmatches(s, gregexpr("\\d+", s))[[1]]); -(v[1] + v[2] / 60) }
gl <- d[grepl("°", d$text), ]
gl$val <- vapply(gl$text, deg, 0)
gl$cx <- gl$x + gl$width / 2; gl$cy <- gl$y + gl$height / 2
lon_fit <- lm(val ~ cx, gl[gl$width > gl$height, ])
lat_fit <- lm(val ~ cy, gl[gl$width < gl$height & gl$x < 1000, ])

# Balise labels: small font words "BIG" followed by "24"
b <- d[d$height == 9, ]
i <- which(grepl("^[A-Z]{3}$", b$text) & grepl("^\\d+$", c(b$text[-1], NA)))
lab <- data.frame(code = paste(b$text[i], as.integer(b$text[i + 1])), cx = b$x[i], cy = b$y[i] + b$height[i] / 2)
lab$lon <- predict(lon_fit, lab)
lab$lat <- predict(lat_fit, lab)

# Labels sit beside their flag: estimate the constant offset from balises that also have field GPS in the xlsx
gps <- read_balises_xlsx("data/Correspondance Balises et coordonnees GPS.xlsx")
gps$code <- norm_code(gps$code)
m <- merge(lab, gps, by = "code", suffixes = c("", ".gps"))
dlon <- median(m$lon.gps - m$lon); dlat <- median(m$lat.gps - m$lat)
m$err <- sqrt(((m$lon + dlon - m$lon.gps) * 111320)^2 + ((m$lat + dlat - m$lat.gps) * 110570)^2)
cat(sprintf("matched %d balises; label offset %.0f m E, %.0f m N; residual median %.0f m, max %.0f m\n",
            nrow(m), dlon * 111320, dlat * 110570, median(m$err), max(m$err)))
print(m[order(-m$err), c("code", "err")][1:8, ])
lab$lon <- lab$lon + dlon; lab$lat <- lab$lat + dlat

trails <- c(BIG = "Bigal Trail", DAN = "Payamino Trail", PAL = "Palestina Trail", SUN = "Suno Trail",
            PIA = "Piha Trail", CYR = "Cyrilo's Trail", SAL = "PNS Trail", BEN = "Palms Trail",
            JAC = "Jacob's Trail", VAL = "BRRS Trail", GUA = "Guadua Trail", BAM = "Bamboo Trail", HOT = "Hot Lip Loop")
lab$trail <- unname(trails[sub(" \\d+$", "", lab$code)])
write.csv(lab[, c("code", "trail", "lon", "lat")], "ref/balises_extra.csv", row.names = FALSE)
cat(nrow(lab), "balises written\n"); print(table(sub(" \\d+$", "", lab$code)))
