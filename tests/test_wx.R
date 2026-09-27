# Run from project root: Rscript tests/test_wx.R — which weather factor the herp chart highlights
for (f in c("R/i18n.R", "R/icons.R", "R/theme.R", "R/media.R", "R/mod_map.R", "R/mod_stats.R")) source(f)
set.seed(1)
nt <- data.frame(temp = runif(80, 18, 24), hum = runif(80, 70, 99), sky = sample(c("Despejado", "Nublado", "Niebla"), 80, TRUE, c(.5, .45, .05)))
nt$n <- round(2 * nt$temp - 30 + rnorm(80, 0, 1))                     # driven by temperature
r <- herp_wx_tests(nt)
stopifnot(identical(attr(r, "best"), "temp"), r$rho[1] > 0.8, is.na(r$rho[3]))
nt$n <- ifelse(nt$sky == "Nublado", 3, 15) + rpois(80, 2)             # driven by sky
stopifnot(identical(attr(herp_wx_tests(nt), "best"), "sky"))
nt$temp <- NA; nt$hum <- NA; nt$sky <- "Nublado"                      # nothing testable
stopifnot(is.na(attr(herp_wx_tests(nt), "best")))
cat("wx checks passed\n")
