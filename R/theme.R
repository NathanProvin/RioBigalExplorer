# Visual identity: cream + Amazon green. Categorical order validated (dataviz validator, light, surface #FBF8F0):
# adjacent CVD ΔE >= 8.9, normal-vision >= 24.6. Ochre/gold slots are < 3:1 on cream -> charts keep labels/tooltips.

COL <- list(
  cream = "#F6F1E4", surface = "#FBF8F0", green = "#1E4D2B", green_dk = "#12301B", ink = "#1F2A22",
  muted = "#6B7565", line = "#E4DCC8", ochre = "#C8A24A", sage = "#8FA88A"
)
PAL <- c("#2F7D32", "#D9822B", "#1F7FA8", "#C9A227", "#8E4C9E", "#6E8B1E", "#3C4FA0", "#C4502F")

# Fixed entity -> colour (colour follows the entity, never its rank)
SOURCE_COL <- c(photo = PAL[1], monkey = PAL[2], herp = PAL[3], track = PAL[8])
# Trail colours = legend of the reserve's PDF map, so the team recognises them
TRAIL_COL <- c("Bigal Trail" = "#C500FF", "Payamino Trail" = "#38A800", "Piha Trail" = "#E1E1E1",
               "Palms Trail" = "#A87001", "PNS Trail" = "#FF5500", "Hot Lip Loop" = "#FFE6E6",
               "Palestina Trail" = "#A8A800", "BRRS Trail" = "#8C5A00", "Bamboo Trail" = "#00A884",
               "Jacob's Trail" = "#9C9C9C", "Guadua Trail" = "#73FFDF", "Suno Trail" = "#004DA8",
               "Cyrilo's Trail" = "#343434")

bigal_theme <- function() {
  bslib::bs_theme(
    version = 5, bg = COL$cream, fg = COL$ink, primary = COL$green, secondary = COL$ochre,
    success = PAL[1], info = PAL[3], warning = PAL[4], danger = PAL[8],
    # fonts from the Google CDN: local = TRUE copied font files on every page render (~6 s)
    base_font = bslib::font_google("Inter", wght = c(400, 500, 600), local = FALSE),
    heading_font = bslib::font_google("Fraunces", wght = c(500, 600), local = FALSE),
    "border-radius" = "0.875rem", "card-border-color" = COL$line, "card-bg" = COL$surface,
    "navbar-bg" = COL$green_dk, "font-size-base" = "0.925rem"
  )
}

# Styled tooltip; charts call e_tip(trigger = "axis", ...) to change trigger/formatting without losing the style
e_tip <- function(e, trigger = "item", ...) {
  echarts4r::e_tooltip(e, trigger = trigger, backgroundColor = COL$surface, borderColor = COL$line,
                       textStyle = list(color = COL$ink, fontFamily = "Inter"), ...)
}

# echarts defaults shared by every chart
e_bigal <- function(e, legend = TRUE) {
  e |>
    echarts4r::e_color(PAL) |>
    e_tip() |>
    echarts4r::e_legend(show = legend, bottom = 0, textStyle = list(color = COL$muted), itemStyle = list(borderWidth = 0)) |>
    echarts4r::e_grid(left = 48, right = 16, top = 36, bottom = if (legend) 48 else 28, containLabel = TRUE) |>
    echarts4r::e_toolbox_feature(feature = "saveAsImage", title = "PNG") |>
    echarts4r::e_text_style(fontFamily = "Inter", color = COL$muted)
}

# Density heatmaps (map + species "Where"): deep blue -> sand -> brick red
HEAT_GRADIENT <- c("0.15" = "#1F3B73", "0.35" = "#5B8DB8", "0.55" = "#E8D8B0", "0.8" = "#C4502F", "1" = "#8E2A1B")
