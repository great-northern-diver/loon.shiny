loon_defaultSerialaxesSettings_args <- function() {
  list(
    titleFontsize = 18,
    parallelXlim = c(-0.1, 1.12),
    parallelYlim = c(-0.1, 1.12),
    radialXlim = c(-0.2, 1.2),
    radialYlim = c(-0.2, 1.2),
    radius = 0.2,
    guidesBackground = loon::l_getOption("canvas_bg_guides"),
    lineColor1 = loon::l_getOption("background"),
    lineColor2 = loon::l_getOption("foreground"),
    guideLineWidth = 2,
    labelFontsize = 9,
    linewidthDefault = 1,
    radiusOffset = 0.1,
    margins = rep(0,4)
  )
}
