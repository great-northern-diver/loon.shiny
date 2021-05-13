loon_default_serialaxes_args <- function() {
  list(
    title_fontsize = 18,
    parallel_xlim = c(-0.1, 1.12),
    parallel_ylim = c(-0.1, 1.12),
    radial_xlim = c(-0.2, 1.2),
    radial_ylim = c(-0.2, 1.2),
    radius = 0.2,
    guides_background = loon::l_getOption("canvas_bg_guides"),
    line_color1 = loon::l_getOption("background"),
    line_color2 = loon::l_getOption("foreground"),
    guideLine_width = 2,
    label_fontsize = 9,
    linewidth_default = 1,
    radius_offset = 0.1,
    margins = rep(0,4)
  )
}
