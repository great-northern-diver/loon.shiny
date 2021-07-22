pt2cm <- function() 0.03514598 # convertUnit(unit(1, "pt"), "cm", valueOnly = TRUE)
pt2mm <- function() 0.3514598 # convertUnit(unit(1, "pt"), "mm", valueOnly = TRUE)
.pt <- 2.845276
pt2px <- function(adjust = 1) 4/3 * adjust
px2pt <- function(adjust = 1) 3/4 * adjust
cm2px <- function(adjust = 1) {
  # grid::convertUnit(grid::unit(1, "cm"), "pt",
  #                   valueOnly = TRUE) * pt2px(adjust = adjust)
  .pt * 10 * pt2px(adjust = adjust)
}
px2cm <- function(adjust = 1) {
  # px2pt(adjust = adjust) * grid::convertUnit(grid::unit(1, "pt"), "cm",
  #                   valueOnly = TRUE)
  px2pt(adjust = adjust) /(.pt * 10)
}
