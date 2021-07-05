loonGrob_positions <- function(gtable, loon.grobs, arrangeGrobArgs = NULL, ...) {
  l_className <- gtable$name
  class(l_className) <- l_className
  UseMethod("loonGrob_positions", l_className)
}

loonGrob_positions.default <- function(gtable, loon.grobs, arrangeGrobArgs = NULL, ...) {

  layout_matrix <- arrangeGrobArgs$layout_matrix
  nrow <- arrangeGrobArgs$nrow
  ncol <- arrangeGrobArgs$ncol

  if (is.null(layout_matrix)) {

    nrow <- nrow %||% 1
    ncol <- ncol %||% 1

    positions <- expand.grid(t = seq_len(nrow), l = seq_len(ncol))
    positions$b <- positions$t
    positions$r <- positions$l
    positions <- positions[order(positions$t), ]
  } else {
    cells <- sort(unique(as.vector(layout_matrix)))
    range_cell <- function(ii) {
      ind <- which(layout_matrix == ii, arr.ind = TRUE)
      c(l = min(ind[, "col"], na.rm = TRUE),
        r = max(ind[, "col"], na.rm = TRUE),
        t = min(ind[, "row"], na.rm = TRUE),
        b = max(ind[, "row"], na.rm = TRUE))
    }
    positions <- data.frame(do.call(rbind, lapply(cells, range_cell)))
    ncol <- max(positions$r)
    nrow <- max(positions$b)
    positions <- positions[seq_along(loon.grobs), ]
  }

  data.frame(
    l = (positions$l - 1)/ncol,
    r = positions$r/ncol,
    t = (positions$t - 1)/nrow,
    b = positions$b/nrow
  )
}

loonGrob_positions.l_facet_grid <- function(gtable, loon.grobs, arrangeGrobArgs = NULL, ...) {

  args <- list(...)

  positions <- loonGrob_positions.default(gtable, loon.grobs, arrangeGrobArgs)
  layout <- gtable$layout

  widths.npc <- args$widths.npc %||% grid::convertUnit(gtable$widths, "npc", valueOnly = TRUE)
  heights.npc <- args$heights.npc %||% grid::convertUnit(gtable$heights, "npc", valueOnly = TRUE)

  gtableWidth.npc <- 1 - sum(widths.npc)
  gtableHeight.npc <- 1 - sum(heights.npc)

  startH <- sum(widths.npc[seq(which(widths.npc == 0)[1L])])
  startV <- sum(heights.npc[seq(which(heights.npc == 0)[1L])])

  positions$t <- positions$t * gtableHeight.npc + startV
  positions$b <- positions$b * gtableHeight.npc + startV
  positions$l <- positions$l * gtableWidth.npc + startH
  positions$r <- positions$r * gtableWidth.npc + startH

  positions
}

loonGrob_positions.l_facet_wrap <- function(gtable, loon.grobs, arrangeGrobArgs = NULL, ...) {

  args <- list(...)

  positions <- loonGrob_positions.default(gtable, loon.grobs, arrangeGrobArgs)
  layout <- gtable$layout

  widths.npc <- args$widths.npc %||% grid::convertUnit(gtable$widths, "npc", valueOnly = TRUE)
  heights.npc <- args$heights.npc %||% grid::convertUnit(gtable$heights, "npc", valueOnly = TRUE)

  gtableWidth.npc <- 1 - sum(widths.npc)
  gtableHeight.npc <- 1 - sum(heights.npc)

  labelHeight.npc <- c()
  i <- vapply(gtable$grobs, is.gtable, logical(1L))
  for(x in gtable$grobs[i]) {
    l.npc <- sum(grid::convertUnit(x$heights, "npc", valueOnly = TRUE))
    labelHeight.npc <- c(l.npc, labelHeight.npc)
  }
  labelHeight.npc <- args$labelHeight.npc %||% labelHeight.npc

  startH <- sum(widths.npc[seq(which(widths.npc == 0)[1L])])
  startV <- sum(heights.npc[seq(which(heights.npc == 0)[1L])])

  positions$t <- positions$t * gtableHeight.npc + startV + labelHeight.npc
  positions$b <- positions$b * gtableHeight.npc + startV
  positions$l <- positions$l * gtableWidth.npc + startH
  positions$r <- positions$r * gtableWidth.npc + startH

  positions
}

loonGrob_positions.l_facet_ggplot <- function(gtable, loon.grobs, arrangeGrobArgs = NULL, ...) {

  which_facet <- function(gtable, grobs.i) {

    isFacetWrap <- any(vapply(grobs.i,
                              function(g) {
                                any(grepl("FacetWrap", g$layout$name))
                              }, logical(1L)))
    isFacetGrid <- any(vapply(grobs.i,
                              function(g) {
                                any(grepl("FacetGrid", g$layout$name))
                              }, logical(1L)))
    facet <- c("facet_wrap", "facet_grid")[c(isFacetWrap, isFacetGrid)]
    if(length(facet) != 1)
      stop("Unknown Facet Mthod", call. = FALSE)
    facet
  }

  i <- vapply(gtable$grobs, is.gtable, logical(1L))
  # facet panels (get rid of labels)
  grobs.i <- gtable$grobs[i]

  facet <- which_facet(gtable, grobs.i)
  if(facet == "facet_wrap") {

    widths.npc <- grid::convertUnit(gtable$widths, "npc", valueOnly = TRUE)
    heights.npc <- grid::convertUnit(gtable$heights, "npc", valueOnly = TRUE)
    # TODO
    # Q: can the label (facet wrap) height be different?
    labelHeight.npc <- sum(grid::convertUnit(grobs.i[[1L]]$heights, "npc", valueOnly = TRUE))

    loonGrob_positions.l_facet_wrap(gtable = gtable,
                                    loon.grobs = loon.grobs,
                                    arrangeGrobArgs = arrangeGrobArgs,
                                    widths.npc = widths.npc,
                                    heights.npc = heights.npc,
                                    labelHeight.npc = labelHeight.npc)

  } else {
    # facet_grid
    widths.npc <- c(
      grid::convertUnit(gtable$widths, "npc", valueOnly = TRUE), # ylabel is on the left
      grid::convertUnit(grobs.i[[1L]]$widths, "npc", valueOnly = TRUE) # subtitles are on the right
    )
    heights.npc <- c(
      grid::convertUnit(grobs.i[[1L]]$heights, "npc", valueOnly = TRUE), # subtitles are on top
      grid::convertUnit(gtable$heights, "npc", valueOnly = TRUE) # xlabel is on bottom
    )

    loonGrob_positions.l_facet_grid(gtable = gtable,
                                    loon.grobs = loon.grobs,
                                    arrangeGrobArgs = arrangeGrobArgs,
                                    widths.npc = widths.npc,
                                    heights.npc = heights.npc)
  }
}








