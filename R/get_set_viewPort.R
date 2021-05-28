get_viewPort <- function(loon.grob) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("get_viewPort", obj)
}

get_viewPort.default <- function(loon.grob) {
  loonPlotLayer <- grid::getGrob(loon.grob, "loon plot")
  loonPlotLayer$vp
}

get_viewPort.l_serialaxes <- function(loon.grob) {
  loonPlotLayer <- grid::getGrob(loon.grob, "l_serialaxes")
  loonPlotLayer$vp
}


set_viewPort_grob <- function(loon.grob, margins, xlim, ylim) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_viewPort_grob", obj)
}

set_viewPort_grob.default <- function(loon.grob, margins, xlim, ylim) {

  grid::setGrob(
    gTree = loon.grob,
    gPath = "loon plot",
    newGrob = editGrob(
      grob = grid::getGrob(loon.grob, "loon plot"),
      vp = grid::vpStack(
        grid::plotViewport(margins = margins, name = "grid::plotViewport"),
        grid::dataViewport(xscale = xlim, yscale = ylim,
                     name = "dataViewport")
      )
    )
  )
}
