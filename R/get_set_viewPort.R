get_viewPort <- function(loon_grob) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("get_viewPort", obj)
}

get_viewPort.default <- function(loon_grob) {
  loon_plot_layer <- getGrob(loon_grob, "loon plot")
  loon_plot_layer$vp
}

get_viewPort.l_serialaxes <- function(loon_grob) {
  loon_plot_layer <- getGrob(loon_grob, "l_serialaxes")
  loon_plot_layer$vp
}


set_viewPort_grob <- function(loon_grob, margins, xlim, ylim) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("set_viewPort_grob", obj)
}

set_viewPort_grob.default <- function(loon_grob, margins, xlim, ylim) {
  
  setGrob(
    gTree = loon_grob,
    gPath = "loon plot",
    newGrob = editGrob(
      grob = getGrob(loon_grob, "loon plot"),
      vp = vpStack(
        plotViewport(margins = margins, name = "plotViewport"),
        dataViewport(xscale = xlim, yscale = ylim,
                     name = "dataViewport")
      )
    )
  )
}