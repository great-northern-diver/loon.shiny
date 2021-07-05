get_itemLabel <- function(loon.grob, plotHover, outputInfo, position) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("get_itemLabel", obj)
}

get_itemLabel.default <- function(loon.grob, plotHover, outputInfo, position) NULL

get_itemLabel.l_plot <- function(loon.grob, plotHover, outputInfo, position) {

  loonWidgetsInfo <- outputInfo$loonWidgetsInfo
  itemLabels <- loonWidgetsInfo$itemLabel

  if(is.null(loonWidgetsInfo$margins) ||
     is.null(loonWidgetsInfo$swap) ||
     is.null(loonWidgetsInfo$ylim) ||
     is.null(loonWidgetsInfo$xlim) ||
     is.null(itemLabels)) return(NULL)

  hoverId <- get_brushId(
    loon.grob = loon.grob,
    coord = list(
      x = loonWidgetsInfo$x,
      y = loonWidgetsInfo$y
    ),
    swapInShiny = loonWidgetsInfo$swapInShiny,
    swapInLoon = loonWidgetsInfo$swapInLoon,
    position = position,
    brushInfo = NULL,
    vp = grid::vpStack(
      grid::plotViewport(margins = loonWidgetsInfo$margins,
                         name = "plotViewport"),
      grid::dataViewport(
        xscale = if(loonWidgetsInfo$swap) {
          loonWidgetsInfo$ylim
        } else {
          loonWidgetsInfo$xlim
        },
        yscale = if(loonWidgetsInfo$swap) {
          loonWidgetsInfo$xlim
        } else {
          loonWidgetsInfo$ylim
        },
        name = "dataViewport")
    ),
    offset = loonWidgetsInfo$offset,
    clickInfo = plotHover
  )

  # has to be active
  whichIsDeactive <- which(!loonWidgetsInfo$active)
  hoverId <- setdiff(hoverId, whichIsDeactive)

  if(length(hoverId) == 0) {
    NULL
  } else {
    itemLabels[hoverId]
  }
}


get_itemLabel.l_serialaxes <- function(loon.grob, plotHover, outputInfo, position) {

  loonWidgetsInfo <- outputInfo$loonWidgetsInfo
  itemLabels <- loonWidgetsInfo$itemLabel

  if(is.null(itemLabels)) return(NULL)

  serialaxesGrob <- grid::getGrob(loon.grob, "l_serialaxes")

  hoverId <- get_brushId(
    loon.grob = loon.grob,
    coord = list(
      x = loonWidgetsInfo$x,
      y = loonWidgetsInfo$y
    ),
    position = position,
    brushInfo = NULL,
    vp = serialaxesGrob$vp,
    offset = loonWidgetsInfo$offset,
    clickInfo = plotHover,
    N = loonWidgetsInfo$N,
    native.x = loonWidgetsInfo$native.x,
    native.y = loonWidgetsInfo$native.y
  )

  # has to be active
  whichIsDeactive <- which(!loonWidgetsInfo$active)
  hoverId <- setdiff(hoverId, whichIsDeactive)

  if(length(hoverId) == 0) {
    NULL
  } else {
    itemLabels[hoverId]
  }
}
