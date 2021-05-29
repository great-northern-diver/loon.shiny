reorder_grob <- function(loon.grob, number = NULL, index, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("reorder_grob", obj)
}

reorder_grob.l_plot <- function(loon.grob, number = NULL, index, ...) {

  if(length(index) == 0) return(loon.grob)

  args <- list(...)
  pointsTreeName <- args$pointsTreeName

  points_grob <- grid::getGrob(loon.grob, pointsTreeName)
  if(is.null(number)) number <- length(points_grob$children)

  grid::setGrob(
    gTree = loon.grob,
    gPath = pointsTreeName,
    newGrob = grid::reorderGrob(
      points_grob,
      c(setdiff(seq(number), index), index)
    )
  )
}

reorder_grob.l_graph <- function(loon.grob, number = NULL, index) {

  if(length(index) == 0) return(loon.grob)

  nodes_grob <- grid::getGrob(loon.grob, "graph nodes")
  if(is.null(number)) number <- length(nodes_grob$children)

  grid::setGrob(
    gTree = loon.grob,
    gPath = "graph nodes",
    newGrob = grid::reorderGrob(
      nodes_grob,
      order = c(setdiff(1:number, index), index)
    )
  )
}


reorder_grob.l_serialaxes <- function(loon.grob, number = NULL, index, ...) {

  if(length(index) == 0) return(loon.grob)

  args <- list(...)
  axesGpath <- args$axesGpath

  axesGrob <- grid::getGrob(loon.grob, axesGpath)
  if(is.null(number)) number <- length(axesGrob$children)

  grid::setGrob(
    gTree = loon.grob,
    gPath = axesGpath,
    newGrob = grid::reorderGrob(
      axesGrob,
      order = c(setdiff(1:number, index), index)
    )
  )
}
