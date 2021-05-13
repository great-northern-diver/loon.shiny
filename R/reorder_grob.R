reorder_grob <- function(loon_grob, number = NULL, index, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("reorder_grob", obj)
}

reorder_grob.l_plot <- function(loon_grob, number = NULL, index, ...) {
  
  if(length(index) == 0) return(loon_grob)
  
  args <- list(...)
  pointsTree_name <- args$pointsTree_name
  
  points_grob <- getGrob(loon_grob, pointsTree_name)
  if(is.null(number)) number <- length(points_grob$children)
  
  setGrob(
    gTree = loon_grob,
    gPath = pointsTree_name,
    newGrob = reorderGrob(
      getGrob(loon_grob, pointsTree_name),
      order = c(setdiff(1:number, index), index)
    )
  )
}

reorder_grob.l_graph <- function(loon_grob, number = NULL, index) {
  
  if(length(index) == 0) return(loon_grob)
  
  nodes_grob <- getGrob(loon_grob, "graph nodes")
  if(is.null(number)) number <- length(nodes_grob$children)
  
  setGrob(
    gTree = loon_grob,
    gPath = "graph nodes",
    newGrob = reorderGrob(
      getGrob(loon_grob, "graph nodes"),
      order = c(setdiff(1:number, index), index)
    )
  )
}


reorder_grob.l_serialaxes <- function(loon_grob, number = NULL, index, ...) {
  
  if(length(index) == 0) return(loon_grob)
  
  args <- list(...)
  axes_gPath <- args$axes_gPath
  
  axes_grob <- getGrob(loon_grob, axes_gPath)
  if(is.null(number)) number <- length(axes_grob$children)
  
  setGrob(
    gTree = loon_grob,
    gPath = axes_gPath,
    newGrob = reorderGrob(
      axes_grob,
      order = c(setdiff(1:number, index), index)
    )
  )
}
