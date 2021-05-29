move_layerVisible_grob <- function(loon.grob, currentLayer, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("move_layerVisible_grob", obj)
}


move_layerVisible_grob.l_plot <- function(loon.grob, currentLayer, ...) {

  if(currentLayer == "scatterplot") {

    args <- list(...)
    pointsTreeName <- args$pointsTreeName
    N <- args$N

    set_reactive_grob(loon.grob, index = seq(N), pointsTreeName = pointsTreeName)

  } else {

    grid::setGrob(
      gTree = loon.grob,
      gPath = currentLayer,
      newGrob = set_reactive_layer(
        currentLayer_grob = grid::getGrob(loon.grob, currentLayer)
      )
    )
  }
}

move_layerVisible_grob.l_hist <- function(loon.grob, currentLayer, ...) {

  if(currentLayer == "histogram") {

    loon.grob
  } else {

    grid::setGrob(
      gTree = loon.grob,
      gPath = currentLayer,
      newGrob = set_reactive_layer(
        currentLayer_grob = grid::getGrob(loon.grob, currentLayer)
      )
    )
  }
}

move_layerVisible_grob.l_graph <- function(loon.grob, currentLayer, ...) {

  if(currentLayer == "graph") {

    args <- list(...)
    N <- args$N
    graph_edges <- args$graph_edges

    set_reactive_grob(loon.grob, index = seq(N), graph_edges = graph_edges)

  } else {

    grid::setGrob(
      gTree = loon.grob,
      gPath = currentLayer,
      newGrob = set_reactive_layer(
        currentLayer_grob = grid::getGrob(loon.grob, currentLayer)
      )
    )
  }
}

set_reactive_layer <- function(currentLayer_grob) {

  if(grepl(currentLayer_grob$name, pattern = "l_layer_polygon:")) {

    do.call(grid::polygonGrob, getGrobArgs(currentLayer_grob))

  } else if(grepl(currentLayer_grob$name, pattern = "l_layer_line:")) {

    do.call(grid::linesGrob, getGrobArgs(currentLayer_grob))

  } else if(grepl(currentLayer_grob$name, pattern = "l_layer_rectangle:")) {

    do.call(grid::rectGrob, getGrobArgs(currentLayer_grob))

  } else if(grepl(currentLayer_grob$name, pattern = "l_layer_oval:")) {

    do.call(grid::polygonGrob, getGrobArgs(currentLayer_grob))

  } else if(grepl(currentLayer_grob$name, pattern = "l_layer_text:")) {

    do.call(grid::textGrob, getGrobArgs(currentLayer_grob))

  } else if(grepl(currentLayer_grob$name, pattern = "l_layer_points:")) {

    do.call(grid::pointsGrob, getGrobArgs(currentLayer_grob))

  } else if(grepl(currentLayer_grob$name, pattern = "l_layer_texts:")) {

    args <- list()
    lapply(1:length(currentLayer_grob$children),
           function(i) {
             args[[i]] <<- getGrobArgs(currentLayer_grob$children[[i]])
           }
    )

    gTree(
      children = do.call(
        gList,
        lapply(1:length(currentLayer_grob$children),
               function(i) {
                 do.call(grid::textGrob, args[[i]])
               })
      ),
      name = currentLayer_grob$name,
      gp = currentLayer_grob$gp,
      vp = currentLayer_grob$vp
    )
  } else if(grepl(currentLayer_grob$name,pattern =  "l_layer_polygons:")) {

    args <- list()
    lapply(1:length(currentLayer_grob$children),
           function(i) {
             args[[i]] <<- getGrobArgs(currentLayer_grob$children[[i]])
           }
    )

    gTree(
      children = do.call(
        gList,
        lapply(1:length(currentLayer_grob$children),
               function(i) {
                 do.call(grid::polygonGrob, args[[i]])
               })
      ),
      name = currentLayer_grob$name,
      gp = currentLayer_grob$gp,
      vp = currentLayer_grob$vp
    )
  } else if(grepl(currentLayer_grob$name, pattern = "l_layer_rectangles:")) {

    args <- list()
    lapply(1:length(currentLayer_grob$children),
           function(i) {
             args[[i]] <<- getGrobArgs(currentLayer_grob$children[[i]])
           }
    )

    gTree(
      children = do.call(
        gList,
        lapply(1:length(currentLayer_grob$children),
               function(i) {
                 do.call(grid::rectGrob, args[[i]])
               })
      ),
      name = currentLayer_grob$name,
      gp = currentLayer_grob$gp,
      vp = currentLayer_grob$vp
    )
  } else if(grepl(currentLayer_grob$name, pattern = "l_layer_lines:")) {

    args <- list()
    lapply(1:length(currentLayer_grob$children),
           function(i) {
             args[[i]] <<- getGrobArgs(currentLayer_grob$children[[i]])
           }
    )

    gTree(
      children = do.call(
        gList,
        lapply(1:length(currentLayer_grob$children),
               function(i) {
                 do.call(grid::linesGrob, args[[i]])
               })
      ),
      name = currentLayer_grob$name,
      gp = currentLayer_grob$gp,
      vp = currentLayer_grob$vp
    )
  } else stop("unspecified layer name")
}
