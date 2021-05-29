move_layerInvisible_grob <- function(loon.grob, currentLayer, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("move_layerInvisible_grob", obj)
}


move_layerInvisible_grob.l_plot <- function(loon.grob, currentLayer, ...) {

  if(currentLayer == "scatterplot") {

    args <- list(...)
    pointsTreeName <- args$pointsTreeName
    N <- args$N

    set_deactive_grob(loon.grob, index = seq(N), pointsTreeName = pointsTreeName)

  } else {

    grid::setGrob(
      gTree = loon.grob,
      gPath = currentLayer,
      newGrob = set_deactive_layer(
        currentLayer_grob = grid::getGrob(loon.grob, currentLayer)
      )
    )
  }
}

move_layerInvisible_grob.l_hist <- function(loon.grob, currentLayer, ...) {

  if(currentLayer == "histogram") {

    grid::setGrob(
      gTree = loon.grob,
      gPath = currentLayer,
      newGrob = grob(name = "histogram")
    )
  } else {

    grid::setGrob(
      gTree = loon.grob,
      gPath = currentLayer,
      newGrob = set_deactive_layer(
        currentLayer_grob = grid::getGrob(loon.grob, currentLayer)
      )
    )
  }
}

move_layerInvisible_grob.l_graph <- function(loon.grob, currentLayer, ...) {

  args <- list(...)
  N <- args$N

  if(currentLayer == "graph") {

    set_deactive_grob(loon.grob, index = seq(N))

  } else {

    grid::setGrob(
      gTree = loon.grob,
      gPath = currentLayer,
      newGrob = set_deactive_layer(
        currentLayer_grob = grid::getGrob(loon.grob, currentLayer)
      )
    )
  }
}

set_deactive_layer <- function(currentLayer_grob) {

  if(grepl(currentLayer_grob$name, pattern = "l_layer_polygon:")) {

    do.call(grob, getGrobArgs(currentLayer_grob))

  } else if(grepl(currentLayer_grob$name,pattern =  "l_layer_line:")) {

    do.call(grob, getGrobArgs(currentLayer_grob))

  } else if(grepl(currentLayer_grob$name,pattern =  "l_layer_rectangle:")) {

    do.call(grob, getGrobArgs(currentLayer_grob))

  } else if(grepl(currentLayer_grob$name, pattern = "l_layer_oval:")) {

    do.call(grob, getGrobArgs(currentLayer_grob))

  } else if(grepl(currentLayer_grob$name, pattern = "l_layer_text:")) {

    do.call(grob, getGrobArgs(currentLayer_grob))

  } else if(grepl(currentLayer_grob$name,pattern =  "l_layer_points:")) {

    do.call(grob, getGrobArgs(currentLayer_grob))

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
                 do.call(grob, args[[i]])
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
                 do.call(grob, args[[i]])
               })
      ),
      name = currentLayer_grob$name,
      gp = currentLayer_grob$gp,
      vp = currentLayer_grob$vp
    )
  } else if(grepl(currentLayer_grob$name,pattern =  "l_layer_rectangles:")) {

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
                 do.call(grob, args[[i]])
               })
      ),
      name = currentLayer_grob$name,
      gp = currentLayer_grob$gp,
      vp = currentLayer_grob$vp
    )
  } else if(grepl(currentLayer_grob$name,pattern =  "l_layer_lines:")) {

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
                 do.call(grob, args[[i]])
               })
      ),
      name = currentLayer_grob$name,
      gp = currentLayer_grob$gp,
      vp = currentLayer_grob$vp
    )
  } else stop("unspecified layer name")
}
