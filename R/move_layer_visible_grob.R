move_layer_visible_grob <- function(loon_grob, current_layer, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("move_layer_visible_grob", obj)
}


move_layer_visible_grob.l_plot <- function(loon_grob, current_layer, ...) {

  if(current_layer == "scatterplot") {

    args <- list(...)
    pointsTree_name <- args$pointsTree_name
    N <- args$N

    set_reactive_grob(loon_grob, index = seq(N), pointsTree_name = pointsTree_name)

  } else {

    setGrob(
      gTree = loon_grob,
      gPath = current_layer,
      newGrob = set_reactive_layer(
        current_layer_grob = getGrob(loon_grob, current_layer)
      )
    )
  }
}

move_layer_visible_grob.l_hist <- function(loon_grob, current_layer, ...) {

  if(current_layer == "histogram") {
    
    loon_grob
  } else {

    setGrob(
      gTree = loon_grob,
      gPath = current_layer,
      newGrob = set_reactive_layer(
        current_layer_grob = getGrob(loon_grob, current_layer)
      )
    )
  }
}

move_layer_visible_grob.l_graph <- function(loon_grob, current_layer, ...) {

  if(current_layer == "graph") {

    args <- list(...)
    N <- args$N
    graph_edges <- args$graph_edges

    set_reactive_grob(loon_grob, index = seq(N), graph_edges = graph_edges)

  } else {

    setGrob(
      gTree = loon_grob,
      gPath = current_layer,
      newGrob = set_reactive_layer(
        current_layer_grob = getGrob(loon_grob, current_layer)
      )
    )
  }
}

set_reactive_layer <- function(current_layer_grob) {

  if(str_detect(current_layer_grob$name, "l_layer_polygon:")) {

    do.call(polygonGrob, getGrobArgs(current_layer_grob))

  } else if(str_detect(current_layer_grob$name, "l_layer_line:")) {

    do.call(linesGrob, getGrobArgs(current_layer_grob))

  } else if(str_detect(current_layer_grob$name, "l_layer_rectangle:")) {

    do.call(rectGrob, getGrobArgs(current_layer_grob))

  } else if(str_detect(current_layer_grob$name, "l_layer_oval:")) {

    do.call(polygonGrob, getGrobArgs(current_layer_grob))

  } else if(str_detect(current_layer_grob$name, "l_layer_text:")) {

    do.call(textGrob, getGrobArgs(current_layer_grob))

  } else if(str_detect(current_layer_grob$name, "l_layer_points:")) {

    do.call(pointsGrob, getGrobArgs(current_layer_grob))

  } else if(str_detect(current_layer_grob$name, "l_layer_texts:")) {

    args <- list()
    lapply(1:length(current_layer_grob$children),
           function(i) {
             args[[i]] <<- getGrobArgs(current_layer_grob$children[[i]])
           }
    )

    gTree(
      children = do.call(
        gList,
        lapply(1:length(current_layer_grob$children),
               function(i) {
                 do.call(textGrob, args[[i]])
               })
      ),
      name = current_layer_grob$name,
      gp = current_layer_grob$gp,
      vp = current_layer_grob$vp
    )
  } else if(str_detect(current_layer_grob$name, "l_layer_polygons:")) {

    args <- list()
    lapply(1:length(current_layer_grob$children),
           function(i) {
             args[[i]] <<- getGrobArgs(current_layer_grob$children[[i]])
           }
    )

    gTree(
      children = do.call(
        gList,
        lapply(1:length(current_layer_grob$children),
               function(i) {
                 do.call(polygonGrob, args[[i]])
               })
      ),
      name = current_layer_grob$name,
      gp = current_layer_grob$gp,
      vp = current_layer_grob$vp
    )
  } else if(str_detect(current_layer_grob$name, "l_layer_rectangles:")) {

    args <- list()
    lapply(1:length(current_layer_grob$children),
           function(i) {
             args[[i]] <<- getGrobArgs(current_layer_grob$children[[i]])
           }
    )

    gTree(
      children = do.call(
        gList,
        lapply(1:length(current_layer_grob$children),
               function(i) {
                 do.call(rectGrob, args[[i]])
               })
      ),
      name = current_layer_grob$name,
      gp = current_layer_grob$gp,
      vp = current_layer_grob$vp
    )
  } else if(str_detect(current_layer_grob$name, "l_layer_lines:")) {

    args <- list()
    lapply(1:length(current_layer_grob$children),
           function(i) {
             args[[i]] <<- getGrobArgs(current_layer_grob$children[[i]])
           }
    )

    gTree(
      children = do.call(
        gList,
        lapply(1:length(current_layer_grob$children),
               function(i) {
                 do.call(linesGrob, args[[i]])
               })
      ),
      name = current_layer_grob$name,
      gp = current_layer_grob$gp,
      vp = current_layer_grob$vp
    )
  } else stop("unspecified layer name")
}
