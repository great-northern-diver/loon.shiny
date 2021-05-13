move_layer_invisible_grob <- function(loon_grob, current_layer, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("move_layer_invisible_grob", obj)
}


move_layer_invisible_grob.l_plot <- function(loon_grob, current_layer, ...) {

  if(current_layer == "scatterplot") {

    args <- list(...)
    pointsTree_name <- args$pointsTree_name
    N <- args$N

    set_deactive_grob(loon_grob, index = seq(N), pointsTree_name = pointsTree_name)

  } else {

    setGrob(
      gTree = loon_grob,
      gPath = current_layer,
      newGrob = set_deactive_layer(
        current_layer_grob = getGrob(loon_grob, current_layer)
      )
    )
  }
}

move_layer_invisible_grob.l_hist <- function(loon_grob, current_layer, ...) {

  if(current_layer == "histogram") {

    setGrob(
      gTree = loon_grob,
      gPath = current_layer,
      newGrob = grob(name = "histogram")
    )
  } else {

    setGrob(
      gTree = loon_grob,
      gPath = current_layer,
      newGrob = set_deactive_layer(
        current_layer_grob = getGrob(loon_grob, current_layer)
      )
    )
  }
}

move_layer_invisible_grob.l_graph <- function(loon_grob, current_layer, ...) {

  args <- list(...)
  N <- args$N
  
  if(current_layer == "graph") {

    set_deactive_grob(loon_grob, index = seq(N))

  } else {

    setGrob(
      gTree = loon_grob,
      gPath = current_layer,
      newGrob = set_deactive_layer(
        current_layer_grob = getGrob(loon_grob, current_layer)
      )
    )
  }
}

set_deactive_layer <- function(current_layer_grob) {

  if(str_detect(current_layer_grob$name, "l_layer_polygon:")) {

    do.call(grob, getGrobArgs(current_layer_grob))

  } else if(str_detect(current_layer_grob$name, "l_layer_line:")) {

    do.call(grob, getGrobArgs(current_layer_grob))

  } else if(str_detect(current_layer_grob$name, "l_layer_rectangle:")) {

    do.call(grob, getGrobArgs(current_layer_grob))

  } else if(str_detect(current_layer_grob$name, "l_layer_oval:")) {

    do.call(grob, getGrobArgs(current_layer_grob))

  } else if(str_detect(current_layer_grob$name, "l_layer_text:")) {

    do.call(grob, getGrobArgs(current_layer_grob))

  } else if(str_detect(current_layer_grob$name, "l_layer_points:")) {

    do.call(grob, getGrobArgs(current_layer_grob))

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
                 do.call(grob, args[[i]])
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
                 do.call(grob, args[[i]])
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
                 do.call(grob, args[[i]])
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
                 do.call(grob, args[[i]])
               })
      ),
      name = current_layer_grob$name,
      gp = current_layer_grob$gp,
      vp = current_layer_grob$vp
    )
  } else stop("unspecified layer name")
}
