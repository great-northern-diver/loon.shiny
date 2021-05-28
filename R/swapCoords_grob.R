swapCoords_grob <- function(loon.grob, x, y, index, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("swapCoords_grob", obj)
}

swapCoords_grob.l_plot <- function(loon.grob, x, y, index = NULL, ...){

  # swap coordinates
  args <- list(...)
  pointsTreeName <- args$pointsTreeName

  if(pointsTreeName != "points: missing glyphs") {

    pointsTree <- grid::getGrob(loon.grob, pointsTreeName)

    if(is.null(index)) {
      if(any(is.na(x)) | any(is.na(y))) {
        index <- intersect(
          which(!is.na(x)),
          which(!is.na(y))
        )
      } else index <- seq(length(pointsTree$children))
    }

    grid::setGrob(
      gTree = loon.grob,
      gPath = pointsTreeName,
      newGrob = gTree(
        children = do.call(
          gList,
          lapply(1:length(pointsTree$children),
                 function(i){
                   grobi <- pointsTree$children[[i]]

                   if(i %in% index) {
                     # different glyphs have different type of grob, some of them are gTree, some of them are grobs
                     if(grepl(grobi$name, pattern = "primitive_glyph")) {
                       editGrob(grobi,
                                x = unit(x[i], "native"),
                                y = unit(y[i], "native")
                       )
                     } else if(grepl(grobi$name, pattern = "serialaxes_glyph")) {

                       polyline_grob <- grid::getGrob(grobi, "polyline")
                       if(is.null(polyline_grob)) {
                         polyline_grob <- grid::getGrob(grobi, "polyline: showArea")
                         polyline_grob_name <- "polyline: showArea"
                       } else polyline_grob_name <- "polyline"

                       grobi$children[[polyline_grob_name]] <- editGrob(polyline_grob,
                                                                        x = unit(x[i], "native") +
                                                                          get_unit(polyline_grob$x, is.unit = FALSE, as.numeric = FALSE),
                                                                        y = unit(y[i], "native") +
                                                                          get_unit(polyline_grob$y, is.unit = FALSE, as.numeric = FALSE)
                       )
                       grobi
                     } else if(grepl(grobi$name, pattern = "polygon_glyph")) {

                       editGrob(grobi,
                                x = unit(x[i], "native") +
                                  get_unit(grobi$x, is.unit = FALSE, as.numeric = FALSE),
                                y = unit(y[i], "native") +
                                  get_unit(grobi$y, is.unit = FALSE, as.numeric = FALSE)
                       )
                     } else if(grepl(grobi$name, pattern = "pointrange_glyph")) {

                       # modify point
                       grobi$children$point <- editGrob(grobi$children$point,
                                                        x = unit(x[i], "native"),
                                                        y = unit(y[i], "native")
                       )
                       # modify range
                       grobi$children$range <- editGrob(grobi$children$range,
                                                        x = grobi$children$range$y,
                                                        y = grobi$children$range$x
                       )
                       grobi
                     } else if(grepl(grobi$name, pattern = "text_glyph")) {
                       editGrob(grobi,
                                x = unit(x[i], "native"),
                                y = unit(y[i], "native")
                       )
                     } else if(grepl(grobi$name, pattern = "image_glyph")) {
                       grobi$children$image_border <- editGrob(grobi$children$image_border,
                                                               x = grobi$children$image_border$y,
                                                               y = grobi$children$image_border$x,
                                                               width = grobi$children$image_border$height,
                                                               height = grobi$children$image_border$width
                       )

                       grobi$children$image <- editGrob(grobi$children$image,
                                                        x = grobi$children$image$y,
                                                        y = grobi$children$image$x,
                                                        width = grobi$children$image$height,
                                                        height = grobi$children$image$width
                       )
                       grobi
                     } else NULL

                   } else {
                     grobi
                   }
                 }
          )
        ), name = pointsTreeName
      )
    )
  } else loon.grob
}

swapCoords_grob.l_graph <- function(loon.grob, x, y, index = NULL, ...) {

  args <- list(...)
  reactive <- args$reactive

  edgesTree <- grid::getGrob(loon.grob, "graph edges")

  if(is.null(index)) {
    if(any(is.na(x)) | any(is.na(y))) {

      index <- intersect(which(!is.na(x)), which(!is.na(y)))
    } else index <- seq(length(edgesTree$children))
  }

  loon.grob <- grid::setGrob(
    gTree = loon.grob,
    gPath = "graph edges",
    newGrob = gTree(
      children = do.call(
        gList,
        lapply(1:length(edgesTree$children),
               function(i){

                 grobi <- edgesTree$children[[i]]
                 if(i %in% index) {
                   if(!is.null(grobi$x) & !is.null(grobi$y)) {
                     editGrob(grobi,
                              x = grobi$y,
                              y = grobi$x
                     )
                   } else grobi
                 } else grobi
               }
        )
      ), name = "graph edges"
    )
  )

  if(!reactive) {

    pointsTree <- grid::getGrob(loon.grob, "graph nodes")

    loon.grob <- grid::setGrob(
      gTree = loon.grob,
      gPath = "graph nodes",
      newGrob = gTree(
        children = do.call(
          gList,
          lapply(1:length(pointsTree$children),
                 function(i){

                   grobi <- pointsTree$children[[i]]
                   if(i %in% index) {

                     editGrob(grobi,
                              x = unit(x[i], "native"),
                              y = unit(y[i], "native")
                     )
                   } else grobi
                 }
          )
        ), name = "graph nodes"
      )
    )

    labelsTree <- grid::getGrob(loon.grob, "graph labels")

    loon.grob <- grid::setGrob(
      gTree = loon.grob,
      gPath = "graph labels",
      newGrob = gTree(
        children = do.call(
          gList,
          lapply(1:length(labelsTree$children),
                 function(i){

                   grobi <- labelsTree$children[[i]]
                   if(i %in% index) {
                     if(!is.null(grobi$x) & !is.null(grobi$y)) {
                       editGrob(grobi,
                                x = grobi$y,
                                y = grobi$x
                       )
                     } else grobi
                   } else grobi
                 }
          )
        ), name = "graph labels"
      )
    )

    graph_grob <- grid::getGrob(loon.grob, "graph")
    childrenOrder <- graph_grob$childrenOrder

    nav_path_points_id <- which(!childrenOrder %in% c("graph edges", "graph nodes", "graph labels"))

    if(length(nav_path_points_id) > 0) {

      lapply(nav_path_points_id,
             function(i){

               grobi <- graph_grob$children[[i]]

               loon.grob <<- grid::setGrob(
                 gTree = loon.grob,
                 gPath = grobi$name,
                 newGrob = gTree(
                   children = do.call(
                     gList,
                     lapply(1:length(grobi$children),
                            function(j){

                              grobij <- grobi$children[[j]]

                              if(!is.null(grobij$x) & !is.null(grobij$y)) {
                                editGrob(
                                  grobij,
                                  x = grobij$y,
                                  y = grobij$x
                                )
                              } else grobij
                            }
                     )
                   ), name = grobi$name
                 )
               )
             }
      )
    }
  }

  loon.grob
}
