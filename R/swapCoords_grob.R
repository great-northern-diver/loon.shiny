swapCoords_grob <- function(loon_grob, x, y, index, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("swapCoords_grob", obj)
}

swapCoords_grob.l_plot <- function(loon_grob, x, y, index = NULL, ...){

  # swap coordinates
  args <- list(...)
  pointsTree_name <- args$pointsTree_name

  if(pointsTree_name != "points: missing glyphs") {

    pointsTree <- getGrob(loon_grob, pointsTree_name)

    if(is.null(index)) {
      if(any(is.na(x)) | any(is.na(y))) {
        index <- intersect(
          which(!is.na(x)),
          which(!is.na(y))
        )
      } else index <- seq(length(pointsTree$children))
    }

    setGrob(
      gTree = loon_grob,
      gPath = pointsTree_name,
      newGrob = gTree(
        children = do.call(
          gList,
          lapply(1:length(pointsTree$children),
                 function(i){
                   grobi <- pointsTree$children[[i]]

                   if(i %in% index) {
                     # different glyphs have different type of grob, some of them are gTree, some of them are grobs
                     if(str_detect(grobi$name, "primitive_glyph")) {
                       editGrob(grobi,
                                x = unit(x[i], "native"),
                                y = unit(y[i], "native")
                       )
                     } else if(str_detect(grobi$name, "serialaxes_glyph")) {

                       polyline_grob <- getGrob(grobi, "polyline")
                       if(is.null(polyline_grob)) {
                         polyline_grob <- getGrob(grobi, "polyline: showArea")
                         polyline_grob_name <- "polyline: showArea"
                       } else polyline_grob_name <- "polyline"

                       grobi$children[[polyline_grob_name]] <- editGrob(polyline_grob,
                                                                        x = unit(x[i], "native") + 
                                                                          get_unit(polyline_grob$x, is.unit = FALSE, as.numeric = FALSE),
                                                                        y = unit(y[i], "native") + 
                                                                          get_unit(polyline_grob$y, is.unit = FALSE, as.numeric = FALSE)
                       )
                       grobi
                     } else if(str_detect(grobi$name, "polygon_glyph")) {

                       editGrob(grobi,
                                x = unit(x[i], "native") + 
                                  get_unit(grobi$x, is.unit = FALSE, as.numeric = FALSE),
                                y = unit(y[i], "native") + 
                                  get_unit(grobi$y, is.unit = FALSE, as.numeric = FALSE)
                       )
                     } else if(str_detect(grobi$name, "pointrange_glyph")) {

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
                     } else if(str_detect(grobi$name, "text_glyph")) {
                       editGrob(grobi,
                                x = unit(x[i], "native"),
                                y = unit(y[i], "native")
                       )
                     } else if(str_detect(grobi$name, "image_glyph")) {
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
        ), name = pointsTree_name
      )
    )
  } else loon_grob
}

swapCoords_grob.l_graph <- function(loon_grob, x, y, index = NULL, ...) {

  args <- list(...)
  reactive <- args$reactive

  edgesTree <- getGrob(loon_grob, "graph edges")

  if(is.null(index)) {
    if(any(is.na(x)) | any(is.na(y))) {
      
      index <- intersect(which(!is.na(x)), which(!is.na(y)))
    } else index <- seq(length(edgesTree$children))
  }

  loon_grob <- setGrob(
    gTree = loon_grob,
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

    pointsTree <- getGrob(loon_grob, "graph nodes")

    loon_grob <- setGrob(
      gTree = loon_grob,
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

    labelsTree <- getGrob(loon_grob, "graph labels")

    loon_grob <- setGrob(
      gTree = loon_grob,
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

    graph_grob <- getGrob(loon_grob, "graph")
    childrenOrder <- graph_grob$childrenOrder

    nav_path_points_id <- which(!childrenOrder %in% c("graph edges", "graph nodes", "graph labels"))

    if(length(nav_path_points_id) > 0) {

      lapply(nav_path_points_id,
             function(i){

               grobi <- graph_grob$children[[i]]

               loon_grob <<- setGrob(
                 gTree = loon_grob,
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

  loon_grob
}
