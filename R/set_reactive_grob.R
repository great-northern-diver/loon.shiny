set_reactive_grob <- function(loon.grob, index, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_reactive_grob", obj)
}

set_reactive_grob.l_plot <- function(loon.grob, index, ...) {

  args <- list(...)
  pointsTreeName <- args$pointsTreeName

  if(pointsTreeName != "points: missing glyphs" & length(index) != 0) {
    newGrob <- grid::getGrob(loon.grob, pointsTreeName)

    lapply(index,
           function(i) {

             if(grepl(newGrob$children[[i]]$name, pattern = "primitive_glyph")) {

               newGrob$children[[i]] <<- do.call(pointsGrob, getGrobArgs(newGrob$children[[i]]))

             } else if(grepl(newGrob$children[[i]]$name, pattern = "pointrange_glyph")) {

               newGrob$children[[i]] <<- gTree(
                 children = gList(
                   # point
                   do.call(pointsGrob, getGrobArgs(grid::getGrob(newGrob$children[[i]], "point"))),
                   # range
                   do.call(grid::linesGrob, getGrobArgs(grid::getGrob(newGrob$children[[i]], "range")))
                 ),
                 name = newGrob$children[[i]]$name
               )

             } else if(grepl(newGrob$children[[i]]$name, pattern = "text_glyph")) {

               newGrob$children[[i]] <<- do.call(grid::textGrob, getGrobArgs(newGrob$children[[i]]))

             } else if(grepl(newGrob$children[[i]]$name, pattern = "serialaxes_glyph")) {

               gTree_names <- newGrob$children[[i]]$childrenOrder

               newGrob$children[[i]] <<- if("polyline" %in% gTree_names) {

                 grid::setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "polyline",
                   newGrob = do.call(grid::linesGrob, getGrobArgs(grid::getGrob(newGrob$children[[i]], "polyline")))
                 )
               } else if("polyline: showArea" %in% gTree_names) {

                 grid::setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "polyline: showArea",
                   newGrob = do.call(grid::polygonGrob, getGrobArgs(grid::getGrob(newGrob$children[[i]], "polyline: showArea")))
                 )
               } else stop("serialaxes name does not match")

               if("boundary" %in% gTree_names) {

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "boundary",
                   newGrob = do.call(grid::polylineGrob, getGrobArgs(grid::getGrob(newGrob$children[[i]], "boundary")))
                 )
               }

               if("axes" %in% gTree_names) {

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "axes",
                   newGrob = do.call(grid::polylineGrob, getGrobArgs(grid::getGrob(newGrob$children[[i]], "axes")))
                 )
               }

             } else if(grepl(newGrob$children[[i]]$name, pattern = "polygon_glyph")) {

               newGrob$children[[i]] <<- if(grepl(newGrob$children[[i]]$name, pattern = "showArea")) {

                 do.call(grid::polygonGrob, getGrobArgs(newGrob$children[[i]]))
               } else {

                 do.call(grid::polylineGrob, getGrobArgs(newGrob$children[[i]]))
               }

             } else if(grepl(newGrob$children[[i]]$name, pattern = "image_glyph")) {

               # in rasterGrob, the first argument is "image", however, in output list, the first argument is "raster"
               # we need to rename the arguments
               raster_args <- getGrobArgs(grid::getGrob(newGrob$children[[i]], "image"))
               names(raster_args) <- c("image", names(raster_args)[-1])

               newGrob$children[[i]] <<- gTree(
                 children = gList(
                   do.call(grid::rectGrob, getGrobArgs(grid::getGrob(newGrob$children[[i]], "image_border"))),
                   do.call(rasterGrob, raster_args)
                 ),
                 name = newGrob$children[[i]]$name
               )

             } else stop("not inplemented")
           }
    )

    grid::setGrob(
      gTree = loon.grob,
      gPath = pointsTreeName,
      newGrob = newGrob
    )
  } else loon.grob
}

set_reactive_grob.l_graph <- function(loon.grob, index, ...) {

  if(length(index) > 0) {
    args <- list(...)
    swap <- args$swap
    edgesTree <- args$graph_edges

    if(swap) {

      edgesTree <- gTree(
        children = do.call(
          gList,
          lapply(1:length(edgesTree$children),
                 function(i){

                   grobi <- edgesTree$children[[i]]
                   if(!is.null(grobi$x) & !is.null(grobi$y)) {
                     grid::editGrob(grobi,
                              x = grobi$y,
                              y = grobi$x
                     )
                   } else grobi
                 }
          )
        ), name = "graph edges"
      )
    }

    loon.grob <- grid::setGrob(
      gTree = loon.grob,
      gPath = "graph edges",
      newGrob = edgesTree
    )

    nodesGrob <- grid::getGrob(loon.grob, "graph nodes")

    lapply(index,
           function(i) {
             nodesGrob$children[[i]] <<- do.call(pointsGrob, getGrobArgs(nodesGrob$children[[i]]))
           }
    )

    loon.grob <- grid::setGrob(
      gTree = loon.grob,
      gPath = "graph nodes",
      newGrob = nodesGrob
    )

    labelsGrob <- grid::getGrob(loon.grob, "graph labels")

    lapply(index,
           function(i) {
             labelsGrob$children[[i]] <<- do.call(grid::textGrob, getGrobArgs(labelsGrob$children[[i]]))
           }
    )

    grid::setGrob(
      gTree = loon.grob,
      gPath = "graph labels",
      newGrob = labelsGrob
    )

  } else loon.grob
}

set_reactive_grob.l_serialaxes <- function(loon.grob, index, ...) {

  if(length(index) > 0) {
    args <- list(...)
    axesGpath <- args$axesGpath
    showArea <- args$showArea

    axesGrob <- grid::getGrob(loon.grob, axesGpath)

    lapply(index,
           function(i) {
             axesGrob$children[[i]] <<- if(showArea) {
               do.call(grid::polygonGrob, getGrobArgs(axesGrob$children[[i]]))
             } else {
               do.call(grid::linesGrob, getGrobArgs(axesGrob$children[[i]]))
             }
           }
    )

    grid::setGrob(
      gTree = loon.grob,
      gPath = axesGpath,
      newGrob = axesGrob
    )
  } else loon.grob
}
