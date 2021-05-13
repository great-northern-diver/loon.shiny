set_reactive_grob <- function(loon_grob, index, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("set_reactive_grob", obj)
}

set_reactive_grob.l_plot <- function(loon_grob, index, ...) {

  args <- list(...)
  pointsTree_name <- args$pointsTree_name

  if(pointsTree_name != "points: missing glyphs" & length(index) != 0) {
    newGrob <- getGrob(loon_grob, pointsTree_name)

    lapply(index,
           function(i) {

             if(str_detect(newGrob$children[[i]]$name, "primitive_glyph")) {

               newGrob$children[[i]] <<- do.call(pointsGrob, getGrobArgs(newGrob$children[[i]]))

             } else if(str_detect(newGrob$children[[i]]$name, "pointrange_glyph")) {

               newGrob$children[[i]] <<- gTree(
                 children = gList(
                   # point
                   do.call(pointsGrob, getGrobArgs(getGrob(newGrob$children[[i]], "point"))),
                   # range
                   do.call(linesGrob, getGrobArgs(getGrob(newGrob$children[[i]], "range")))
                 ),
                 name = newGrob$children[[i]]$name
               )

             } else if(str_detect(newGrob$children[[i]]$name, "text_glyph")) {

               newGrob$children[[i]] <<- do.call(textGrob, getGrobArgs(newGrob$children[[i]]))

             } else if(str_detect(newGrob$children[[i]]$name, "serialaxes_glyph")) {

               gTree_names <- newGrob$children[[i]]$childrenOrder

               newGrob$children[[i]] <<- if("polyline" %in% gTree_names) {

                 setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "polyline",
                   newGrob = do.call(linesGrob, getGrobArgs(getGrob(newGrob$children[[i]], "polyline")))
                 )
               } else if("polyline: showArea" %in% gTree_names) {

                 setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "polyline: showArea",
                   newGrob = do.call(polygonGrob, getGrobArgs(getGrob(newGrob$children[[i]], "polyline: showArea")))
                 )
               } else stop("serialaxes name does not match")

               if("boundary" %in% gTree_names) {

                 newGrob$children[[i]] <<- setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "boundary",
                   newGrob = do.call(polylineGrob, getGrobArgs(getGrob(newGrob$children[[i]], "boundary")))
                 )
               }

               if("axes" %in% gTree_names) {

                 newGrob$children[[i]] <<- setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "axes",
                   newGrob = do.call(polylineGrob, getGrobArgs(getGrob(newGrob$children[[i]], "axes")))
                 )
               }

             } else if(str_detect(newGrob$children[[i]]$name, "polygon_glyph")) {

               newGrob$children[[i]] <<- if(str_detect(newGrob$children[[i]]$name, "showArea")) {

                 do.call(polygonGrob, getGrobArgs(newGrob$children[[i]]))
               } else {

                 do.call(polylineGrob, getGrobArgs(newGrob$children[[i]]))
               }

             } else if(str_detect(newGrob$children[[i]]$name, "image_glyph")) {

               # in rasterGrob, the first argument is "image", however, in output list, the first argument is "raster"
               # we need to rename the arguments
               raster_args <- getGrobArgs(getGrob(newGrob$children[[i]], "image"))
               names(raster_args) <- c("image", names(raster_args)[-1])

               newGrob$children[[i]] <<- gTree(
                 children = gList(
                   do.call(rectGrob, getGrobArgs(getGrob(newGrob$children[[i]], "image_border"))),
                   do.call(rasterGrob, raster_args)
                 ),
                 name = newGrob$children[[i]]$name
               )

             } else stop("not inplemented")
           }
    )

    setGrob(
      gTree = loon_grob,
      gPath = pointsTree_name,
      newGrob = newGrob
    )
  } else loon_grob
}

set_reactive_grob.l_graph <- function(loon_grob, index, ...) {
  
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
                     editGrob(grobi,
                              x = grobi$y,
                              y = grobi$x
                     )
                   } else grobi
                 }
          )
        ), name = "graph edges"
      )
    }
    
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "graph edges",
      newGrob = edgesTree
    )
    
    nodesGrob <- getGrob(loon_grob, "graph nodes")
    
    lapply(index,
           function(i) {
             nodesGrob$children[[i]] <<- do.call(pointsGrob, getGrobArgs(nodesGrob$children[[i]]))
           }
    )
    
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "graph nodes",
      newGrob = nodesGrob
    )
    
    labelsGrob <- getGrob(loon_grob, "graph labels")
    
    lapply(index,
           function(i) {
             labelsGrob$children[[i]] <<- do.call(textGrob, getGrobArgs(labelsGrob$children[[i]]))
           }
    )
    
    setGrob(
      gTree = loon_grob,
      gPath = "graph labels",
      newGrob = labelsGrob
    )
    
  } else loon_grob
}

set_reactive_grob.l_serialaxes <- function(loon_grob, index, ...) {
  
  if(length(index) > 0) {
    args <- list(...)
    axes_gPath <- args$axes_gPath
    showArea <- args$showArea
    
    axes_grob <- getGrob(loon_grob, axes_gPath)
    
    lapply(index,
           function(i) {
             axes_grob$children[[i]] <<- if(showArea) {
               do.call(polygonGrob, getGrobArgs(axes_grob$children[[i]]))
             } else {
               do.call(linesGrob, getGrobArgs(axes_grob$children[[i]]))
             }
           }
    )
    
    setGrob(
      gTree = loon_grob,
      gPath = axes_gPath,
      newGrob = axes_grob
    )
  } else loon_grob
}
