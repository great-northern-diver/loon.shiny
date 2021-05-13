set_deactive_grob <- function(loon_grob, index, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("set_deactive_grob", obj)
}

set_deactive_grob.l_plot <- function(loon_grob, index, ...) {

  args <- list(...)
  pointsTree_name <- args$pointsTree_name

  if(pointsTree_name != "points: missing glyphs" & length(index) != 0) {

    newGrob <- getGrob(loon_grob, pointsTree_name)

    lapply(index,
           function(i) {

             if(str_detect(newGrob$children[[i]]$name, "primitive_glyph")) {

               newGrob$children[[i]] <<- do.call(grob, getGrobArgs(newGrob$children[[i]]))

             } else if(str_detect(newGrob$children[[i]]$name, "pointrange_glyph")) {

               newGrob$children[[i]] <<- gTree(
                 children = gList(
                   # point
                   do.call(grob, getGrobArgs(getGrob(newGrob$children[[i]], "point"))),
                   # range
                   do.call(grob, getGrobArgs(getGrob(newGrob$children[[i]], "range")))
                 ),
                 name = newGrob$children[[i]]$name
               )

             } else if(str_detect(newGrob$children[[i]]$name, "text_glyph")) {

               newGrob$children[[i]] <<- do.call(grob, getGrobArgs(newGrob$children[[i]]))

             } else if(str_detect(newGrob$children[[i]]$name, "serialaxes_glyph")) {

               newGrob$children[[i]] <<- gTree(
                 children = gList(
                   do.call(grob, getGrobArgs(newGrob$children[[i]]$children[[1]])),
                   do.call(grob, getGrobArgs(newGrob$children[[i]]$children[[2]])),
                   do.call(grob, getGrobArgs(newGrob$children[[i]]$children[[3]]))
                 ),
                 name = newGrob$children[[i]]$name
               )

             } else if(str_detect(newGrob$children[[i]]$name, "polygon_glyph")) {

               newGrob$children[[i]] <<- do.call(grob, getGrobArgs(newGrob$children[[i]]))

             } else if(str_detect(newGrob$children[[i]]$name, "image_glyph")) {

               newGrob$children[[i]] <<- gTree(
                 children = gList(
                   do.call(grob, getGrobArgs(getGrob(newGrob$children[[i]], "image_border"))),
                   do.call(grob, getGrobArgs(getGrob(newGrob$children[[i]], "image")))
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
  } else {
    loon_grob
  }
}


set_deactive_grob.l_hist <- function(loon_grob, index, ...) {
  
  if(length(index) >0) {
    
    args <- list(...)
    yshows <- args$yshows
    active <- args$active 
    binId <- args$binId
    binX <- args$binX 
    binHeight <- args$binHeight
    binwidth <- args$binwidth
    n <- args$n 
    swapAxes <- args$swapAxes
    showStackedColors <- args$showStackedColors
    showOutlines <- args$showOutlines
    color <- args$color
    colorFill <- args$colorFill 
    colorOutline <- args$colorOutline
    
    
  } else {
    loon_grob
  }
}

set_deactive_grob.l_graph <- function(loon_grob, index) {
  
  if(length(index) > 0) {
    
    nodesGrob <- getGrob(loon_grob, "graph nodes")
    
    lapply(index,
           function(i) {
             nodesGrob$children[[i]] <<- do.call(grob, getGrobArgs(nodesGrob$children[[i]]))
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
             labelsGrob$children[[i]] <<- do.call(grob, getGrobArgs(labelsGrob$children[[i]]))
           }
    )
    
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "graph labels",
      newGrob = labelsGrob
    )
    
    
    edgesGrob <- getGrob(loon_grob, "graph edges")
    
    lapply(1:length(edgesGrob$children),
           function(i) {
             
             edgesGrob$children[[i]] <<- if(i %in% index) {
               do.call(grob, getGrobArgs(edgesGrob$children[[i]]))
             } else {
               
               grobi <- edgesGrob$children[[i]]
               
               if(!is.null(grobi$x) & !is.null(grobi$y)) {
                 
                 active_id <- which(!grobi$id %in% index)
                 
                 if(length(active_id) > 0) {
                   editGrob(
                     grob = grobi,
                     x = grobi$x[active_id],
                     y = grobi$y[active_id],
                     id = grobi$id[active_id],
                     gp = gpar(
                       col = grobi$gp$col[active_id[which(active_id <= length(grobi$gp$col))]],
                       lwd = edgesGrob$children[[i]]$gp$lwd
                     )
                   )
                 } else grob(name = grobi$name)
               } else grobi
             }
           }
    )
    
    setGrob(
      gTree = loon_grob,
      gPath = "graph edges",
      newGrob = edgesGrob
    )
  } else {
    loon_grob
  }
}


set_deactive_grob.l_serialaxes <- function(loon_grob, index, ...) {
  
  if(length(index) > 0) {
    
    args <- list(...)
    axes_gPath <- args$axes_gPath
    axes_grob <- getGrob(loon_grob, axes_gPath)
    
    lapply(index,
           function(i) {
             axes_grob$children[[i]] <<- do.call(grob, getGrobArgs(axes_grob$children[[i]]))
           }
    )
    
    setGrob(
      gTree = loon_grob,
      gPath = axes_gPath,
      newGrob = axes_grob
    )
  } else {
    loon_grob
  }
}
