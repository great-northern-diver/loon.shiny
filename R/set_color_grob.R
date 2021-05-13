set_color_grob <- function(loon_grob, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("set_color_grob", obj)
}

set_color_grob.l_plot <- function(loon_grob, ...) {

  args <- list(...)
  index <- args$index
  pointsTree_name <- args$pointsTree_name
  
  if(pointsTree_name != "points: missing glyphs" & length(index) > 0) {
    
    color <- args$color
    size <- args$size
    pch <- args$pch
    loon_color <- args$loon_color
    
    if(length(color) == 1) {
      color <- rep(color, length(index))
    } else if (length(color) != length(index)) {
      stop("color length is not equal to index length")
    } else NULL
    
    newGrob <- getGrob(loon_grob, pointsTree_name)
    
    lapply(index,
           function(i) {
             
             grobi <- newGrob$children[[i]]
             
             newGrob$children[[i]] <<- if(str_detect(grobi$name, "primitive_glyph")) {
               
               pch_i <- if(is.null(pch)) grobi$pch else pch[i]
               size_i <- if(is.null(size)) grobi$gp$cex else size[i]
               
               editGrob(
                 grob = grobi,
                 gp = if(pch_i %in% 21:24) {
                   gpar(
                     fill = color[which(index %in% i)],
                     cex = size_i,
                     col = loon_color$foreground_color[1]
                   )
                 } else {
                   gpar(
                     col = color[which(index %in% i)],
                     cex = size_i
                   )
                 }
               )
             } else if(str_detect(grobi$name, "serialaxes_glyph"))  {
               
               polyline_grob <- getGrob(grobi, "polyline")
               if(is.null(polyline_grob)) {
                 polyline_grob <- getGrob(grobi, "polyline: showArea")
                 polyline_grob_name <-  "polyline: showArea"
                 polyline_grob$gp$fill <- color[which(index %in% i)]
               } else {
                 polyline_grob_name <-  "polyline"
                 polyline_grob$gp$col <- color[which(index %in% i)]
               }
               
               setGrob(
                 gTree = grobi,
                 gPath = polyline_grob_name,
                 newGrob = polyline_grob
               )
               
             } else if(str_detect(grobi$name, "polygon_glyph")) {
               
               editGrob(
                 grob = grobi,
                 gp = gpar(
                   fill = color[which(index %in% i)],
                   col = color[which(index %in% i)],
                   fontsize = grobi$gp$lwd
                 )
               )
               
             } else if(str_detect(grobi$name, "pointrange_glyph")) {
               
               point_grob <- getGrob(grobi, "point")
               line_grob <- getGrob(grobi, "range")
               
               point_grob$gp$col <- color[which(index %in% i)]
               line_grob$gp$col <- color[which(index %in% i)]
               
               tmpGrob <- setGrob(
                 gTree = grobi,
                 gPath = "point",
                 newGrob = point_grob
               )
               
               setGrob(
                 gTree = tmpGrob,
                 gPath = "range",
                 newGrob = line_grob
               )
               
             } else if(str_detect(grobi$name, "text_glyph"))  {
               
               editGrob(
                 grob = grobi,
                 gp = gpar(
                   col = color[which(index %in% i)],
                   fontsize = size[which(index %in% i)] * loon_default_size()[["adjusted_size"]]
                 )
               )
               
             } else if(str_detect(grobi$name, "image_glyph")) {
               
               setGrob(
                 gTree = grobi,
                 gPath = "image_border",
                 newGrob = editGrob(
                   grob = getGrob(grobi, "image_border"),
                   gp = gpar(
                     fill = color[which(index %in% i)],
                     col =  NA
                   )
                 )
               )
               
             } else {warning("Not implemented glyph"); grobi}
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

set_color_grob.l_hist <- function(loon_grob, ...) {
  
  args <- list(...)
  index <- args$index
  
  if(length(index) > 0) {
    
    color <- args$color
    changeColorWay <- args$changeColorWay
    loon_color <- args$loon_color
    
    if(length(color) == 1) {
      color <- rep(color, length(index))
    } else if (length(color) != length(index)) {
      stop("color length is not equal to index length")
    } else NULL
    
    newGrob <- getGrob(loon_grob, "histogram")
    
    lapply(index,
           function(i){
             newGrob$children[[i]] <<- editGrob(
               grob = newGrob$children[[i]],
               gp = if(changeColorWay == "fill") {
                 gpar(
                   fill = color[which(index %in% i)],
                   col = loon_color$foreground_color[1]
                 )
               } else if(changeColorWay == "col") {
                 gpar(
                   col = color[which(index %in% i)]
                 )
               }
             )
           }
    )
    
    setGrob(
      gTree = loon_grob,
      gPath = "histogram",
      newGrob = newGrob
    )
  } else {
    loon_grob
  }
}

set_color_grob.l_graph <- function(loon_grob, ...) {

  args <- list(...)
  index <- args$index
  
  if(length(index) > 0) {
    
    color <- args$color
    size <- args$size
    pch <- args$pch
    
    if(length(color) == 1) {
      color <- rep(color, length(index))
    } else if (length(color) != length(index)) {
      stop("color length is not equal to index length")
    } else NULL
    
    loon_color <- args$loon_color
    
    newGrob <- getGrob(loon_grob, "graph nodes")
    
    lapply(index,
           function(i) {
             
             grobi <- newGrob$children[[i]]
             
             pch_i <- if(is.null(pch)) grobi$pch else pch[i]
             size_i <- if(is.null(size)) grobi$gp$cex else size[i]
             
             newGrob$children[[i]] <<- editGrob(
               grob = grobi,
               gp = if(pch_i %in% 21:24) {
                 gpar(
                   fill = color[which(index %in% i)],
                   cex = size_i,
                   col = loon_color$foreground_color[1]
                 )
               } else {
                 gpar(
                   col = color[which(index %in% i)],
                   cex = size_i
                 )
               }
             )
           }
    )
    
    setGrob(
      gTree = loon_grob,
      gPath = "graph nodes",
      newGrob = newGrob
    )
  } else {
    loon_grob
  }
}

set_color_grob.l_serialaxes <- function(loon_grob, ...) {

  args <- list(...)
  index <- args$index
  
  if(length(index) > 0) {
    color <- args$color
    axes_gPath <- args$axes_gPath
    axes_grob <- getGrob(loon_grob, axes_gPath)
    
    if(length(color) == 1) {
      color <- rep(color, length(index))
    } else if (length(color) != length(index)) {
      stop("color length is not equal to index length")
    } else NULL
    
    lapply(index,
           function(i) {
             
             grobi <- axes_grob$children[[i]]
             axes_grob$children[[i]] <<- editGrob(
               grob = grobi,
               gp = if(str_detect(grobi$name, "showArea")) {
                 gpar(fill = color[which(index %in% i)], col = NA)
               } else {
                 gpar(col = color[which(index %in% i)], lwd = grobi$gp$lwd)
               }
             )
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
