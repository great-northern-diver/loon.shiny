set_serialAxes_area_grob <- function(loon_grob, pointsTree_name, glyph_name, set_area, swap = NULL, which_is_deactive) {
  
  newGrob <- getGrob(loon_grob, pointsTree_name)
  
  serialaxes_and_active <- setdiff(which(str_detect(glyph_name, "serialaxes")), which_is_deactive)
  
  if(length(serialaxes_and_active) > 0) {
    
    lapply(serialaxes_and_active,
           function(i) {
             serialaxes_tree <- newGrob$children[[i]]
             
             serialaxes_grob <- getGrob(newGrob$children[[i]], "polyline")
             if(is.null(serialaxes_grob)) {
               serialaxes_grob <- getGrob(newGrob$children[[i]], "polyline: showArea")
               serialaxes_grob_name <-  "polyline: showArea"
             } else {
               serialaxes_grob_name <-  "polyline"
             }
             
             if(str_detect(serialaxes_tree$name, "parallel")) {
               if(set_area) {
                 if(!is(serialaxes_grob, "polygon")) {
                   newGrob$children[[i]] <<- setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = serialaxes_grob_name,
                     newGrob =  editGrob(
                       grob =  do.call(
                         polygonGrob,
                         args = polyline2gon(Filter(Negate(is.null), 
                                                    x = getGrobArgs(serialaxes_grob)))
                       ),
                       gp = gpar(
                         fill = serialaxes_grob$gp$col
                       )
                     )
                   )
                 }
               } else {
                 if(!is(serialaxes_grob, "lines")) {
                   newGrob$children[[i]] <<- setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = serialaxes_grob_name,
                     newGrob = editGrob(
                       grob = do.call(
                         linesGrob,
                         args = polygon2line(Filter(Negate(is.null), 
                                                    x = getGrobArgs(serialaxes_grob)))
                       ),
                       gp = gpar(
                         col = serialaxes_grob$gp$fill
                       )
                     )
                   )
                 }
               }
             } else {
               # radial axes
               if(set_area) {
                 if(!is(serialaxes_grob, "polygon")) {
                   newGrob$children[[i]] <<- setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = serialaxes_grob_name,
                     newGrob = editGrob(
                       grob = do.call(
                         polygonGrob,
                         args = Filter(Negate(is.null), 
                                       x = getGrobArgs(serialaxes_grob))
                       ),
                       gp = gpar(
                         fill = serialaxes_grob$gp$col
                       )
                     )
                   )
                 }
               } else {
                 if(!is(serialaxes_grob, "lines")) {
                   newGrob$children[[i]] <<- setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = serialaxes_grob_name,
                     newGrob = editGrob(
                       grob = do.call(
                         linesGrob,
                         args = Filter(Negate(is.null), x = getGrobArgs(serialaxes_grob))
                       ),
                       gp = gpar(
                         col = serialaxes_grob$gp$fill
                       )
                     )
                   )
                 }
               }
             }
           }
    )
    
  } else NULL
  
  setGrob(
    gTree = loon_grob,
    gPath = pointsTree_name,
    newGrob = newGrob
  )
}

polygon2line <- function(grobArgs) {
  x <- grobArgs$x
  y <- grobArgs$y
  
  center_x <- unique(get_unit(x, "native", as.numeric = TRUE))
  xx <- get_unit(x, "native", is.unit = FALSE, as.numeric = TRUE)
  rounding_x <- xx[1:(length(xx)/2)]
  
  center_y <- unique(get_unit(y, "native", as.numeric = TRUE))
  yy <- get_unit(y, "native", is.unit = FALSE, as.numeric = TRUE)
  rounding_y <- yy[1:(length(yy)/2)]
  
  grobArgs$x <- NULL
  grobArgs$y <- NULL
  
  c(
    list(
      x = unit(center_x, "native") + unit(rounding_x, "mm"),
      y = unit(center_y, "native") + unit(rounding_y, "mm")
    ),
    grobArgs
  )
  
}

polygon2line <- function(grobArgs) {
  x <- grobArgs$x
  y <- grobArgs$y
  
  center_x <- unique(get_unit(x, "native", as.numeric = TRUE))
  xx <- get_unit(x, "native", is.unit = FALSE, as.numeric = TRUE)
  rounding_x <- xx[1:(length(xx)/2)]
  
  center_y <- unique(get_unit(y, "native", as.numeric = TRUE))
  yy <- get_unit(y, "native", is.unit = FALSE, as.numeric = TRUE)
  rounding_y <- yy[1:(length(yy)/2)]
  
  grobArgs$x <- NULL
  grobArgs$y <- NULL
  
  c(
    list(
      x = unit(center_x, "native") + unit(rounding_x, "mm"),
      y = unit(center_y, "native") + unit(rounding_y, "mm")
    ),
    grobArgs
  )
}

polyline2gon <- function(grobArgs) {
  x <- grobArgs$x
  y <- grobArgs$y
  
  scaleY <- serialAxes_scale()
  
  center_x <- unique(get_unit(x, "native", as.numeric = TRUE))
  xx <- get_unit(x, "native", is.unit = FALSE, as.numeric = TRUE)
  rounding_x <- c(xx, rev(xx))
  
  center_y <- unique(get_unit(y, "native", as.numeric = TRUE))
  yy <- get_unit(y, "native", is.unit = FALSE, as.numeric = TRUE)
  rounding_y <- c(yy, scaleY * rep(1,length(yy)))
  
  grobArgs$x <- NULL
  grobArgs$y <- NULL
  
  c(
    list(
      x = unit(center_x, "native") + unit(rounding_x, "mm"),
      y = unit(center_y, "native") + unit(rounding_y, "mm")
    ),
    grobArgs
  )
}

serialAxes_scale <- function() {
  -3.2
}
