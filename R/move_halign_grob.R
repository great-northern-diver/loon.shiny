move_halign_grob <- function(loon_grob, index, swap, halign_y, temporary = FALSE, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("move_halign_grob", obj)
}

move_halign_grob.l_plot <- function(loon_grob, index, swap, halign_y, temporary = FALSE, ...) {
  
  if(length(index) == 0) return(loon_grob)
  
  args <- list(...)
  pointsTree_name <- args$pointsTree_name
  
  if(pointsTree_name != "points: missing glyphs") {
    
    newGrob <- getGrob(loon_grob, pointsTree_name)
    
    if(!temporary & swap) {
      lapply(index,
             function(i) {
               
               if(str_detect(newGrob$children[[i]]$name, "primitive_glyph")) {
                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   x = unit(halign_y, "native")
                 )
               } else if(str_detect(newGrob$children[[i]]$name, "serialaxes_glyph"))  {
                 
                 polyline_grob <- getGrob(newGrob$children[[i]], "polyline")
                 if(is.null(polyline_grob)) {
                   polyline_grob <- getGrob(newGrob$children[[i]], "polyline: showArea")
                   polyline_grob_name <-  "polyline: showArea"
                 } else polyline_grob_name <-  "polyline"
                 
                 polyline_grob$x <- unit(halign_y, "native") + 
                   get_unit(polyline_grob$x,
                            is.unit = FALSE,
                            as.numeric = FALSE)
                 
                 newGrob$children[[i]] <<- setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = polyline_grob_name,
                   newGrob = polyline_grob
                 )
                 
               } else if(str_detect(newGrob$children[[i]]$name, "polygon_glyph")) {
                 
                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   x = unit(halign_y, "native") + 
                     get_unit(newGrob$children[[i]]$x, 
                              is.unit = FALSE,
                              as.numeric = FALSE)
                 )
                 
                 
               } else if(str_detect(newGrob$children[[i]]$name, "pointrange_glyph")) {
                 
                 point_grob <- getGrob(newGrob$children[[i]], "point")
                 line_grob <- getGrob(newGrob$children[[i]], "range")
                 
                 point_grob$x <- unit(halign_y, "native")
                 
                 range <- diff(sort(as.numeric(line_grob$x)))/2
                 line_grob$x <- unit(c(halign_y - range, halign_y + range), "native")
                 
                 tmpGrob <- setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "point",
                   newGrob = point_grob
                 )
                 
                 newGrob$children[[i]] <<- setGrob(
                   gTree = tmpGrob,
                   gPath = "range",
                   newGrob = line_grob
                 )
                 
               } else if(str_detect(newGrob$children[[i]]$name, "text_glyph"))  {
                 
                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   x = unit(halign_y, "native")
                 )
                 
               } else if(str_detect(newGrob$children[[i]]$name, "image_glyph")) {
                 
                 image_border_grob <- getGrob(newGrob$children[[i]], "image_border")
                 image_grob <- getGrob(newGrob$children[[i]], "image")
                 
                 image_border_grob$x <- unit(halign_y, "native")
                 image_grob$x <- unit(halign_y, "native")
                 
                 tmpGrob <- setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "image_border",
                   newGrob = image_border_grob
                 )
                 
                 newGrob$children[[i]] <<- setGrob(
                   gTree = tmpGrob,
                   gPath = "image",
                   newGrob = image_grob
                 )
               } else stop("not implemented")
             }
      )
    } else {
      lapply(index,
             function(i) {
               
               if(str_detect(newGrob$children[[i]]$name, "primitive_glyph")) {
                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   y = unit(halign_y, "native")
                 )
               } else if(str_detect(newGrob$children[[i]]$name, "serialaxes_glyph"))  {
                 
                 polyline_grob <- getGrob(newGrob$children[[i]], "polyline")
                 if(is.null(polyline_grob)) {
                   polyline_grob <- getGrob(newGrob$children[[i]], "polyline: showArea")
                   polyline_grob_name <-  "polyline: showArea"
                 } else polyline_grob_name <-  "polyline"
                 
                 polyline_grob$y <- unit(halign_y, "native") + 
                   get_unit(polyline_grob$y, 
                            is.unit = FALSE,
                            as.numeric = FALSE)
                 
                 newGrob$children[[i]] <<- setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = polyline_grob_name,
                   newGrob = polyline_grob
                 )
                 
               } else if(str_detect(newGrob$children[[i]]$name, "polygon_glyph")) {
                 
                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   y = unit(halign_y, "native") + 
                     get_unit(newGrob$children[[i]]$y, 
                              is.unit = FALSE,
                              as.numeric = FALSE)
                 )
                 
               } else if(str_detect(newGrob$children[[i]]$name, "pointrange_glyph")) {
                 
                 point_grob <- getGrob(newGrob$children[[i]], "point")
                 line_grob <- getGrob(newGrob$children[[i]], "range")
                 
                 point_grob$y <- unit(halign_y, "native")
                 
                 range <- diff(sort(as.numeric(line_grob$y)))/2
                 line_grob$y <- unit(c(halign_y - range, halign_y + range), "native")
                 
                 tmpGrob <- setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "point",
                   newGrob = point_grob
                 )
                 
                 newGrob$children[[i]] <<- setGrob(
                   gTree = tmpGrob,
                   gPath = "range",
                   newGrob = line_grob
                 )
                 
               } else if(str_detect(newGrob$children[[i]]$name, "text_glyph"))  {
                 
                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   y = unit(halign_y, "native")
                 )
                 
               } else if(str_detect(newGrob$children[[i]]$name, "image_glyph")) {
                 
                 image_border_grob <- getGrob(newGrob$children[[i]], "image_border")
                 image_grob <- getGrob(newGrob$children[[i]], "image")
                 
                 image_border_grob$y <- unit(halign_y, "native")
                 image_grob$y <- unit(halign_y, "native")
                 
                 tmpGrob <- setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "image_border",
                   newGrob = image_border_grob
                 )
                 
                 newGrob$children[[i]] <<- setGrob(
                   gTree = tmpGrob,
                   gPath = "image",
                   newGrob = image_grob
                 )
               } else stop("not implemented")
             }
      )
    }
    
    setGrob(
      gTree = loon_grob,
      gPath = pointsTree_name,
      newGrob = newGrob
    )
  } else loon_grob
}

move_halign_grob.l_graph <- function(loon_grob, index, swap, halign_y, temporary = FALSE) {
  
  if(length(index) == 0) return(loon_grob)
  
  nodesGrob <- getGrob(loon_grob, "graph nodes")
  labelsGrob <- getGrob(loon_grob, "graph labels")
  edgesGrob <- getGrob(loon_grob, "graph edges")
  
  if(!temporary & swap) {
    
    lapply(index,
           function(i) {
             
             nodesGrob$children[[i]] <<- editGrob(
               grob = nodesGrob$children[[i]],
               x = unit(halign_y, "native")
             )
           }
    )
    
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "graph nodes",
      newGrob = nodesGrob
    )
    
    # avoid labelsGrob to be nullGrob
    if(!str_detect(grobName(labelsGrob), "null")) {
      lapply(index,
             function(i) {
               
               grobi <- labelsGrob$children[[i]]
               
               labelsGrob$children[[i]] <<- editGrob(
                 grob = grobi,
                 x = unit(halign_y, "native") + 
                   get_unit(grobi$y, 
                            is.unit = FALSE,
                            as.numeric = FALSE)
               )
             }
      )
      
      loon_grob <- setGrob(
        gTree = loon_grob,
        gPath = "graph labels",
        newGrob = labelsGrob
      )
    }
    
    lapply(1:length(edgesGrob$children),
           function(i) {
             
             grobi <- edgesGrob$children[[i]]
             
             if(!str_detect(grobi$name, "missing")) {
               
               to_id <-  grobi$id
               num_line <- length(to_id)/2
               
               edgesGrob$children[[i]] <<- if(i %in% index) {
                 
                 x <- c(rep(halign_y, num_line), c(grobi$x)[(num_line + 1) : (2*num_line)])
                 change_id <- which(to_id %in% index)[which(to_id %in% index) > num_line]
                 
                 if(length(change_id) > 0) {
                   
                   x[change_id] <- halign_y
                   editGrob(
                     grobi,
                     x = unit(x,"native")
                   )
                 } else {
                   editGrob(
                     grobi,
                     x = unit(x,"native")
                   )
                 }
               } else {
                 
                 change_id <- which(to_id %in% index)[which(to_id %in% index) > num_line]
                 x <- c(grobi$x)
                 
                 if(length(change_id) > 0) {
                   
                   x[change_id] <- halign_y
                   editGrob(
                     grobi,
                     x = unit(x,"native")
                   )
                 } else grobi
               }
             }
           }
    )
    
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "graph edges",
      newGrob = edgesGrob
    )
  } else {
    
    lapply(index,
           function(i) {
             
             nodesGrob$children[[i]] <<- editGrob(
               grob = nodesGrob$children[[i]],
               y = unit(halign_y, "native")
             )
           }
    )
    
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "graph nodes",
      newGrob = nodesGrob
    )
    
    # avoid labelsGrob to be nullGrob
    if(!str_detect(grobName(labelsGrob), "null")) {
      lapply(index,
             function(i) {
               
               grobi <- labelsGrob$children[[i]]
               
               labelsGrob$children[[i]] <<- editGrob(
                 grob = grobi,
                 y = unit(halign_y, "native") + 
                   get_unit(grobi$y, 
                            is.unit = FALSE,
                            as.numeric = FALSE)
               )
             }
      )
      
      loon_grob <- setGrob(
        gTree = loon_grob,
        gPath = "graph labels",
        newGrob = labelsGrob
      )
    }
    
    lapply(1:length(edgesGrob$children),
           function(i) {
             
             grobi <- edgesGrob$children[[i]]
             
             if(!str_detect(grobi$name, "missing")) {
               
               to_id <-  grobi$id
               num_line <- length(to_id)/2
               
               edgesGrob$children[[i]] <<- if(i %in% index) {
                 
                 y <- c(rep(halign_y, num_line), c(grobi$y)[(num_line + 1) : (2*num_line)])
                 change_id <- which(to_id %in% index)[which(to_id %in% index) > num_line]
                 
                 if(length(change_id) > 0) {
                   
                   y[change_id] <- halign_y
                   editGrob(
                     grobi,
                     y = unit(y,"native")
                   )
                 } else {
                   editGrob(
                     grobi,
                     y = unit(y,"native")
                   )
                 }
               } else {
                 
                 change_id <- which(to_id %in% index)[which(to_id %in% index) > num_line]
                 y <- c(grobi$y)
                 
                 if(length(change_id) > 0) {
                   
                   y[change_id] <- halign_y
                   editGrob(
                     grobi,
                     y = unit(y,"native")
                   )
                 } else grobi
               }
             }
           }
    )
    
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "graph edges",
      newGrob = edgesGrob
    )
  }
  
  loon_grob
}

