move_jitter_grob <- function(loon_grob, index, swap, jitter_xy, temporary, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("move_jitter_grob", obj)
}

move_jitter_grob.l_plot <- function(loon_grob, index, swap, jitter_xy, temporary = FALSE, ...) {

  if(length(index) == 0) return(loon_grob)
  
  args <- list(...)
  pointsTree_name <- args$pointsTree_name

  if(pointsTree_name != "points: missing glyphs") {
    x <- jitter_xy$x
    y <- jitter_xy$y

    newGrob <- getGrob(loon_grob, pointsTree_name)

    if(!temporary & swap) {
      lapply(index,
             function(i) {

               if(str_detect(newGrob$children[[i]]$name, "primitive_glyph")) {
                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   y = unit(x[which(index %in% i)], "native"),
                   x = unit(y[which(index %in% i)], "native")
                 )
               } else if(str_detect(newGrob$children[[i]]$name, "serialaxes_glyph"))  {

                 polyline_grob <- getGrob(newGrob$children[[i]], "polyline")
                 if(is.null(polyline_grob)) {
                   polyline_grob <- getGrob(newGrob$children[[i]], "polyline: showArea")
                   polyline_grob_name <-  "polyline: showArea"
                 } else polyline_grob_name <-  "polyline"

                 polyline_grob <- editGrob(
                   polyline_grob,
                   y = unit(x[which(index %in% i)], "native") + 
                     get_unit(polyline_grob$x, 
                              is.unit = FALSE,
                              as.numeric = FALSE),
                   x = unit(y[which(index %in% i)], "native") + 
                     get_unit(polyline_grob$y, 
                              is.unit = FALSE,
                              as.numeric = FALSE)
                 )

                 newGrob$children[[i]] <<- setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = polyline_grob_name,
                   newGrob = polyline_grob
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "polygon_glyph")) {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   y = unit(x[which(index %in% i)], "native") +  
                     get_unit(newGrob$children[[i]]$x, 
                              is.unit = FALSE,
                              as.numeric = FALSE),
                   x = unit(y[which(index %in% i)], "native") +  
                     get_unit(newGrob$children[[i]]$y, 
                              is.unit = FALSE,
                              as.numeric = FALSE)
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "pointrange_glyph")) {

                 point_grob <- getGrob(newGrob$children[[i]], "point")
                 line_grob <- getGrob(newGrob$children[[i]], "range")

                 point_grob <- editGrob(
                   point_grob,
                   y = unit(x[which(index %in% i)], "native"),
                   x = unit(y[which(index %in% i)], "native")
                 )

                 range <- diff(sort(as.numeric(line_grob$x)))/2
                 # TODO
                 line_grob <- editGrob(
                   line_grob,
                   y = unit(rep(x[which(index %in% i)], 2), "native"),
                   x = unit(c(y[which(index %in% i)] - range, y[which(index %in% i)] + range), "native")
                 )


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
                   y = unit(x[which(index %in% i)], "native"),
                   x = unit(y[which(index %in% i)], "native")
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "image_glyph")) {

                 image_border_grob <- getGrob(newGrob$children[[i]], "image_border")
                 image_grob <- getGrob(newGrob$children[[i]], "image")

                 image_border_grob <- editGrob(
                   image_border_grob,
                   y = unit(x[which(index %in% i)], "native"),
                   x = unit(y[which(index %in% i)], "native")
                 )

                 image_grob <- editGrob(
                   image_grob,
                   y = unit(x[which(index %in% i)], "native"),
                   x = unit(y[which(index %in% i)], "native")
                 )

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
                   x = unit(x[which(index %in% i)], "native"),
                   y = unit(y[which(index %in% i)], "native")
                 )
               } else if(str_detect(newGrob$children[[i]]$name, "serialaxes_glyph"))  {

                 polyline_grob <- getGrob(newGrob$children[[i]], "polyline")
                 if(is.null(polyline_grob)) {
                   polyline_grob <- getGrob(newGrob$children[[i]], "polyline: showArea")
                   polyline_grob_name <-  "polyline: showArea"
                 } else polyline_grob_name <-  "polyline"

                 polyline_grob <- editGrob(
                   polyline_grob,
                   x = unit(x[which(index %in% i)], "native") + 
                     get_unit(polyline_grob$x, 
                              is.unit = FALSE,
                              as.numeric = FALSE),
                   y = unit(y[which(index %in% i)], "native") + 
                     get_unit(polyline_grob$y, 
                              is.unit = FALSE,
                              as.numeric = FALSE)
                 )

                 newGrob$children[[i]] <<- setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = polyline_grob_name,
                   newGrob = polyline_grob
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "polygon_glyph")) {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   x = unit(x[which(index %in% i)], "native") +  
                     get_unit(newGrob$children[[i]]$x, 
                              is.unit = FALSE,
                              as.numeric = FALSE),
                   y = unit(y[which(index %in% i)], "native") +  
                     get_unit(newGrob$children[[i]]$y, 
                              is.unit = FALSE,
                              as.numeric = FALSE)
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "pointrange_glyph")) {

                 point_grob <- getGrob(newGrob$children[[i]], "point")
                 line_grob <- getGrob(newGrob$children[[i]], "range")

                 point_grob <- editGrob(
                   point_grob,
                   x = unit(x[which(index %in% i)], "native"),
                   y = unit(y[which(index %in% i)], "native")
                 )

                 range <- diff(sort(as.numeric(line_grob$y)))/2
                 line_grob <- editGrob(
                   line_grob,
                   x = unit(rep(x[which(index %in% i)], 2), "native"),
                   y = unit(c(y[which(index %in% i)] - range, y[which(index %in% i)] + range), "native")
                 )


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
                   x = unit(x[which(index %in% i)], "native"),
                   y = unit(y[which(index %in% i)], "native")
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "image_glyph")) {

                 image_border_grob <- getGrob(newGrob$children[[i]], "image_border")
                 image_grob <- getGrob(newGrob$children[[i]], "image")

                 image_border_grob <- editGrob(
                   image_border_grob,
                   x = unit(x[which(index %in% i)], "native"),
                   y = unit(y[which(index %in% i)], "native")
                 )

                 image_grob <- editGrob(
                   image_grob,
                   x = unit(x[which(index %in% i)], "native"),
                   y = unit(y[which(index %in% i)], "native")
                 )

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


move_jitter_grob.l_graph <- function(loon_grob, index, swap, jitter_xy, temporary = FALSE, ...) {

  if(length(index) == 0) return(loon_grob)
  
  jx <- jitter_xy$x
  jy <- jitter_xy$y

  nodesGrob <- getGrob(loon_grob, "graph nodes")
  labelsGrob <- getGrob(loon_grob, "graph labels")
  edgesGrob <- getGrob(loon_grob, "graph edges")

  if(!temporary & swap) {

    lapply(index,
           function(i) {

             nodesGrob$children[[i]] <<- editGrob(
               grob = nodesGrob$children[[i]],
               x = unit(jy[which(index == i)], "native"),
               y = unit(jx[which(index == i)], "native")
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
                 x = unit(jy[which(index == i)], "native") + 
                   get_unit(grobi$y, 
                            is.unit = FALSE,
                            as.numeric = FALSE),
                 y = unit(jx[which(index == i)], "native") + 
                   get_unit(grobi$x, 
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

                 x <- c(rep(jy[which(index == i)], num_line), c(grobi$x)[(num_line + 1) : (2*num_line)])
                 y <- c(rep(jx[which(index == i)], num_line), c(grobi$y)[(num_line + 1) : (2*num_line)])

                 change_id <- which(to_id %in% index)[which(to_id %in% index) > num_line]

                 if(length(change_id) > 0) {

                   x[change_id] <- jy[which(index %in% to_id[change_id])]
                   y[change_id] <- jx[which(index %in% to_id[change_id])]

                   editGrob(
                     grobi,
                     x = unit(x, "native"),
                     y = unit(y, "native")
                   )
                 } else {
                   editGrob(
                     grobi,
                     x = unit(x, "native"),
                     y = unit(y, "native")
                   )
                 }
               } else {

                 change_id <- which(to_id %in% index)[which(to_id %in% index) > num_line]

                 x <- c(grobi$x)
                 y <- c(grobi$y)

                 if(length(change_id) > 0) {

                   x[change_id] <- jy[which(index %in% to_id[change_id])]
                   y[change_id] <- jx[which(index %in% to_id[change_id])]

                   editGrob(
                     grobi,
                     x = unit(x, "native"),
                     y = unit(y, "native")
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
               x = unit(jx[which(index == i)], "native"),
               y = unit(jy[which(index == i)], "native")
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
                 y = unit(jy[which(index == i)], "native") + 
                   get_unit(grobi$y, 
                            is.unit = FALSE,
                            as.numeric = FALSE),
                 x = unit(jx[which(index == i)], "native") + 
                   get_unit(grobi$x, 
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

                 y <- c(rep(jy[which(index == i)], num_line), c(grobi$y)[(num_line + 1) : (2*num_line)])
                 x <- c(rep(jx[which(index == i)], num_line), c(grobi$x)[(num_line + 1) : (2*num_line)])

                 change_id <- which(to_id %in% index)[which(to_id %in% index) > num_line]

                 if(length(change_id) > 0) {

                   x[change_id] <- jx[which(index %in% to_id[change_id])]
                   y[change_id] <- jy[which(index %in% to_id[change_id])]

                   editGrob(
                     grobi,
                     x = unit(x, "native"),
                     y = unit(y, "native")
                   )
                 } else {
                   editGrob(
                     grobi,
                     x = unit(x, "native"),
                     y = unit(y, "native")
                   )
                 }
               } else {

                 change_id <- which(to_id %in% index)[which(to_id %in% index) > num_line]

                 x <- c(grobi$x)
                 y <- c(grobi$y)

                 if(length(change_id) > 0) {

                   x[change_id] <- jx[which(index %in% to_id[change_id])]
                   y[change_id] <- jy[which(index %in% to_id[change_id])]

                   editGrob(
                     grobi,
                     x = unit(x, "native"),
                     y = unit(y, "native")
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


jitter_coord  <- function(x, y, index) {

  if(length(index) == 1) {

    jitter_x <- x[index]
    jitter_y <- y[index]

  } else {

    diff_x <- diff(sort(x[index]))

    diff_y <- diff(sort(y[index]))
    
    diff_x <- diff_x[diff_x > 1e-2]
    diff_y <- diff_y[diff_y > 1e-2]
    
    dx <- if(length(diff_x) == 0) 
      1e-2
    else 
      min(diff_x)
    
    dy <- if(length(diff_y) == 0) 
      1e-2
    else 
      min(diff_y)

    jitter_x <- rnorm(length(index), x[index], dx/5)

    jitter_y <- rnorm(length(index), y[index], dx/5)
  }

  list(
    x = jitter_x,
    y = jitter_y
  )
}
