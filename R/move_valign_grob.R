move_valign_grob <- function(loon.grob, index, swap, valignX, temporary = FALSE, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("move_valign_grob", obj)
}

move_valign_grob.l_plot <- function(loon.grob, index, swap, valignX, temporary = FALSE, ...) {

  if(length(index) == 0) return(loon.grob)

  args <- list(...)
  pointsTreeName <- args$pointsTreeName

  if(pointsTreeName != "points: missing glyphs") {
    newGrob <- grid::getGrob(loon.grob, pointsTreeName)

    if(!temporary & swap) {
      lapply(index,
             function(i) {

               if(grepl(newGrob$children[[i]]$name, pattern = "primitive_glyph")) {
                 newGrob$children[[i]] <<- grid::editGrob(
                   grob = newGrob$children[[i]],
                   y = unit(valignX, "native")
                 )
               } else if(grepl(newGrob$children[[i]]$name, pattern = "serialaxes_glyph"))  {

                 polyline_grob <- grid::getGrob(newGrob$children[[i]], "polyline")
                 if(is.null(polyline_grob)) {
                   polyline_grob <- grid::getGrob(newGrob$children[[i]], "polyline: showArea")
                   polyline_grob_name <-  "polyline: showArea"
                 } else polyline_grob_name <-  "polyline"

                 polyline_grob$y <- unit(valignX, "native") +
                   get_unit(polyline_grob$y,
                            is.unit = FALSE,
                            as.numeric = FALSE)

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = polyline_grob_name,
                   newGrob = polyline_grob
                 )

               } else if(grepl(newGrob$children[[i]]$name, pattern = "polygon_glyph")) {

                 newGrob$children[[i]] <<- grid::editGrob(
                   grob = newGrob$children[[i]],
                   y = unit(valignX, "native") +
                     get_unit(newGrob$children[[i]]$y,
                              is.unit = FALSE,
                              as.numeric = FALSE)
                 )

               } else if(grepl(newGrob$children[[i]]$name, pattern = "pointrange_glyph")) {

                 # TODO
                 pointGrob <- grid::getGrob(newGrob$children[[i]], "point")
                 line_grob <- grid::getGrob(newGrob$children[[i]], "range")

                 pointGrob$y <- unit(valignX, "native")
                 line_grob$x <- unit(rep(valignX, 2), "native")

                 tmpGrob <- grid::setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "point",
                   newGrob = pointGrob
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = tmpGrob,
                   gPath = "range",
                   newGrob = line_grob
                 )

               } else if(grepl(newGrob$children[[i]]$name, pattern = "text_glyph"))  {

                 newGrob$children[[i]] <<- grid::editGrob(
                   grob = newGrob$children[[i]],
                   y = unit(valignX, "native")
                 )

               } else if(grepl(newGrob$children[[i]]$name, pattern = "image_glyph")) {

                 imageBorderGrob <- grid::getGrob(newGrob$children[[i]], "image_border")
                 imageGrob <- grid::getGrob(newGrob$children[[i]], "image")

                 imageBorderGrob$y <- unit(valignX, "native")
                 imageGrob$y <- unit(valignX, "native")

                 tmpGrob <- grid::setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "image_border",
                   newGrob = imageBorderGrob
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = tmpGrob,
                   gPath = "image",
                   newGrob = imageGrob
                 )
               } else stop("not inplemented")
             }
      )
    } else {
      lapply(index,
             function(i) {

               if(grepl(newGrob$children[[i]]$name, pattern = "primitive_glyph")) {
                 newGrob$children[[i]] <<- grid::editGrob(
                   grob = newGrob$children[[i]],
                   x = unit(valignX, "native")
                 )
               } else if(grepl(newGrob$children[[i]]$name, pattern = "serialaxes_glyph"))  {

                 polyline_grob <- grid::getGrob(newGrob$children[[i]], "polyline")
                 if(is.null(polyline_grob)) {
                   polyline_grob <- grid::getGrob(newGrob$children[[i]], "polyline: showArea")
                   polyline_grob_name <-  "polyline: showArea"
                 } else polyline_grob_name <-  "polyline"

                 polyline_grob$x <- unit(valignX, "native") +
                   get_unit(polyline_grob$x,
                            is.unit = FALSE,
                            as.numeric = FALSE)

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = polyline_grob_name,
                   newGrob = polyline_grob
                 )

               } else if(grepl(newGrob$children[[i]]$name,pattern =  "polygon_glyph")) {

                 newGrob$children[[i]] <<- grid::editGrob(
                   grob = newGrob$children[[i]],
                   x = unit(valignX, "native") +
                     get_unit(newGrob$children[[i]]$x,
                              is.unit = FALSE,
                              as.numeric = FALSE)
                 )

               } else if(grepl(newGrob$children[[i]]$name,pattern =  "pointrange_glyph")) {

                 pointGrob <- grid::getGrob(newGrob$children[[i]], "point")
                 line_grob <- grid::getGrob(newGrob$children[[i]], "range")

                 pointGrob$x <- unit(valignX, "native")
                 line_grob$x <- unit(rep(valignX, 2), "native")

                 tmpGrob <- grid::setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "point",
                   newGrob = pointGrob
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = tmpGrob,
                   gPath = "range",
                   newGrob = line_grob
                 )

               } else if(grepl(newGrob$children[[i]]$name,pattern =  "text_glyph"))  {

                 newGrob$children[[i]] <<- grid::editGrob(
                   grob = newGrob$children[[i]],
                   x = unit(valignX, "native")
                 )

               } else if(grepl(newGrob$children[[i]]$name,pattern =  "image_glyph")) {

                 imageBorderGrob <- grid::getGrob(newGrob$children[[i]], "image_border")
                 imageGrob <- grid::getGrob(newGrob$children[[i]], "image")

                 imageBorderGrob$x <- unit(valignX, "native")
                 imageGrob$x <- unit(valignX, "native")

                 tmpGrob <- grid::setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = "image_border",
                   newGrob = imageBorderGrob
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = tmpGrob,
                   gPath = "image",
                   newGrob = imageGrob
                 )
               } else stop("not inplemented")
             }
      )
    }

    grid::setGrob(
      gTree = loon.grob,
      gPath = pointsTreeName,
      newGrob = newGrob
    )
  } else loon.grob
}


move_valign_grob.l_graph <- function(loon.grob, index, swap, valignX, temporary = FALSE) {

  if(length(index) == 0) return(loon.grob)

  nodesGrob <- grid::getGrob(loon.grob, "graph nodes")
  labelsGrob <- grid::getGrob(loon.grob, "graph labels")
  edgesGrob <- grid::getGrob(loon.grob, "graph edges")

  if(!temporary & swap) {

    lapply(index,
           function(i) {

             nodesGrob$children[[i]] <<- grid::editGrob(
               grob = nodesGrob$children[[i]],
               y = unit(valignX, "native")
             )
           }
    )

    loon.grob <- grid::setGrob(
      gTree = loon.grob,
      gPath = "graph nodes",
      newGrob = nodesGrob
    )

    # avoid labelsGrob to be nullGrob
    if(!grepl(grobName(labelsGrob),pattern =  "null")) {
      lapply(index,
             function(i) {

               grobi <- labelsGrob$children[[i]]

               labelsGrob$children[[i]] <<- grid::editGrob(
                 grob = grobi,
                 y = unit(valignX, "native") +
                   get_unit(grobi$x,
                            is.unit = FALSE,
                            as.numeric = FALSE)
               )
             }
      )

      loon.grob <- grid::setGrob(
        gTree = loon.grob,
        gPath = "graph labels",
        newGrob = labelsGrob
      )
    }

    lapply(1:length(edgesGrob$children),
           function(i) {

             grobi <- edgesGrob$children[[i]]

             if(!grepl(grobi$name,pattern =  "missing")) {

               to_id <-  grobi$id
               num_line <- length(to_id)/2

               edgesGrob$children[[i]] <<- if(i %in% index) {

                 y <- c(rep(valignX, num_line), c(grobi$y)[(num_line + 1) : (2*num_line)])
                 change_id <- which(to_id %in% index)[which(to_id %in% index) > num_line]

                 if(length(change_id) > 0) {

                   y[change_id] <- valignX
                   grid::editGrob(
                     grobi,
                     y = unit(y,"native")
                   )
                 } else {
                   grid::editGrob(
                     grobi,
                     y = unit(y,"native")
                   )
                 }
               } else {

                 change_id <- which(to_id %in% index)[which(to_id %in% index) > num_line]
                 y <- c(grobi$y)

                 if(length(change_id) > 0) {

                   y[change_id] <- valignX
                   grid::editGrob(
                     grobi,
                     y = unit(y,"native")
                   )
                 } else grobi
               }
             }
           }
    )

    loon.grob <- grid::setGrob(
      gTree = loon.grob,
      gPath = "graph edges",
      newGrob = edgesGrob
    )
  } else {

    lapply(index,
           function(i) {

             nodesGrob$children[[i]] <<- grid::editGrob(
               grob = nodesGrob$children[[i]],
               x = unit(valignX, "native")
             )
           }
    )

    loon.grob <- grid::setGrob(
      gTree = loon.grob,
      gPath = "graph nodes",
      newGrob = nodesGrob
    )

    # avoid labelsGrob to be nullGrob
    if(!grepl(grobName(labelsGrob),pattern =  "null")) {
      lapply(index,
             function(i) {

               grobi <- labelsGrob$children[[i]]

               labelsGrob$children[[i]] <<- grid::editGrob(
                 grob = grobi,
                 x = unit(valignX, "native") +
                   get_unit(grobi$x,
                            is.unit = FALSE,
                            as.numeric = FALSE)
               )
             }
      )
      loon.grob <- grid::setGrob(
        gTree = loon.grob,
        gPath = "graph labels",
        newGrob = labelsGrob
      )
    }

    lapply(1:length(edgesGrob$children),
           function(i) {

             grobi <- edgesGrob$children[[i]]

             if(!grepl(grobi$name, pattern = "missing")) {

               to_id <-  grobi$id
               num_line <- length(to_id)/2

               edgesGrob$children[[i]] <<- if(i %in% index) {

                 x <- c(rep(valignX, num_line), c(grobi$x)[(num_line + 1) : (2*num_line)])
                 change_id <- which(to_id %in% index)[which(to_id %in% index) > num_line]

                 if(length(change_id) > 0) {

                   x[change_id] <- valignX
                   grid::editGrob(
                     grobi,
                     x = unit(x,"native")
                   )
                 } else {
                   grid::editGrob(
                     grobi,
                     x = unit(x,"native")
                   )
                 }
               } else {

                 change_id <- which(to_id %in% index)[which(to_id %in% index) > num_line]
                 x <- c(grobi$x)

                 if(length(change_id) > 0) {

                   x[change_id] <- valignX
                   grid::editGrob(
                     grobi,
                     x = unit(x,"native")
                   )
                 } else grobi
               }
             }
           }
    )

    loon.grob <- grid::setGrob(
      gTree = loon.grob,
      gPath = "graph edges",
      newGrob = edgesGrob
    )
  }

  loon.grob
}
