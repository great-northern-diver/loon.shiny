move_reset_grob <- function(loon.grob, index, swap, xyOriginal, temporary, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("move_reset_grob", obj)
}


move_reset_grob.l_plot <- function(loon.grob, index, swap, xyOriginal, temporary = FALSE, ...) {

  if(length(index) == 0) return(loon.grob)

  args <- list(...)
  pointsTreeName <- args$pointsTreeName

  if(pointsTreeName != "points: missing glyphs") {

    newGrob <- grid::getGrob(loon.grob, pointsTreeName)

    if(temporary & swap) {

      lapply(index,
             function(i) {

               if(grepl(newGrob$children[[i]]$name, pattern = "primitive_glyph")) {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   x = xyOriginal[[i]]$y,
                   y = xyOriginal[[i]]$x
                 )

               } else if(grepl(newGrob$children[[i]]$name,pattern =  "serialaxes_glyph"))  {

                 polyline_grob <- grid::getGrob(newGrob$children[[i]], "polyline")
                 if(is.null(polyline_grob)) {
                   polyline_grob <- grid::getGrob(newGrob$children[[i]], "polyline: showArea")
                   polyline_grob_name <-  "polyline: showArea"
                 } else {
                   polyline_grob_name <-  "polyline"
                 }

                 polyline_grob <- editGrob(
                   polyline_grob,
                   y = xyOriginal[[i]]$x,
                   x = xyOriginal[[i]]$y
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = polyline_grob_name,
                   newGrob = polyline_grob
                 )

               } else if(grepl(newGrob$children[[i]]$name, pattern = "polygon_glyph")) {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   x = xyOriginal[[i]]$y,
                   y = xyOriginal[[i]]$x
                 )

               } else if(grepl(newGrob$children[[i]]$name,pattern =  "pointrange_glyph")) {

                 point_grob <- grid::getGrob(newGrob$children[[i]], "point")
                 range_grob <- grid::getGrob(newGrob$children[[i]], "range")

                 point_grob <- editGrob(
                   point_grob,
                   y = xyOriginal[[i]]$x,
                   x = xyOriginal[[i]]$y
                 )

                 range_grob <- editGrob(
                   range_grob,
                   y = xyOriginal[[i]]$x_range,
                   x = xyOriginal[[i]]$y_range
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = grid::setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = "point",
                     newGrob = point_grob
                   ),
                   gPath = "range",
                   newGrob = range_grob
                 )

               } else if(grepl(newGrob$children[[i]]$name, pattern = "text_glyph"))  {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   x = xyOriginal[[i]]$y,
                   y = xyOriginal[[i]]$x
                 )

               } else if(grepl(newGrob$children[[i]]$name,pattern =  "image_glyph")) {

                 image_border_grob <- grid::getGrob(newGrob$children[[i]], "image_border")
                 image_grob <- grid::getGrob(newGrob$children[[i]], "image")

                 image_border_grob <- editGrob(
                   image_border_grob,
                   y = xyOriginal[[i]]$x_border,
                   x = xyOriginal[[i]]$y_border
                 )

                 image_grob <- editGrob(
                   image_grob,
                   y = xyOriginal[[i]]$x,
                   x = xyOriginal[[i]]$y
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = grid::setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = "image_border",
                     newGrob = image_border_grob
                   ),
                   gPath = "image",
                   newGrob = image_grob
                 )
               } else stop("not implemented")
             }
      )
    } else {
      lapply(index,
             function(i) {

               if(grepl(newGrob$children[[i]]$name, pattern = "primitive_glyph")) {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   y = xyOriginal[[i]]$y,
                   x = xyOriginal[[i]]$x
                 )

               } else if(grepl(newGrob$children[[i]]$name,pattern =  "serialaxes_glyph"))  {

                 polyline_grob <- grid::getGrob(newGrob$children[[i]], "polyline")
                 if(is.null(polyline_grob)) {
                   polyline_grob <- grid::getGrob(newGrob$children[[i]], "polyline: showArea")
                   polyline_grob_name <-  "polyline: showArea"
                 } else {
                   polyline_grob_name <-  "polyline"
                 }

                 polyline_grob <- editGrob(
                   polyline_grob,
                   x = xyOriginal[[i]]$x,
                   y = xyOriginal[[i]]$y
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = polyline_grob_name,
                   newGrob = polyline_grob
                 )

               } else if(grepl(newGrob$children[[i]]$name,pattern =  "polygon_glyph")) {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   y = xyOriginal[[i]]$y,
                   x = xyOriginal[[i]]$x
                 )

               } else if(grepl(newGrob$children[[i]]$name,pattern =  "pointrange_glyph")) {

                 point_grob <- grid::getGrob(newGrob$children[[i]], "point")
                 range_grob <- grid::getGrob(newGrob$children[[i]], "range")

                 point_grob <- editGrob(
                   point_grob,
                   x = xyOriginal[[i]]$x,
                   y = xyOriginal[[i]]$y
                 )

                 range_grob <- editGrob(
                   range_grob,
                   x = xyOriginal[[i]]$x_range,
                   y = xyOriginal[[i]]$y_range
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = grid::setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = "point",
                     newGrob = point_grob
                   ),
                   gPath = "range",
                   newGrob = range_grob
                 )

               } else if(grepl(newGrob$children[[i]]$name,pattern =  "text_glyph"))  {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   y = xyOriginal[[i]]$y,
                   x = xyOriginal[[i]]$x
                 )

               } else if(grepl(newGrob$children[[i]]$name,pattern =  "image_glyph")) {

                 image_border_grob <- grid::getGrob(newGrob$children[[i]], "image_border")
                 image_grob <- grid::getGrob(newGrob$children[[i]], "image")

                 image_border_grob <- editGrob(
                   image_border_grob,
                   x = xyOriginal[[i]]$x_border,
                   y = xyOriginal[[i]]$y_border
                 )

                 image_grob <- editGrob(
                   image_grob,
                   x = xyOriginal[[i]]$x,
                   y = xyOriginal[[i]]$y
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = grid::setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = "image_border",
                     newGrob = image_border_grob
                   ),
                   gPath = "image",
                   newGrob = image_grob
                 )
               } else stop("not implemented")
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





move_reset_grob.l_graph <- function(loon.grob, index, swap, xyOriginal, temporary = FALSE, ...) {

  if(length(index) == 0) return(loon.grob)

  xy <- xyOriginal[index]
  x <- y <- c()

  lapply(1:length(xy),
         function(i) {
           if(swap) {
             x[i] <<- xy[[i]]$y
             y[i] <<- xy[[i]]$x
           } else {
             x[i] <<- xy[[i]]$x
             y[i] <<- xy[[i]]$y
           }
         }
  )

  move_jitter_grob.l_graph(loon.grob,
                           index,
                           swap,
                           jitterxy  = list(x = x, y = y),
                           temporary, ...)
}
