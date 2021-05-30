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

    if(temporary && swap) {

      lapply(index,
             function(i) {

               grobi <- newGrob$children[[i]]

               if(grepl(grobi$name, pattern = "primitive_glyph")) {

                 newGrob$children[[i]] <<- grid::editGrob(
                   grob = grobi,
                   x = xyOriginal[[i]]$y,
                   y = xyOriginal[[i]]$x
                 )

               } else if(grepl(grobi$name,pattern =  "serialaxes_glyph"))  {

                 polyline_grob <- grid::getGrob(grobi, "polyline")
                 if(is.null(polyline_grob)) {
                   polyline_grob <- grid::getGrob(grobi, "polyline: showArea")
                   polyline_grob_name <-  "polyline: showArea"
                 } else {
                   polyline_grob_name <-  "polyline"
                 }

                 polyline_grob <- grid::editGrob(
                   polyline_grob,
                   y = xyOriginal[[i]]$x,
                   x = xyOriginal[[i]]$y
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = grobi,
                   gPath = polyline_grob_name,
                   newGrob = polyline_grob
                 )

               } else if(grepl(grobi$name, pattern = "polygon_glyph")) {

                 newGrob$children[[i]] <<- grid::editGrob(
                   grob = grobi,
                   x = xyOriginal[[i]]$y,
                   y = xyOriginal[[i]]$x
                 )

               } else if(grepl(grobi$name,pattern =  "pointrange_glyph")) {

                 pointGrob <- grid::getGrob(grobi, "point")
                 range_grob <- grid::getGrob(grobi, "range")

                 pointGrob <- grid::editGrob(
                   pointGrob,
                   y = xyOriginal[[i]]$x,
                   x = xyOriginal[[i]]$y
                 )

                 range_grob <- grid::editGrob(
                   range_grob,
                   y = xyOriginal[[i]]$x_range,
                   x = xyOriginal[[i]]$y_range
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = grid::setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = "point",
                     newGrob = pointGrob
                   ),
                   gPath = "range",
                   newGrob = range_grob
                 )

               } else if(grepl(grobi$name, pattern = "text_glyph"))  {

                 newGrob$children[[i]] <<- grid::editGrob(
                   grob = grobi,
                   x = xyOriginal[[i]]$y,
                   y = xyOriginal[[i]]$x
                 )

               } else if(grepl(grobi$name,pattern =  "image_glyph")) {

                 imageBorderGrob <- grid::getGrob(grobi, "image_border")
                 imageGrob <- grid::getGrob(grobi, "image")

                 imageBorderGrob <- grid::editGrob(
                   imageBorderGrob,
                   y = xyOriginal[[i]]$x_border,
                   x = xyOriginal[[i]]$y_border
                 )

                 imageGrob <- grid::editGrob(
                   imageGrob,
                   y = xyOriginal[[i]]$x,
                   x = xyOriginal[[i]]$y
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = grid::setGrob(
                     gTree = grobi,
                     gPath = "image_border",
                     newGrob = imageBorderGrob
                   ),
                   gPath = "image",
                   newGrob = imageGrob
                 )
               } else stop("not implemented")
             }
      )
    } else {
      lapply(index,
             function(i) {

               grobi <- newGrob$children[[i]]

               if(grepl(grobi$name, pattern = "primitive_glyph")) {

                 newGrob$children[[i]] <<- grid::editGrob(
                   grob = grobi,
                   y = xyOriginal[[i]]$y,
                   x = xyOriginal[[i]]$x
                 )

               } else if(grepl(grobi$name,pattern =  "serialaxes_glyph"))  {

                 polyline_grob <- grid::getGrob(grobi, "polyline")
                 if(is.null(polyline_grob)) {
                   polyline_grob <- grid::getGrob(grobi, "polyline: showArea")
                   polyline_grob_name <-  "polyline: showArea"
                 } else {
                   polyline_grob_name <-  "polyline"
                 }

                 polyline_grob <- grid::editGrob(
                   polyline_grob,
                   x = xyOriginal[[i]]$x,
                   y = xyOriginal[[i]]$y
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = grobi,
                   gPath = polyline_grob_name,
                   newGrob = polyline_grob
                 )

               } else if(grepl(grobi$name,pattern =  "polygon_glyph")) {

                 newGrob$children[[i]] <<- grid::editGrob(
                   grob = grobi,
                   y = xyOriginal[[i]]$y,
                   x = xyOriginal[[i]]$x
                 )

               } else if(grepl(grobi$name,pattern =  "pointrange_glyph")) {

                 pointGrob <- grid::getGrob(grobi, "point")
                 range_grob <- grid::getGrob(grobi, "range")

                 pointGrob <- grid::editGrob(
                   pointGrob,
                   x = xyOriginal[[i]]$x,
                   y = xyOriginal[[i]]$y
                 )

                 range_grob <- grid::editGrob(
                   range_grob,
                   x = xyOriginal[[i]]$x_range,
                   y = xyOriginal[[i]]$y_range
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = grid::setGrob(
                     gTree = grobi,
                     gPath = "point",
                     newGrob = pointGrob
                   ),
                   gPath = "range",
                   newGrob = range_grob
                 )

               } else if(grepl(grobi$name,pattern =  "text_glyph"))  {

                 newGrob$children[[i]] <<- grid::editGrob(
                   grob = grobi,
                   y = xyOriginal[[i]]$y,
                   x = xyOriginal[[i]]$x
                 )

               } else if(grepl(grobi$name,pattern =  "image_glyph")) {

                 imageBorderGrob <- grid::getGrob(grobi, "image_border")
                 imageGrob <- grid::getGrob(grobi, "image")

                 imageBorderGrob <- grid::editGrob(
                   imageBorderGrob,
                   x = xyOriginal[[i]]$x_border,
                   y = xyOriginal[[i]]$y_border
                 )

                 imageGrob <- grid::editGrob(
                   imageGrob,
                   x = xyOriginal[[i]]$x,
                   y = xyOriginal[[i]]$y
                 )

                 newGrob$children[[i]] <<- grid::setGrob(
                   gTree = grid::setGrob(
                     gTree = grobi,
                     gPath = "image_border",
                     newGrob = imageBorderGrob
                   ),
                   gPath = "image",
                   newGrob = imageGrob
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
