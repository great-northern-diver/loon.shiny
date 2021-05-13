move_reset_grob <- function(loon_grob, index, swap, xy_original, temporary, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("move_reset_grob", obj)
}


move_reset_grob.l_plot <- function(loon_grob, index, swap, xy_original, temporary = FALSE, ...) {

  if(length(index) == 0) return(loon_grob)
  
  args <- list(...)
  pointsTree_name <- args$pointsTree_name

  if(pointsTree_name != "points: missing glyphs") {

    newGrob <- getGrob(loon_grob, pointsTree_name)

    if(temporary & swap) {

      lapply(index,
             function(i) {

               if(str_detect(newGrob$children[[i]]$name, "primitive_glyph")) {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   x = xy_original[[i]]$y,
                   y = xy_original[[i]]$x
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "serialaxes_glyph"))  {

                 polyline_grob <- getGrob(newGrob$children[[i]], "polyline")
                 if(is.null(polyline_grob)) {
                   polyline_grob <- getGrob(newGrob$children[[i]], "polyline: showArea")
                   polyline_grob_name <-  "polyline: showArea"
                 } else {
                   polyline_grob_name <-  "polyline"
                 }

                 polyline_grob <- editGrob(
                   polyline_grob,
                   y = xy_original[[i]]$x,
                   x = xy_original[[i]]$y
                 )

                 newGrob$children[[i]] <<- setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = polyline_grob_name,
                   newGrob = polyline_grob
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "polygon_glyph")) {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   x = xy_original[[i]]$y,
                   y = xy_original[[i]]$x
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "pointrange_glyph")) {

                 point_grob <- getGrob(newGrob$children[[i]], "point")
                 range_grob <- getGrob(newGrob$children[[i]], "range")

                 point_grob <- editGrob(
                   point_grob,
                   y = xy_original[[i]]$x,
                   x = xy_original[[i]]$y
                 )

                 range_grob <- editGrob(
                   range_grob,
                   y = xy_original[[i]]$x_range,
                   x = xy_original[[i]]$y_range
                 )

                 newGrob$children[[i]] <<- setGrob(
                   gTree = setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = "point",
                     newGrob = point_grob
                   ),
                   gPath = "range",
                   newGrob = range_grob
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "text_glyph"))  {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   x = xy_original[[i]]$y,
                   y = xy_original[[i]]$x
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "image_glyph")) {

                 image_border_grob <- getGrob(newGrob$children[[i]], "image_border")
                 image_grob <- getGrob(newGrob$children[[i]], "image")

                 image_border_grob <- editGrob(
                   image_border_grob,
                   y = xy_original[[i]]$x_border,
                   x = xy_original[[i]]$y_border
                 )

                 image_grob <- editGrob(
                   image_grob,
                   y = xy_original[[i]]$x,
                   x = xy_original[[i]]$y
                 )

                 newGrob$children[[i]] <<- setGrob(
                   gTree = setGrob(
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

               if(str_detect(newGrob$children[[i]]$name, "primitive_glyph")) {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   y = xy_original[[i]]$y,
                   x = xy_original[[i]]$x
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "serialaxes_glyph"))  {

                 polyline_grob <- getGrob(newGrob$children[[i]], "polyline")
                 if(is.null(polyline_grob)) {
                   polyline_grob <- getGrob(newGrob$children[[i]], "polyline: showArea")
                   polyline_grob_name <-  "polyline: showArea"
                 } else {
                   polyline_grob_name <-  "polyline"
                 }

                 polyline_grob <- editGrob(
                   polyline_grob,
                   x = xy_original[[i]]$x,
                   y = xy_original[[i]]$y
                 )

                 newGrob$children[[i]] <<- setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = polyline_grob_name,
                   newGrob = polyline_grob
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "polygon_glyph")) {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   y = xy_original[[i]]$y,
                   x = xy_original[[i]]$x
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "pointrange_glyph")) {

                 point_grob <- getGrob(newGrob$children[[i]], "point")
                 range_grob <- getGrob(newGrob$children[[i]], "range")

                 point_grob <- editGrob(
                   point_grob,
                   x = xy_original[[i]]$x,
                   y = xy_original[[i]]$y
                 )

                 range_grob <- editGrob(
                   range_grob,
                   x = xy_original[[i]]$x_range,
                   y = xy_original[[i]]$y_range
                 )

                 newGrob$children[[i]] <<- setGrob(
                   gTree = setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = "point",
                     newGrob = point_grob
                   ),
                   gPath = "range",
                   newGrob = range_grob
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "text_glyph"))  {

                 newGrob$children[[i]] <<- editGrob(
                   grob = newGrob$children[[i]],
                   y = xy_original[[i]]$y,
                   x = xy_original[[i]]$x
                 )

               } else if(str_detect(newGrob$children[[i]]$name, "image_glyph")) {

                 image_border_grob <- getGrob(newGrob$children[[i]], "image_border")
                 image_grob <- getGrob(newGrob$children[[i]], "image")

                 image_border_grob <- editGrob(
                   image_border_grob,
                   x = xy_original[[i]]$x_border,
                   y = xy_original[[i]]$y_border
                 )

                 image_grob <- editGrob(
                   image_grob,
                   x = xy_original[[i]]$x,
                   y = xy_original[[i]]$y
                 )

                 newGrob$children[[i]] <<- setGrob(
                   gTree = setGrob(
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

    setGrob(
      gTree = loon_grob,
      gPath = pointsTree_name,
      newGrob = newGrob
    )
  } else loon_grob
}





move_reset_grob.l_graph <- function(loon_grob, index, swap, xy_original, temporary = FALSE, ...) {

  if(length(index) == 0) return(loon_grob)
  
  xy <- xy_original[index]
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

  move_jitter_grob.l_graph(loon_grob,
                           index,
                           swap,
                           jitter_xy  = list(x = x, y = y),
                           temporary, ...)
}
