set_alpha_grob <- function(loon.grob, index, newAlpha, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_alpha_grob", obj)
}

set_alpha_grob.l_plot <- function(loon.grob, index, newAlpha, ...) {

  args <- list(...)
  pointsTreeName <- args$pointsTreeName
  len <- length(index)

  if(pointsTreeName != "points: missing glyphs" && len > 0) {

    roundings <- args$roundings

    newGrob <- grid::getGrob(loon.grob, pointsTreeName)

    lapply(index,
           function(i) {

             grobi <- newGrob$children[[i]]

             if(grepl(grobi$name, pattern = "primitive_glyph")) {

               gp <- grobi$gp
               gp$alpha <- newAlpha

               newGrob$children[[i]] <<- grid::editGrob(
                 grob = grobi,
                 gp = gp
               )
             } else if(grepl(grobi$name, pattern = "pointrange_glyph")) {

               pointGrob <- grid::getGrob(grobi, "point")
               gp <- pointGrob$gp
               gp$alpha <- newAlpha

               newGrob$children[[i]] <<- grid::setGrob(
                 gTree = grobi,
                 gPath = "point",
                 newGrob = grid::editGrob(
                   grob = pointGrob,
                   gp = gp
                 )
               )
             } else if(grepl(grobi$name, pattern = "text_glyph")) {

               gp <- grobi$gp
               gp$alpha <- newAlpha

               newGrob$children[[i]] <<- grid::editGrob(
                 grob = grobi,
                 gp = gp
               )
             } else if(grepl(grobi$name, pattern = "serialaxes_glyph")) {

               polyline_grob <- grid::getGrob(grobi, "polyline")
               if(is.null(polyline_grob)) {
                 polyline_grob <- grid::getGrob(grobi, "polyline: showArea")
                 polyline_grob_name <-  "polyline: showArea"
                 polyline_grob$gp$alpha <- newAlpha
               } else {
                 polyline_grob_name <-  "polyline"
                 polyline_grob$gp$alpha <- newAlpha
               }

               newGrob$children[[i]] <<- grid::setGrob(
                 gTree = grobi,
                 gPath = polyline_grob_name,
                 newGrob = polyline_grob
               )
             } else if(grepl(grobi$name, pattern = "polygon_glyph")) {

               gp <- grobi$gp
               gp$alpha <- newAlpha

               newGrob$children[[i]] <<- grid::editGrob(
                 grob = grobi,
                 gp = gp
               )

             } else if(grepl(grobi$name, pattern = "image_glyph")) {

               imageBorderGrob <- grid::getGrob(grobi, "image_border")
               gp <- imageBorderGrob$gp
               gp$alpha <- newAlpha
               imageBorderGrob <-  grid::editGrob(
                 grob = imageBorderGrob,
                 gp = gp
               )

               imageGrob <- grid::getGrob(grobi, "image")
               gp <- imageGrob$gp
               gp$alpha <- newAlpha
               imageGrob <- grid::editGrob(
                 grob = imageGrob,
                 gp = gp
               )

               newGrob$children[[i]] <<- gTree(
                 children = gList(
                   imageBorderGrob,
                   imageGrob
                 ),
                 name =  grobi$name
               )

             } else {
               warning("Not Implemented yet", call. = FALSE)
               grobi
             }
           }
    )

    grid::setGrob(
      gTree = loon.grob,
      gPath = pointsTreeName,
      newGrob = newGrob
    )
  } else {
    loon.grob
  }
}

set_alpha_grob.l_graph <- function(loon.grob, index, newAlpha, ...) {

  if(length(index) > 0) {

    newGrob <- grid::getGrob(loon.grob, "graph nodes")

    lapply(index,
           function(i) {

             grobi <- newGrob$children[[i]]
             gp <- grobi$gp
             gp$alpha <- newAlpha

             newGrob$children[[i]] <<- grid::editGrob(
               grob = grobi,
               gp = gp
             )

           }
    )

    grid::setGrob(
      gTree = loon.grob,
      gPath = "graph nodes",
      newGrob = newGrob
    )
  } else {
    loon.grob
  }
}


set_alpha_grob.l_serialaxes <- function(loon.grob, index, newAlpha, ...) {

  args <- list(...)
  axesGpath <- args$axesGpath

  if(length(index) > 0) {

    axesGrob <- grid::getGrob(loon.grob, axesGpath)

    lapply(index,
           function(i) {

             grobi <- axesGrob$children[[i]]
             gp <- grobi$gp
             gp$alpha <- newAlpha

             axesGrob$children[[i]] <<- grid::editGrob(
               grob = grobi,
               gp = gp
             )
           }
    )

    grid::setGrob(
      gTree = loon.grob,
      gPath = axesGpath,
      newGrob = axesGrob
    )
  } else {
    loon.grob
  }
}
