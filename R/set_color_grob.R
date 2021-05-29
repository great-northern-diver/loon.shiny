set_color_grob <- function(loon.grob, index, newColor, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_color_grob", obj)
}

set_color_grob.l_plot <- function(loon.grob, index, newColor, ...) {

  args <- list(...)
  pointsTreeName <- args$pointsTreeName
  len <- length(index)

  if(pointsTreeName != "points: missing glyphs" && len > 0) {

    newGrob <- grid::getGrob(loon.grob, pointsTreeName)

    lenNewColor <- length(newColor)
    if(lenNewColor == 1) {
      newColor <- rep(newColor, len)
    } else if (lenNewColor != len) {
      stop("color length is not equal to index length")
    } else NULL

    lapply(seq(len),
           function(k) {

             i <- index[k]

             grobi <- newGrob$children[[i]]

             newGrob$children[[i]] <<- if(grepl(grobi$name, pattern = "primitive_glyph")) {

               gp <- grobi$gp
               pch <- grobi$pch %||% 21

               if(pch %in% 21:24) {
                 gp$fill <- newColor[k]
                 gp$col <- bounder_color()
               } else {
                 gp$col <- newColor[k]
               }

               grid::editGrob(
                 grob = grobi,
                 gp = gp
               )
             } else if(grepl(grobi$name, pattern = "serialaxes_glyph"))  {

               polyline_grob <- grid::getGrob(grobi, "polyline")
               if(is.null(polyline_grob)) {
                 polyline_grob <- grid::getGrob(grobi, "polyline: showArea")
                 polyline_grob_name <-  "polyline: showArea"
                 polyline_grob$gp$fill <- newColor[k]
                 polyline_grob$gp$col <- NULL
               } else {
                 polyline_grob_name <-  "polyline"
                 polyline_grob$gp$col <- newColor[k]
               }

               grid::setGrob(
                 gTree = grobi,
                 gPath = polyline_grob_name,
                 newGrob = polyline_grob
               )

             } else if(grepl(grobi$name,pattern =  "polygon_glyph")) {

               gp <- grobi$gp
               gp$fill <- newColor[k]
               gp$col <- newColor[k]

               grid::editGrob(
                 grob = grobi,
                 gp = gp
               )

             } else if(grepl(grobi$name, pattern = "pointrange_glyph")) {

               pointGrob <- grid::getGrob(grobi, "point")
               lineGrob <- grid::getGrob(grobi, "range")

               pointGrob$gp$col <- newColor[k]
               lineGrob$gp$col <- newColor[k]

               tmpGrob <- grid::setGrob(
                 gTree = grobi,
                 gPath = "point",
                 newGrob = pointGrob
               )

               grid::setGrob(
                 gTree = tmpGrob,
                 gPath = "range",
                 newGrob = lineGrob
               )

             } else if(grepl(grobi$name, pattern = "text_glyph"))  {

               gp <- grobi$gp
               gp$col <- newColor[k]

               grid::editGrob(
                 grob = grobi,
                 gp = gp
               )

             } else if(grepl(grobi$name,pattern =  "image_glyph")) {

               imageBorderGrob <- grid::getGrob(grobi, "image_border")
               gp <- imageBorderGrob$gp
               gp$fill <- newColor[k]

               grid::setGrob(
                 gTree = grobi,
                 gPath = "image_border",
                 newGrob = grid::editGrob(
                   grob = imageBorderGrob,
                   gp = gp
                 )
               )

             } else {warning("Not implemented glyph", call. = FALSE); grobi}
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

set_color_grob.l_hist <- function(loon.grob, index, newColor, ...) {

  len <- length(index)

  if(len > 0) {

    lenNewColor <- length(newColor)
    if(lenNewColor == 1) {
      newColor <- rep(newColor, len)
    } else if (lenNewColor != len) {
      stop("color length is not equal to index length")
    } else NULL

    args <- list(...)
    changeColorWay <- args$changeColorWay

    newGrob <- grid::getGrob(loon.grob, "histogram")

    lapply(seq(len),
           function(k){

             i <- index[k]
             grobi <- newGrob$children[[i]]

             gp <- grobi$gp
             if(changeColorWay == "fill") {
               gp$fill <- newColor[k]
               gp$col <- bounder_color()
             } else if(changeColorWay == "col") {
               gp$col <- newColor[k]
             }


             newGrob$children[[i]] <<- grid::editGrob(
               grob = grobi,
               gp = gp
             )
           }
    )

    grid::setGrob(
      gTree = loon.grob,
      gPath = "histogram",
      newGrob = newGrob
    )
  } else {
    loon.grob
  }
}

set_color_grob.l_graph <- function(loon.grob, index, newColor, ...) {

  len <- length(index)

  if(len > 0) {

    lenNewColor <- length(newColor)
    if(lenNewColor == 1) {
      newColor <- rep(newColor, len)
    } else if (lenNewColor != len) {
      stop("color length is not equal to index length")
    } else NULL

    newGrob <- grid::getGrob(loon.grob, "graph nodes")

    lapply(seq(len),
           function(k) {

             i <- index[k]
             grobi <- newGrob$children[[i]]
             pch <- grobi$pch %||% 21
             gp <- grobi$gp

             if(pch %in% 21:24) {
               gp$col <- bounder_color()
               gp$fill <- newColor[k]
             } else {
               gp$col <- newColor[k]
             }

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

set_color_grob.l_serialaxes <- function(loon.grob, index, newColor, ...) {

  len <- length(index)

  if(len > 0) {

    args <- list(...)
    lenNewColor <- length(newColor)

    if(lenNewColor == 1) {
      newColor <- rep(newColor, len)
    } else if (lenNewColor != len) {
      stop("color length is not equal to index length")
    } else NULL

    axesGpath <- args$axesGpath
    axesGrob <- grid::getGrob(loon.grob, axesGpath)

    lapply(seq(len),
           function(k) {

             i <- index[k]
             grobi <- axesGrob$children[[i]]
             gp <- grobi$gp

             if(grepl(grobi$name, pattern = "showArea")) {

               gp$fill <- newColor[k]
               gp$col <- NULL

             } else {
               gp$col <- newColor[k]
             }

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
