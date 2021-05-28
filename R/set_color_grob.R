set_color_grob <- function(loon.grob, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_color_grob", obj)
}

set_color_grob.l_plot <- function(loon.grob, ...) {

  args <- list(...)
  index <- args$index
  pointsTreeName <- args$pointsTreeName

  if(pointsTreeName != "points: missing glyphs" && length(index) > 0) {

    color <- args$color
    size <- args$size
    pch <- args$pch

    if(length(color) == 1) {
      color <- rep(color, length(index))
    } else if (length(color) != length(index)) {
      stop("color length is not equal to index length")
    } else NULL

    newGrob <- grid::getGrob(loon.grob, pointsTreeName)

    lapply(index,
           function(i) {

             grobi <- newGrob$children[[i]]

             newGrob$children[[i]] <<- if(grepl(grobi$name, pattern = "primitive_glyph")) {

               pch_i <- if(is.null(pch)) grobi$pch else pch[i]
               size_i <- if(is.null(size)) grobi$gp$cex else size[i]

               editGrob(
                 grob = grobi,
                 gp = if(pch_i %in% 21:24) {
                   gpar(
                     fill = color[which(index %in% i)],
                     cex = size_i,
                     col = bounder_color()
                   )
                 } else {
                   gpar(
                     col = color[which(index %in% i)],
                     cex = size_i
                   )
                 }
               )
             } else if(grepl(grobi$name, pattern = "serialaxes_glyph"))  {

               polyline_grob <- grid::getGrob(grobi, "polyline")
               if(is.null(polyline_grob)) {
                 polyline_grob <- grid::getGrob(grobi, "polyline: showArea")
                 polyline_grob_name <-  "polyline: showArea"
                 polyline_grob$gp$fill <- color[which(index %in% i)]
               } else {
                 polyline_grob_name <-  "polyline"
                 polyline_grob$gp$col <- color[which(index %in% i)]
               }

               grid::setGrob(
                 gTree = grobi,
                 gPath = polyline_grob_name,
                 newGrob = polyline_grob
               )

             } else if(grepl(grobi$name,pattern =  "polygon_glyph")) {

               editGrob(
                 grob = grobi,
                 gp = gpar(
                   fill = color[which(index %in% i)],
                   col = color[which(index %in% i)],
                   fontsize = grobi$gp$lwd
                 )
               )

             } else if(grepl(grobi$name, pattern = "pointrange_glyph")) {

               point_grob <- grid::getGrob(grobi, "point")
               line_grob <- grid::getGrob(grobi, "range")

               point_grob$gp$col <- color[which(index %in% i)]
               line_grob$gp$col <- color[which(index %in% i)]

               tmpGrob <- grid::setGrob(
                 gTree = grobi,
                 gPath = "point",
                 newGrob = point_grob
               )

               grid::setGrob(
                 gTree = tmpGrob,
                 gPath = "range",
                 newGrob = line_grob
               )

             } else if(grepl(grobi$name, pattern = "text_glyph"))  {

               editGrob(
                 grob = grobi,
                 gp = gpar(
                   col = color[which(index %in% i)],
                   fontsize = size[which(index %in% i)] * loon_default_size()[["adjusted_size"]]
                 )
               )

             } else if(grepl(grobi$name,pattern =  "image_glyph")) {

               grid::setGrob(
                 gTree = grobi,
                 gPath = "image_border",
                 newGrob = editGrob(
                   grob = grid::getGrob(grobi, "image_border"),
                   gp = gpar(
                     fill = color[which(index %in% i)],
                     col =  NA
                   )
                 )
               )

             } else {warning("Not implemented glyph"); grobi}
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

set_color_grob.l_hist <- function(loon.grob, ...) {

  args <- list(...)
  index <- args$index

  if(length(index) > 0) {

    color <- args$color
    changeColorWay <- args$changeColorWay

    if(length(color) == 1) {
      color <- rep(color, length(index))
    } else if (length(color) != length(index)) {
      stop("color length is not equal to index length")
    } else NULL

    newGrob <- grid::getGrob(loon.grob, "histogram")

    lapply(index,
           function(i){
             newGrob$children[[i]] <<- editGrob(
               grob = newGrob$children[[i]],
               gp = if(changeColorWay == "fill") {
                 gpar(
                   fill = color[which(index %in% i)],
                   col = bounder_color()
                 )
               } else if(changeColorWay == "col") {
                 gpar(
                   col = color[which(index %in% i)]
                 )
               }
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

set_color_grob.l_graph <- function(loon.grob, ...) {

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

    newGrob <- grid::getGrob(loon.grob, "graph nodes")

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
                   col = bounder_color()
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

    grid::setGrob(
      gTree = loon.grob,
      gPath = "graph nodes",
      newGrob = newGrob
    )
  } else {
    loon.grob
  }
}

set_color_grob.l_serialaxes <- function(loon.grob, ...) {

  args <- list(...)
  index <- args$index

  if(length(index) > 0) {
    color <- args$color
    axesGpath <- args$axesGpath
    axesGrob <- grid::getGrob(loon.grob, axesGpath)

    if(length(color) == 1) {
      color <- rep(color, length(index))
    } else if (length(color) != length(index)) {
      stop("color length is not equal to index length")
    } else NULL

    lapply(index,
           function(i) {

             grobi <- axesGrob$children[[i]]
             axesGrob$children[[i]] <<- editGrob(
               grob = grobi,
               gp = if(grepl(grobi$name, pattern = "showArea")) {
                 gpar(fill = color[which(index %in% i)], col = NA)
               } else {
                 gpar(col = color[which(index %in% i)], lwd = grobi$gp$lwd)
               }
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
