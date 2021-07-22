set_size_grob <- function(loon.grob, index, newSize, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_size_grob", obj)
}

set_size_grob.l_plot <- function(loon.grob, index, newSize, ...) {

  args <- list(...)
  pointsTreeName <- args$pointsTreeName
  len <- length(index)

  if(pointsTreeName != "points: missing glyphs" && len > 0) {

    roundings <- args$roundings
    oldSize <- args$oldSize
    newGrob <- grid::getGrob(loon.grob, pointsTreeName)

    lapply(seq(len),
           function(l) {
             i <- index[l]
             grobi <- newGrob$children[[i]]

             if(grepl(grobi$name, pattern = "primitive_glyph")) {

               gp <- grobi$gp
               gp$fontsize <- newSize[i]

               newGrob$children[[i]] <<- grid::editGrob(
                 grob = grobi,
                 gp = gp
               )
             } else if(grepl(grobi$name, pattern = "pointrange_glyph")) {

               pointGrob <- grid::getGrob(grobi, "point")
               gp <- pointGrob$gp
               gp$fontsize <- newSize[i]

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
               gp$fontsize <- newSize[i]

               newGrob$children[[i]] <<- grid::editGrob(
                 grob = grobi,
                 gp = gp
               )
             } else if(grepl(grobi$name, pattern = "serialaxes_glyph")) {

               # rounding <- roundings[[i]][[1]]

               type <- if(grepl(grobi$name, pattern = "parallel")) "parallel" else "radial"

               if(is.na(newSize[i])) stop("wrong size")
               # reset boundary
               boundaryGrob <- grid::getGrob(grobi, "boundary")
               if(is.null(boundaryGrob)) {
                 boundaryGrob <- grid::getGrob(grobi, "boundary: polylineGrob arguments")
               }

               # xBoundary <- rounding$boundaryGrobRounding$x * newSize[i]/oldSize[i]
               # yBoundary <- rounding$boundaryGrobRounding$y * newSize[i]/oldSize[i]
               xBoundary <- get_unit(boundaryGrob$x, unit = "cm", as.numeric = TRUE) * newSize[i]/oldSize[i]
               yBoundary <- get_unit(boundaryGrob$y, unit = "cm", as.numeric = TRUE) * newSize[i]/oldSize[i]

               boundaryGrob <- grid::editGrob(
                 grob = boundaryGrob,
                 x = get_unit(boundaryGrob$x, as.numeric = FALSE) +
                   unit(xBoundary, "cm"),
                 y = get_unit(boundaryGrob$y, as.numeric = FALSE) +
                   unit(yBoundary, "cm")
               )

               # axes serialaxes
               axesGrob <- grid::getGrob(grobi, "axes")
               if(is.null(axesGrob)) {
                 axesGrob <- grid::getGrob(grobi, "axes: polylineGrob arguments")
                 axesGrob_name <- "axes: polylineGrob arguments"
               } else {
                 axesGrob_name <- "axes"
               }

               xAxesRounding <- get_unit(axesGrob$x, unit = "cm", as.numeric = TRUE) * newSize[i]/oldSize[i]
               yAxesRounding <- get_unit(axesGrob$y, unit = "cm", as.numeric = TRUE) * newSize[i]/oldSize[i]
               # xAxesRounding <- rounding$axesGrobRounding$x * newSize[i]/oldSize[i]
               # yAxesRounding <- rounding$axesGrobRounding$y * newSize[i]/oldSize[i]

               axesGrob <- grid::editGrob(
                 grob = axesGrob,
                 x = get_unit(axesGrob$x, as.numeric = FALSE) +
                   unit(xAxesRounding, "cm"),
                 y = get_unit(axesGrob$y, as.numeric = FALSE) +
                   unit(yAxesRounding, "cm")
               )

               serialaxesGrob <- grid::getGrob(grobi, "polyline")
               if(is.null(serialaxesGrob)) {
                 serialaxesGrob <- grid::getGrob(grobi, "polyline: showArea")
               }

               xRounding <- get_unit(serialaxesGrob$x, unit = "cm", as.numeric = TRUE) * newSize[i]/oldSize[i]
               yRounding <- get_unit(serialaxesGrob$y, unit = "cm", as.numeric = TRUE) * newSize[i]/oldSize[i]
               # xRounding <- rounding$serialaxesGrobRounding$x * newSize[i]/oldSize[i]
               # yRounding <- rounding$serialaxesGrobRounding$y * newSize[i]/oldSize[i]

               serialaxesGrob <- grid::editGrob(
                 grob = serialaxesGrob,
                 x = get_unit(serialaxesGrob$x, as.numeric = FALSE) + unit(xRounding, "cm"),
                 y = get_unit(serialaxesGrob$y, as.numeric = FALSE) + unit(yRounding, "cm")
               )

               newGrob$children[[i]] <<- if(grepl(grobi$name,pattern =  "parallel")) {
                 gTree(
                   children = gList(
                     boundaryGrob,
                     axesGrob,
                     serialaxesGrob
                   ),
                   name =  grobi$name
                 )
               } else {
                 gTree(
                   children = gList(
                     serialaxesGrob,
                     boundaryGrob,
                     axesGrob
                   ),
                   name =  grobi$name
                 )
               }
             } else if(grepl(grobi$name, pattern = "polygon_glyph")) {

               # rounding <- roundings[[i]][[1]]

               xRounding <- get_unit(grobi$x, unit = "cm", as.numeric = TRUE) * newSize[i]/oldSize[i]
               yRounding <- get_unit(grobi$y, unit = "cm", as.numeric = TRUE) * newSize[i]/oldSize[i]

               newGrob$children[[i]] <<- grid::editGrob(
                 grob = grobi,
                 x = get_unit(grobi$x, as.numeric = FALSE) + unit(xRounding, "cm"),
                 y = get_unit(grobi$y, as.numeric = FALSE) + unit(yRounding, "cm")
               )
             } else if(grepl(grobi$name, pattern = "image_glyph")) {

               # rounding <- roundings[[i]][[1]]
               imageBorderGrob <- grid::getGrob(grobi, "image_border")

               imageBorderGrob <- grid::editGrob(
                 grob = imageBorderGrob,
                 width = imageBorderGrob$width + (newSize[i] - oldSize[i]) * pt2cm() * unit(2, "cm"),
                 height = imageBorderGrob$height + (newSize[i] - oldSize[i]) * pt2cm() * unit(2, "cm")
               )

               imageGrob <- grid::getGrob(grobi, "image")
               imageGrob <- grid::editGrob(
                 grob = imageGrob,
                 width = imageGrob$width + (newSize[i] - oldSize[i])  * pt2cm() * unit(2, "cm"),
                 height = imageGrob$height + (newSize[i] - oldSize[i]) * pt2cm() * unit(2, "cm")
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

set_size_grob.l_graph <- function(loon.grob, index, newSize, ...) {

  if(length(index) > 0) {

    newGrob <- grid::getGrob(loon.grob, "graph nodes")

    lapply(index,
           function(i) {

             grobi <- newGrob$children[[i]]
             gp <- grobi$gp
             gp$fontsize <- newSize[i]

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


set_size_grob.l_serialaxes <- function(loon.grob, index, newSize, ...) {

  args <- list(...)
  axesGpath <- args$axesGpath
  showArea <- args$showArea

  if(!showArea && length(index) > 0) {

    axesGrob <- grid::getGrob(loon.grob, axesGpath)

    lapply(index,
           function(i) {

             grobi <- axesGrob$children[[i]]
             gp <- grobi$gp
             gp$lwd <- newSize[i]

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
