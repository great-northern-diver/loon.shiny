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

    newGrob <- grid::getGrob(loon.grob, pointsTreeName)

    lapply(index,
           function(i) {

             grobi <- newGrob$children[[i]]

             if(grepl(grobi$name, pattern = "primitive_glyph")) {

               gp <- grobi$gp
               gp$cex <- newSize[i]

               newGrob$children[[i]] <<- grid::editGrob(
                 grob = grobi,
                 gp = gp
               )
             } else if(grepl(grobi$name, pattern = "pointrange_glyph")) {

               pointGrob <- grid::getGrob(grobi, "point")
               gp <- pointGrob$gp
               gp$cex <- newSize[i]

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
               gp$fontsize <- newSize[i] * loon_default_size()[["adjusted_size"]]

               newGrob$children[[i]] <<- grid::editGrob(
                 grob = grobi,
                 gp = gp
               )
             } else if(grepl(grobi$name, pattern = "serialaxes_glyph")) {

               rounding <- roundings[[i]][[1]]

               if(is.na(newSize[i])) stop("wrong size")
               # reset boundary
               boundaryGrob <- grid::getGrob(grobi, "boundary")
               if(is.null(boundaryGrob)) {
                 boundaryGrob <- grid::getGrob(grobi, "boundary: polylineGrob arguments")
               }

               xBoundary <- rounding$boundaryGrobRounding$x * sqrt(newSize[i]/default_size())
               yBoundary <- rounding$boundaryGrobRounding$y * sqrt(newSize[i]/default_size())

               boundaryGrob <- grid::editGrob(
                 grob = boundaryGrob,
                 x = get_unit(boundaryGrob$x, as.numeric = FALSE) +
                   unit(xBoundary, "mm"),
                 y = get_unit(boundaryGrob$y, as.numeric = FALSE) +
                   unit(yBoundary, "mm")
               )

               # axes serialaxes
               axesGrob <- grid::getGrob(grobi, "axes")
               if(is.null(axesGrob)) {
                 axesGrob <- grid::getGrob(grobi, "axes: polylineGrob arguments")
                 axesGrob_name <- "axes: polylineGrob arguments"
               } else {
                 axesGrob_name <- "axes"
               }

               xAxesRounding <- rounding$axesGrobRounding$x * sqrt(newSize[i]/default_size())
               yAxesRounding <- rounding$axesGrobRounding$y * sqrt(newSize[i]/default_size())

               axesGrob <- grid::editGrob(
                 grob = axesGrob,
                 x = get_unit(axesGrob$x, as.numeric = FALSE) +
                   unit(xAxesRounding, "mm"),
                 y = get_unit(axesGrob$y, as.numeric = FALSE) +
                   unit(yAxesRounding, "mm")
               )

               serialaxesGrob <- grid::getGrob(grobi, "polyline")
               if(is.null(serialaxesGrob)) {
                 serialaxesGrob <- grid::getGrob(grobi, "polyline: showArea")
               }

               xRounding <- rounding$serialaxesGrobRounding$x * sqrt(newSize[i]/default_size())
               yRounding <- rounding$serialaxesGrobRounding$y * sqrt(newSize[i]/default_size())

               serialaxesGrob <- grid::editGrob(
                 grob = serialaxesGrob,
                 x = get_unit(serialaxesGrob$x, as.numeric = FALSE) + unit(xRounding, "mm"),
                 y = get_unit(serialaxesGrob$y, as.numeric = FALSE) + unit(yRounding, "mm")
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

               rounding <- roundings[[i]][[1]]

               xRounding <- rounding$x * sqrt(newSize[i]/default_size())
               yRounding <- rounding$y * sqrt(newSize[i]/default_size())

               newGrob$children[[i]] <<- grid::editGrob(
                 grob = grobi,
                 x = get_unit(grobi$x, as.numeric = FALSE) + unit(xRounding, "mm"),
                 y = get_unit(grobi$y, as.numeric = FALSE) + unit(yRounding, "mm")
               )
             } else if(grepl(grobi$name, pattern = "image_glyph")) {

               rounding <- roundings[[i]][[1]]

               imageBorderGrob <- grid::getGrob(grobi, "image_border")

               width <- rounding$width * sqrt(newSize[i]/default_size())
               height <- rounding$height * sqrt(newSize[i]/default_size())

               imageBorderGrob <- grid::editGrob(
                 grob = imageBorderGrob,
                 width = get_unit(imageBorderGrob$width, unit = "mm", as.numeric = FALSE) + unit(width, "cm"),
                 height = get_unit(imageBorderGrob$height, unit = "mm", as.numeric = FALSE) + unit(height, "cm")
               )

               imageGrob <- grid::getGrob(grobi, "image")
               imageGrob <- grid::editGrob(
                 grob = imageGrob,
                 width = unit(width, "cm"),
                 height = unit(height, "cm")
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
             gp$cex <- newSize[i]

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
