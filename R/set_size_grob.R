set_size_grob <- function(loon.grob, index, newSize, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_size_grob", obj)
}

set_size_grob.l_plot <- function(loon.grob, index, newSize, ...) {

  args <- list(...)
  pointsTreeName <- args$pointsTreeName

  if(pointsTreeName != "points: missing glyphs" & length(index) > 0) {

    roundings <- args$roundings
    pch <- args$pch

    newGrob <- grid::getGrob(loon.grob, pointsTreeName)

    lapply(index,
           function(i) {
             if(grepl(newGrob$children[[i]]$name, pattern = "primitive_glyph")) {
               newGrob$children[[i]] <<- editGrob(
                 grob = newGrob$children[[i]],
                 gp = if(pch[i] %in% 21:24) {
                   gpar(
                     fill = newGrob$children[[i]]$gp$fill,
                     cex = newSize[i],
                     col = newGrob$children[[i]]$gp$col
                   )
                 } else {
                   gpar(
                     col = newGrob$children[[i]]$gp$col,
                     cex = newSize[i]
                   )
                 }
               )
             } else if(grepl(newGrob$children[[i]]$name, pattern = "pointrange_glyph")) {

               newGrob$children[[i]] <<- grid::setGrob(
                 gTree = newGrob$children[[i]],
                 gPath = "point",
                 newGrob = editGrob(
                   grob = grid::getGrob(newGrob$children[[i]], "point"),
                   gp = if(pch[i] %in% 21:24) {
                     gpar(
                       fill = newGrob$children[[i]]$gp$fill,
                       cex = newSize[i],
                       col = newGrob$children[[i]]$gp$col
                     )
                   } else {
                     gpar(
                       col = newGrob$children[[i]]$gp$col,
                       cex = newSize[i]
                     )
                   }
                 )
               )
             } else if(grepl(newGrob$children[[i]]$name, pattern = "text_glyph")) {
               newGrob$children[[i]] <<- editGrob(
                 grob = newGrob$children[[i]],
                 gp = gpar(
                   col = newGrob$children[[i]]$gp$col,
                   fontsize = newSize[i] * loon_default_size()[["adjusted_size"]]
                 )
               )
             } else if(grepl(newGrob$children[[i]]$name, pattern = "serialaxes_glyph")) {

               serialaxes_tree <- newGrob$children[[i]]
               rounding <- roundings[[i]][[1]]

               if(is.na(newSize[i])) stop("wrong size")
               # reset boundary
               boundary_grob <- grid::getGrob(serialaxes_tree, "boundary")
               if(is.null(boundary_grob)) {
                 boundary_grob <- grid::getGrob(serialaxes_tree, "boundary: polylineGrob arguments")
               }

               x_boundary <- rounding$boundary_grob_rounding$x * sqrt(newSize[i]/default_size())
               y_boundary <- rounding$boundary_grob_rounding$y * sqrt(newSize[i]/default_size())

               boundary_grob <- editGrob(
                 grob = boundary_grob,
                 x = get_unit(boundary_grob$x, as.numeric = FALSE) +
                   unit(x_boundary, "mm"),
                 y = get_unit(boundary_grob$y, as.numeric = FALSE) +
                   unit(y_boundary, "mm")
               )

               # axes serialaxes
               axesGrob <- grid::getGrob(serialaxes_tree, "axes")
               if(is.null(axesGrob)) {
                 axesGrob <- grid::getGrob(serialaxes_tree, "axes: polylineGrob arguments")
                 axesGrob_name <- "axes: polylineGrob arguments"
               } else {
                 axesGrob_name <- "axes"
               }

               x_axesRounding <- rounding$axesGrob_rounding$x * sqrt(newSize[i]/default_size())
               y_axesRounding <- rounding$axesGrob_rounding$y * sqrt(newSize[i]/default_size())

               axesGrob <- editGrob(
                 grob = axesGrob,
                 x = get_unit(axesGrob$x, as.numeric = FALSE) +
                   unit(x_axesRounding, "mm"),
                 y = get_unit(axesGrob$y, as.numeric = FALSE) +
                   unit(y_axesRounding, "mm")
               )

               serialaxesGrob <- grid::getGrob(newGrob$children[[i]], "polyline")
               if(is.null(serialaxesGrob)) {
                 serialaxesGrob <- grid::getGrob(newGrob$children[[i]], "polyline: showArea")
               }

               x_rounding <- rounding$serialaxesGrob_rounding$x * sqrt(newSize[i]/default_size())
               y_rounding <- rounding$serialaxesGrob_rounding$y * sqrt(newSize[i]/default_size())

               serialaxesGrob <- editGrob(
                 grob = serialaxesGrob,
                 x = get_unit(serialaxesGrob$x, as.numeric = FALSE) + unit(x_rounding, "mm"),
                 y = get_unit(serialaxesGrob$y, as.numeric = FALSE) + unit(y_rounding, "mm")
               )

               newGrob$children[[i]] <<- if(grepl(newGrob$children[[i]]$name,pattern =  "parallel")) {
                 gTree(
                   children = gList(
                     boundary_grob,
                     axesGrob,
                     serialaxesGrob
                   ),
                   name =  newGrob$children[[i]]$name
                 )
               } else {
                 gTree(
                   children = gList(
                     serialaxesGrob,
                     boundary_grob,
                     axesGrob
                   ),
                   name =  newGrob$children[[i]]$name
                 )
               }
             } else if(grepl(newGrob$children[[i]]$name, pattern = "polygon_glyph")) {
               rounding <- roundings[[i]][[1]]

               x_rounding <- rounding$x * sqrt(newSize[i]/default_size())
               y_rounding <- rounding$y * sqrt(newSize[i]/default_size())

               newGrob$children[[i]] <<- editGrob(
                 grob = newGrob$children[[i]],
                 x = get_unit(newGrob$children[[i]]$x, as.numeric = FALSE) + unit(x_rounding, "mm"),
                 y = get_unit(newGrob$children[[i]]$y, as.numeric = FALSE) + unit(y_rounding, "mm")
               )
             } else if(grepl(newGrob$children[[i]]$name, pattern = "image_glyph")) {
               rounding <- roundings[[i]][[1]]

               image_border_grob <- grid::getGrob(newGrob$children[[i]], "image_border")

               width <- rounding$width * sqrt(newSize[i]/default_size())
               height <- rounding$height * sqrt(newSize[i]/default_size())

               image_border_grob <- editGrob(
                 grob = image_border_grob,
                 width = get_unit(image_border_grob$width, unit = "mm", as.numeric = FALSE) + unit(width, "cm"),
                 height = get_unit(image_border_grob$height, unit = "mm", as.numeric = FALSE) + unit(height, "cm")
               )

               image_grob <- grid::getGrob(newGrob$children[[i]], "image")
               image_grob <- editGrob(
                 grob = image_grob,
                 width = unit(width, "cm"),
                 height = unit(height, "cm")
               )

               newGrob$children[[i]] <<- gTree(
                 children = gList(
                   image_border_grob,
                   image_grob
                 ),
                 name =  newGrob$children[[i]]$name
               )
             } else stop("not inplemented")
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
    args <- list(...)
    pch <- args$pch

    newGrob <- grid::getGrob(loon.grob, "graph nodes")

    lapply(index,
           function(i) {

             newGrob$children[[i]] <<- editGrob(
               grob = newGrob$children[[i]],
               gp = if(pch[i] %in% 21:24) {
                 gpar(
                   fill = newGrob$children[[i]]$gp$fill,
                   cex = newSize[i],
                   col = newGrob$children[[i]]$gp$col
                 )
               } else {
                 gpar(
                   col = newGrob$children[[i]]$gp$col,
                   cex = newSize[i]
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


set_size_grob.l_serialaxes <- function(loon.grob, index, newSize, ...) {

  args <- list(...)
  axesGpath <- args$axesGpath
  showArea <- args$showArea

  if(!showArea & length(index) > 0) {
    axesGrob <- grid::getGrob(loon.grob, axesGpath)

    lapply(index,
           function(i) {

             grobi <- axesGrob$children[[i]]

             axesGrob$children[[i]] <<- editGrob(
               grob = grobi,
               gp = gpar(
                 col = grobi$gp$col,
                 lwd = newSize[i]
               )
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
