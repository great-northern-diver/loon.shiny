set_size_grob <- function(loon_grob, index, new_size, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("set_size_grob", obj)
}

set_size_grob.l_plot <- function(loon_grob, index, new_size, ...) {
  
  args <- list(...)  
  pointsTree_name <- args$pointsTree_name

  if(pointsTree_name != "points: missing glyphs" & length(index) > 0) {

    roundings <- args$roundings
    pch <- args$pch
    
    newGrob <- getGrob(loon_grob, pointsTree_name)

    lapply(index,
           function(i) {
             if(str_detect(newGrob$children[[i]]$name, "primitive_glyph")) {
               newGrob$children[[i]] <<- editGrob(
                 grob = newGrob$children[[i]],
                 gp = if(pch[i] %in% 21:24) {
                   gpar(
                     fill = newGrob$children[[i]]$gp$fill,
                     cex = new_size[i],
                     col = newGrob$children[[i]]$gp$col
                   )
                 } else {
                   gpar(
                     col = newGrob$children[[i]]$gp$col,
                     cex = new_size[i]
                   )
                 }
               )
             } else if(str_detect(newGrob$children[[i]]$name, "pointrange_glyph")) {

               newGrob$children[[i]] <<- setGrob(
                 gTree = newGrob$children[[i]],
                 gPath = "point",
                 newGrob = editGrob(
                   grob = getGrob(newGrob$children[[i]], "point"),
                   gp = if(pch[i] %in% 21:24) {
                     gpar(
                       fill = newGrob$children[[i]]$gp$fill,
                       cex = new_size[i],
                       col = newGrob$children[[i]]$gp$col
                     )
                   } else {
                     gpar(
                       col = newGrob$children[[i]]$gp$col,
                       cex = new_size[i]
                     )
                   }
                 )
               )
             } else if(str_detect(newGrob$children[[i]]$name, "text_glyph")) {
               newGrob$children[[i]] <<- editGrob(
                 grob = newGrob$children[[i]],
                 gp = gpar(
                   col = newGrob$children[[i]]$gp$col,
                   fontsize = new_size[i] * loon_default_size()[["adjusted_size"]]
                 )
               )
             } else if(str_detect(newGrob$children[[i]]$name, "serialaxes_glyph")) {

               serialaxes_tree <- newGrob$children[[i]]
               rounding <- roundings[[i]][[1]]

               if(is.na(new_size[i])) stop("wrong size") 
               # reset boundary
               boundary_grob <- getGrob(serialaxes_tree, "boundary")
               if(is.null(boundary_grob)) {
                 boundary_grob <- getGrob(serialaxes_tree, "boundary: polylineGrob arguments")
               }

               x_boundary <- rounding$boundary_grob_rounding$x * sqrt(new_size[i]/default_size())
               y_boundary <- rounding$boundary_grob_rounding$y * sqrt(new_size[i]/default_size())
               
               boundary_grob <- editGrob(
                 grob = boundary_grob,
                 x = get_unit(boundary_grob$x, as.numeric = FALSE) +
                   unit(x_boundary, "mm"),
                 y = get_unit(boundary_grob$y, as.numeric = FALSE) +
                   unit(y_boundary, "mm")
               )

               # axes serialaxes
               axes_grob <- getGrob(serialaxes_tree, "axes")
               if(is.null(axes_grob)) {
                 axes_grob <- getGrob(serialaxes_tree, "axes: polylineGrob arguments")
                 axes_grob_name <- "axes: polylineGrob arguments"
               } else {
                 axes_grob_name <- "axes"
               }

               x_axesRounding <- rounding$axes_grob_rounding$x * sqrt(new_size[i]/default_size())
               y_axesRounding <- rounding$axes_grob_rounding$y * sqrt(new_size[i]/default_size())
               
               axes_grob <- editGrob(
                 grob = axes_grob,
                 x = get_unit(axes_grob$x, as.numeric = FALSE) +
                   unit(x_axesRounding, "mm"),
                 y = get_unit(axes_grob$y, as.numeric = FALSE) +
                   unit(y_axesRounding, "mm")
               )

               serialaxes_grob <- getGrob(newGrob$children[[i]], "polyline")
               if(is.null(serialaxes_grob)) {
                 serialaxes_grob <- getGrob(newGrob$children[[i]], "polyline: showArea")
               }

               x_rounding <- rounding$serialaxes_grob_rounding$x * sqrt(new_size[i]/default_size())
               y_rounding <- rounding$serialaxes_grob_rounding$y * sqrt(new_size[i]/default_size())
               
               serialaxes_grob <- editGrob(
                 grob = serialaxes_grob,
                 x = get_unit(serialaxes_grob$x, as.numeric = FALSE) + unit(x_rounding, "mm"),
                 y = get_unit(serialaxes_grob$y, as.numeric = FALSE) + unit(y_rounding, "mm")
               )

               newGrob$children[[i]] <<- if(str_detect(newGrob$children[[i]]$name, "parallel")) {
                 gTree(
                   children = gList(
                     boundary_grob,
                     axes_grob,
                     serialaxes_grob
                   ),
                   name =  newGrob$children[[i]]$name
                 )
               } else {
                 gTree(
                   children = gList(
                     serialaxes_grob,
                     boundary_grob,
                     axes_grob
                   ),
                   name =  newGrob$children[[i]]$name
                 )
               }
             } else if(str_detect(newGrob$children[[i]]$name, "polygon_glyph")) {
               rounding <- roundings[[i]][[1]]

               x_rounding <- rounding$x * sqrt(new_size[i]/default_size())
               y_rounding <- rounding$y * sqrt(new_size[i]/default_size())
               
               newGrob$children[[i]] <<- editGrob(
                 grob = newGrob$children[[i]],
                 x = get_unit(newGrob$children[[i]]$x, as.numeric = FALSE) + unit(x_rounding, "mm"),
                 y = get_unit(newGrob$children[[i]]$y, as.numeric = FALSE) + unit(y_rounding, "mm")
               )
             } else if(str_detect(newGrob$children[[i]]$name, "image_glyph")) {
               rounding <- roundings[[i]][[1]]

               image_border_grob <- getGrob(newGrob$children[[i]], "image_border")
              
               width <- rounding$width * sqrt(new_size[i]/default_size())
               height <- rounding$height * sqrt(new_size[i]/default_size())
               
               image_border_grob <- editGrob(
                 grob = image_border_grob,
                 width = get_unit(image_border_grob$width, unit = "mm", as.numeric = FALSE) + unit(width, "cm"),
                 height = get_unit(image_border_grob$height, unit = "mm", as.numeric = FALSE) + unit(height, "cm")
               )

               image_grob <- getGrob(newGrob$children[[i]], "image")
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

    setGrob(
      gTree = loon_grob,
      gPath = pointsTree_name,
      newGrob = newGrob
    )
  } else {
    loon_grob
  }
}

set_size_grob.l_graph <- function(loon_grob, index, new_size, ...) {

  if(length(index) > 0) {
    args <- list(...)
    pch <- args$pch
    
    newGrob <- getGrob(loon_grob, "graph nodes")
    
    lapply(index,
           function(i) {
             
             newGrob$children[[i]] <<- editGrob(
               grob = newGrob$children[[i]],
               gp = if(pch[i] %in% 21:24) {
                 gpar(
                   fill = newGrob$children[[i]]$gp$fill,
                   cex = new_size[i],
                   col = newGrob$children[[i]]$gp$col
                 )
               } else {
                 gpar(
                   col = newGrob$children[[i]]$gp$col,
                   cex = new_size[i]
                 )
               }
             )
             
           }
    )
    
    setGrob(
      gTree = loon_grob,
      gPath = "graph nodes",
      newGrob = newGrob
    )
  } else {
    loon_grob
  }
}


set_size_grob.l_serialaxes <- function(loon_grob, index, new_size, ...) {

  args <- list(...)
  axes_gPath <- args$axes_gPath
  showArea <- args$showArea

  if(!showArea & length(index) > 0) {
    axes_grob <- getGrob(loon_grob, axes_gPath)

    lapply(index,
           function(i) {

             grobi <- axes_grob$children[[i]]

             axes_grob$children[[i]] <<- editGrob(
               grob = grobi,
               gp = gpar(
                 col = grobi$gp$col,
                 lwd = new_size[i]
               )
             )
           }
    )

    setGrob(
      gTree = loon_grob,
      gPath = axes_gPath,
      newGrob = axes_grob
    )
  } else {
    loon_grob
  }
}
