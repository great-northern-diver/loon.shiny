set_specifiedGlyph_grob <- function(loon_grob, index, tmp, ...) {
  args <- list(...)  
  pointsTree_name <- args$pointsTree_name
  
  if(pointsTree_name != "points: missing glyphs" && length(index) > 0) {
    
    points_grob <- grid::getGrob(loon_grob, pointsTree_name)
    points_layer <- points_grob[["children"]]
    
    roundings <- args$roundings
    pch <- args$pch
    size <- args$size
    x <- args$x
    y <- args$y
    color <- args$color
    loon_color <- args$loon_color
    glyph_setting <- args$glyph_setting
    
    lapply(index,
           function(i) {
             
             point_layer <- points_layer[[i]]
             glyph_name <- paste(gsub("[[:digit:].,  ]", "", names(roundings[[i]])), 
                                 as.numeric(gsub("[^[:digit:].,  ]", "", names(points_layer[i]))), 
                                 sep = " ")
             rounding <- roundings[[i]][[1]]
             color <- if(tmp) loon_color$select_color[1] else color[i]
             
             if(stringr::str_detect(glyph_name, "polygon_glyph")) {
               
               poly_x <- rounding$x * sqrt(size[i]/default_size())
               poly_y <- rounding$y * sqrt(size[i]/default_size())
               
               showArea <- stringr::str_detect(glyph_name, "showArea")
               points_grob$children[[i]] <<- if(showArea) {
                 
                 grid::polygonGrob(x = grid::unit(x[i], "native") + grid::unit(poly_x, "mm"),
                                   y = grid::unit(y[i], "native") + grid::unit(poly_y, "mm"),
                                   gp = grid::gpar(
                                     fill = color
                                   ),
                                   name = glyph_name
                 )
               } else {
                 polylineGrob(x = grid::unit(x[i], "native") + grid::unit(c(poly_x, poly_x[1]), "mm"),
                              y = grid::unit(y[i], "native") + grid::unit(c(poly_y, poly_y[1]), "mm"),
                              gp = grid::gpar(
                                col =  color
                              ),
                              name = glyph_name
                 )
               }
             } else if(stringr::str_detect(glyph_name, "serialaxes_glyph")) {
               
               scale <- sqrt(size[i]/default_size())
               x_boundary <- rounding$boundary_grob_rounding$x * scale
               y_boundary <- rounding$boundary_grob_rounding$y * scale
               
               x_axesRounding <- rounding$axes_grob_rounding$x * scale
               y_axesRounding <- rounding$axes_grob_rounding$y * scale
               
               x_rounding <- rounding$serialaxes_grob_rounding$x * scale
               y_rounding <- rounding$serialaxes_grob_rounding$y * scale
               dimension <- length(x_axesRounding)/2
               box_color <- "#B3B3B3"
               
               is_radial <- stringr::str_detect(glyph_name, "radial")
               if(is_radial) {
                 
                 points_grob$children[[i]] <<- gTree (
                   children = gList(
                     if(glyph_setting$showArea) {
                       grid::polygonGrob(x = grid::unit(x[i], "native") + grid::unit(x_rounding, "mm"),
                                         y = grid::unit(y[i], "native") + grid::unit(y_rounding, "mm"),
                                         gp = grid::gpar(fill = color, 
                                                         col = NA),
                                         name = "polyline: showArea")
                     } else {
                       grid::linesGrob(x = grid::unit(x[i], "native") + grid::unit(x_rounding, "mm"),
                                       y = grid::unit(y[i], "native") + grid::unit(y_rounding, "mm"),
                                       gp = grid::gpar(col = color),
                                       name = "polyline")
                     },
                     loon::condGrob(
                       test = glyph_setting$showEnclosing,
                       grobFun = polylineGrob,
                       name = "boundary",
                       x = grid::unit(x[i], "native") + grid::unit(x_boundary, "mm"),
                       y = grid::unit(y[i], "native") + grid::unit(y_boundary, "mm"),
                       gp = grid::gpar(col = box_color)
                     ),
                     loon::condGrob(
                       test = glyph_setting$showAxes,
                       grobFun = polylineGrob,
                       name = "axes",
                       x = grid::unit(x[i], "native") + grid::unit(x_axesRounding, "mm"),
                       y = grid::unit(y[i], "native") + grid::unit(y_axesRounding, "mm"),
                       id = rep(1:dimension, 2),
                       gp = grid::gpar(col = box_color)
                     )
                   ), name = glyph_name
                 )
               }
               
               is_parallel <- stringr::str_detect(glyph_name, "parallel")
               if(is_parallel) {
                 
                 points_grob$children[[i]] <<- gTree (
                   children = gList(
                     loon::condGrob(
                       test = glyph_setting$showEnclosing,
                       grobFun = polylineGrob,
                       name = "boundary",
                       x = grid::unit(x[i], "native") + 
                         grid::unit((c(0, 0, 1, 0, 0, 1, 1, 1) - 0.5) * scale, "mm"),
                       y = grid::unit(y[i], "native") + 
                         grid::unit((c(0, 0, 0, 1, 1, 0, 1, 1) - 0.5) * scale, "mm"),
                       id = rep(1:4, 2),
                       gp = grid::gpar(col = box_color)
                     ),
                     loon::condGrob(
                       test = glyph_setting$showAxes,
                       grobFun = polylineGrob,
                       name = "axes",
                       x = grid::unit(x[i], "native") + grid::unit(x_axesRounding, "mm"),
                       y = grid::unit(y[i], "native") + grid::unit(y_axesRounding, "mm"),
                       id = rep(1:dimension, each = 2),
                       gp = grid::gpar(col = box_color)
                     ),
                     if(glyph_setting$showArea) {
                       grid::polygonGrob(x = grid::unit(x[i], "native") + grid::unit(x_rounding, "mm"),
                                         y = grid::unit(y[i], "native") + grid::unit(y_rounding, "mm"),
                                         gp = grid::gpar(fill = color, 
                                                         col = NA),
                                         name = "polyline: showArea")
                     } else {
                       grid::linesGrob(x = grid::unit(x[i], "native") + grid::unit(x_rounding, "mm"),
                                       y = grid::unit(y[i], "native") + grid::unit(y_rounding, "mm"),
                                       gp = grid::gpar(col = color),
                                       name = "polyline")
                     }
                   ), name = glyph_name
                 )
               }
               
             }  else if(stringr::str_detect(glyph_name, "image_glyph")) {
               
               width <- rounding$width * sqrt(size[i]/default_size())
               height <- rounding$height * sqrt(size[i]/default_size())
               
               points_grob$children[[i]] <<- gTree(
                 children = gList(
                   rectGrob(x = grid::unit(x[i], "native"), 
                            y = grid::unit(y[i], "native"),
                            just = "centre",
                            width = grid::unit(width, "cm") + grid::unit(2, "mm"), 
                            height = grid::unit(height, "cm") + grid::unit(2, "mm"),
                            gp = grid::gpar(
                              fill = color,
                              col = NA
                            ),
                            name = "image_border"),
                   rasterGrob(rounding$raster, 
                              x = grid::unit(x[i], "native"), 
                              y = grid::unit(y[i], "native"), 
                              just = "centre",
                              width = grid::unit(width, "cm"), 
                              height = grid::unit(height, "cm"),
                              name = "image")
                 ),
                 name = glyph_name
               )
               
             }  else if(stringr::str_detect(glyph_name, "text_glyph")) {
               
               points_grob$children[[i]] <<- textGrob(label = rounding$text,
                                                      x = grid::unit(x[i], "native"), 
                                                      y = grid::unit(y[i], "native"),
                                                      gp=grid::gpar(fontsize = loon_default_size()[["adjusted_size"]] * size[i],
                                                                    col = color),
                                                      name = glyph_name
               )
             }  else if(stringr::str_detect(glyph_name, "pointrange_glyph")) {
               
               showArea <- glyph_setting$showArea
               points_grob$children[[i]] <<- gTree(
                 children =  gList(
                   if(showArea) {
                     grid::pointsGrob(x = grid::unit(x[i], "native"), 
                                      y = grid::unit(y[i], "native"),
                                      gp = grid::gpar(fill = color,
                                                      cex = size[i]),
                                      pch = 21,
                                      name = "point")
                   } else {
                     grid::pointsGrob(x = grid::unit(x[i], "native"), 
                                      y = grid::unit(y[i], "native"),
                                      gp = grid::gpar(col = color,
                                                      cex = size[i]),
                                      pch = 19,
                                      name = "point")
                   },
                   grid::linesGrob(x = rounding$x_range,
                                   y = rounding$y_range,
                                   gp = grid::gpar(col = color),
                                   name = "range")
                 ),
                 name = glyph_name
               )
             } else stop("Unknown glyph name")
           })

    grid::setGrob(
      gTree = loon_grob,
      gPath = pointsTree_name,
      newGrob = points_grob
    )
    
  } else loon_grob
}