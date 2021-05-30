set_specifiedGlyph_grob <- function(loon.grob, index, tmp, ...) {
  args <- list(...)
  pointsTreeName <- args$pointsTreeName

  if(pointsTreeName != "points: missing glyphs" && length(index) > 0) {

    points_grob <- grid::getGrob(loon.grob, pointsTreeName)
    points_layer <- points_grob[["children"]]

    roundings <- args$roundings
    pch <- args$pch
    size <- args$size
    x <- args$x
    y <- args$y
    color <- args$color
    alpha <- args$alpha
    loonColor <- args$loonColor
    nonePrimitiveGlyphSettings <- args$nonePrimitiveGlyphSettings

    lapply(index,
           function(i) {

             point_layer <- points_layer[[i]]
             glyphNames <- paste(gsub("[[:digit:].,  ]", "", names(roundings[[i]])),
                                 as.numeric(gsub("[^[:digit:].,  ]", "", names(points_layer[i]))),
                                 sep = " ")
             rounding <- roundings[[i]][[1]]
             color <- if(tmp) select_color() else color[i]

             if(grepl(glyphNames, pattern = "polygon_glyph")) {

               poly_x <- rounding$x * sqrt(size[i]/default_size())
               poly_y <- rounding$y * sqrt(size[i]/default_size())

               showArea <- grepl(glyphNames, pattern = "showArea")
               points_grob$children[[i]] <<- if(showArea) {

                 grid::polygonGrob(x = grid::unit(x[i], "native") + grid::unit(poly_x, "mm"),
                                   y = grid::unit(y[i], "native") + grid::unit(poly_y, "mm"),
                                   gp = grid::gpar(
                                     fill = color,
                                     alpha = alpha[i]
                                   ),
                                   name = glyphNames
                 )
               } else {
                 grid::polylineGrob(x = grid::unit(x[i], "native") + grid::unit(c(poly_x, poly_x[1]), "mm"),
                                    y = grid::unit(y[i], "native") + grid::unit(c(poly_y, poly_y[1]), "mm"),
                                    gp = grid::gpar(
                                      col =  color,
                                      alpha = alpha[i]
                                    ),
                                    name = glyphNames
                 )
               }
             } else if(grepl(glyphNames, pattern = "serialaxes_glyph")) {

               scale <- sqrt(size[i]/default_size())
               xBoundary <- rounding$boundaryGrobRounding$x * scale
               yBoundary <- rounding$boundaryGrobRounding$y * scale

               xAxesRounding <- rounding$axesGrobRounding$x * scale
               yAxesRounding <- rounding$axesGrobRounding$y * scale

               xRounding <- rounding$serialaxesGrobRounding$x * scale
               yRounding <- rounding$serialaxesGrobRounding$y * scale
               dimension <- length(xAxesRounding)/2
               box_color <- "#B3B3B3"

               is_radial <- grepl(glyphNames, pattern = "radial")
               if(is_radial) {

                 points_grob$children[[i]] <<- gTree (
                   children = gList(
                     if(nonePrimitiveGlyphSettings$showArea) {
                       grid::polygonGrob(x = grid::unit(x[i], "native") + grid::unit(xRounding, "mm"),
                                         y = grid::unit(y[i], "native") + grid::unit(yRounding, "mm"),
                                         gp = grid::gpar(fill = color,
                                                         col = NA,
                                                         alpha = alpha[i]),
                                         name = "polyline: showArea")
                     } else {
                       grid::linesGrob(x = grid::unit(x[i], "native") + grid::unit(xRounding, "mm"),
                                       y = grid::unit(y[i], "native") + grid::unit(yRounding, "mm"),
                                       gp = grid::gpar(col = color,
                                                       alpha = alpha[i]),
                                       name = "polyline")
                     },
                     loon::condGrob(
                       test = nonePrimitiveGlyphSettings$showEnclosing,
                       grobFun = grid::polylineGrob,
                       name = "boundary",
                       x = grid::unit(x[i], "native") + grid::unit(xBoundary, "mm"),
                       y = grid::unit(y[i], "native") + grid::unit(yBoundary, "mm"),
                       gp = grid::gpar(col = box_color)
                     ),
                     loon::condGrob(
                       test = nonePrimitiveGlyphSettings$showAxes,
                       grobFun = grid::polylineGrob,
                       name = "axes",
                       x = grid::unit(x[i], "native") + grid::unit(xAxesRounding, "mm"),
                       y = grid::unit(y[i], "native") + grid::unit(yAxesRounding, "mm"),
                       id = rep(1:dimension, 2),
                       gp = grid::gpar(col = box_color)
                     )
                   ), name = glyphNames
                 )
               }

               is_parallel <- grepl(glyphNames, pattern = "parallel")
               if(is_parallel) {

                 points_grob$children[[i]] <<- gTree (
                   children = gList(
                     loon::condGrob(
                       test = nonePrimitiveGlyphSettings$showEnclosing,
                       grobFun = grid::polylineGrob,
                       name = "boundary",
                       x = grid::unit(x[i], "native") +
                         grid::unit((c(0, 0, 1, 0, 0, 1, 1, 1) - 0.5) * scale, "mm"),
                       y = grid::unit(y[i], "native") +
                         grid::unit((c(0, 0, 0, 1, 1, 0, 1, 1) - 0.5) * scale, "mm"),
                       id = rep(1:4, 2),
                       gp = grid::gpar(col = box_color)
                     ),
                     loon::condGrob(
                       test = nonePrimitiveGlyphSettings$showAxes,
                       grobFun = grid::polylineGrob,
                       name = "axes",
                       x = grid::unit(x[i], "native") + grid::unit(xAxesRounding, "mm"),
                       y = grid::unit(y[i], "native") + grid::unit(yAxesRounding, "mm"),
                       id = rep(1:dimension, each = 2),
                       gp = grid::gpar(col = box_color)
                     ),
                     if(nonePrimitiveGlyphSettings$showArea) {
                       grid::polygonGrob(x = grid::unit(x[i], "native") + grid::unit(xRounding, "mm"),
                                         y = grid::unit(y[i], "native") + grid::unit(yRounding, "mm"),
                                         gp = grid::gpar(fill = color,
                                                         col = NA,
                                                         alpha = alpha[i]),
                                         name = "polyline: showArea")
                     } else {
                       grid::linesGrob(x = grid::unit(x[i], "native") + grid::unit(xRounding, "mm"),
                                       y = grid::unit(y[i], "native") + grid::unit(yRounding, "mm"),
                                       gp = grid::gpar(col = color,
                                                       alpha = alpha[i]),
                                       name = "polyline")
                     }
                   ), name = glyphNames
                 )
               }

             }  else if(grepl(glyphNames, pattern = "image_glyph")) {

               width <- rounding$width * sqrt(size[i]/default_size())
               height <- rounding$height * sqrt(size[i]/default_size())

               points_grob$children[[i]] <<- gTree(
                 children = gList(
                   grid::rectGrob(x = grid::unit(x[i], "native"),
                                  y = grid::unit(y[i], "native"),
                                  just = "centre",
                                  width = grid::unit(width, "cm") + grid::unit(2, "mm"),
                                  height = grid::unit(height, "cm") + grid::unit(2, "mm"),
                                  gp = grid::gpar(
                                    fill = color,
                                    col = NA,
                                    alpha = alpha[i]
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
                 name = glyphNames
               )

             }  else if(grepl(glyphNames, pattern = "text_glyph")) {

               points_grob$children[[i]] <<- grid::textGrob(label = rounding$text,
                                                            x = grid::unit(x[i], "native"),
                                                            y = grid::unit(y[i], "native"),
                                                            gp=grid::gpar(fontsize = loon_default_size()[["adjusted_size"]] * size[i],
                                                                          col = color,
                                                                          alpha = alpha[i]),
                                                            name = glyphNames
               )
             }  else if(grepl(glyphNames,pattern =  "pointrange_glyph")) {

               showArea <- nonePrimitiveGlyphSettings$showArea
               points_grob$children[[i]] <<- gTree(
                 children =  gList(
                   if(showArea) {
                     grid::pointsGrob(x = grid::unit(x[i], "native"),
                                      y = grid::unit(y[i], "native"),
                                      gp = grid::gpar(fill = color,
                                                      cex = size[i],
                                                      alpha = alpha[i]),
                                      pch = 21,
                                      name = "point")
                   } else {
                     grid::pointsGrob(x = grid::unit(x[i], "native"),
                                      y = grid::unit(y[i], "native"),
                                      gp = grid::gpar(col = color,
                                                      cex = size[i],
                                                      alpha = alpha[i]),
                                      pch = 19,
                                      name = "point")
                   },
                   grid::linesGrob(x = rounding$x_range,
                                   y = rounding$y_range,
                                   gp = grid::gpar(col = color),
                                   name = "range")
                 ),
                 name = glyphNames
               )
             } else stop("Unknown glyph name")
           })

    grid::setGrob(
      gTree = loon.grob,
      gPath = pointsTreeName,
      newGrob = points_grob
    )

  } else loon.grob
}
