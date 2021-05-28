set_linkingInfo <- function(loon.grob, output.grob, linkedInfo,
                            linkedStates, tabPanelName,
                            order, loonWidgetsInfo, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_linkingInfo", obj)
}

set_linkingInfo.l_plot <- function(loon.grob, output.grob,
                                   linkedInfo, linkedStates, tabPanelName,
                                   order, loonWidgetsInfo, ...) {

  # pointsTree name
  pointsTreeName <- loonWidgetsInfo$pointsTreeName

  if(pointsTreeName != "points: missing glyphs") {

    if(length(linkedStates) > 0) {

      args <- list(...)
      roundings <- args$roundings

      new.loon.grob <- grid::getGrob(loon.grob, pointsTreeName)
      new.output.grob <- grid::getGrob(output.grob, pointsTreeName)

      color <- if("color" %in% linkedStates) {
        linkedColor <- linkedInfo$color[order]
        NAid <- is.na(linkedColor)
        if(any(NAid)) {
          linkedColor[NAid] <- loonWidgetsInfo$color[NAid]
          linkedColor
        } else linkedColor
      } else loonWidgetsInfo$color

      selected <- if("selected" %in% linkedStates) {

        linkedselected <- linkedInfo$selected[order]
        NAid <- is.na(linkedselected)
        if(any(NAid)) {
          linkedselected[NAid] <- loonWidgetsInfo$selected[NAid]
          linkedselected
        } else linkedselected

      } else loonWidgetsInfo$selected

      active <- if("active" %in% linkedStates) {

        linkedactive <- linkedInfo$active[order]
        NAid <- is.na(linkedactive)
        if(any(NAid)) {
          linkedactive[NAid] <- loonWidgetsInfo$active[NAid]
          linkedactive
        } else linkedactive
      } else loonWidgetsInfo$active

      size <- if("size" %in% linkedStates) {

        linkedsize <- linkedInfo$size[order]
        NAid <- is.na(linkedsize)
        if(any(NAid)) {
          linkedsize[NAid] <- loonWidgetsInfo$size[NAid]
          linkedsize
        } else linkedsize
      } else loonWidgetsInfo$size

      pch <- if("pch" %in% linkedStates) {

        linkedpch <- linkedInfo$pch[order]
        NAid <- is.na(linkedpch)
        if(any(NAid)) {
          linkedpch[NAid] <- loonWidgetsInfo$pch[NAid]
          linkedpch
        } else linkedpch
      } else loonWidgetsInfo$pch

      lapply(seq(loonWidgetsInfo$N),
             function(i) {

               grobi <- new.loon.grob$children[[i]]

               if(grepl(grobi$name, pattern = "primitive_glyph")) {

                 # set pch, size and color

                 if("glyph" %in% linkedStates) {
                   grobi_pch <- pch[i]
                   if(!is.numeric(grobi_pch)) grobi_pch <- grobi$pch
                 } else grobi_pch <- grobi$pch

                 if("color" %in% linkedStates) {
                   grobi_color <- color[i]
                 } else {
                   if(grobi_pch %in% 21:24) {
                     grobi_color <- grobi$gp$fill
                   } else {
                     grobi_color <- grobi$gp$col
                   }
                 }

                 if("size" %in% linkedStates) {
                   grobi_size <- size[i]
                 } else grobi_size <- grobi$gp$cex

                 grobi <- editGrob(
                   grob = grobi,
                   gp = if(grobi_pch %in% 21:24) {
                     gpar(
                       fill = grobi_color,
                       cex = grobi_size,
                       col = bounder_color()
                     )
                   } else {
                     gpar(
                       col = grobi_color,
                       cex = grobi_size
                     )
                   },
                   pch = grobi_pch
                 )

                 # set deactive and reactive
                 if("active" %in% linkedStates) {
                   grobi <- if(!active[i]) {
                     do.call(grob, getGrobArgs(grobi))
                   } else {
                     do.call(pointsGrob, getGrobArgs(grobi))
                   }
                 }

                 new.loon.grob$children[[i]] <<- grobi

                 new.output.grob$children[[i]] <<- if("selected" %in% linkedStates && selected[i]) {

                   editGrob(
                     grob = grobi,
                     gp = if(grobi_pch %in% 21:24) {
                       gpar(
                         fill = select_color(),
                         cex = grobi_size,
                         col = bounder_color()
                       )
                     } else {
                       gpar(
                         col = select_color(),
                         cex = grobi_size
                       )
                     }
                   )
                 } else grobi


               } else if(grepl(grobi$name, pattern = "serialaxes_glyph"))  {

                 # reset boundary
                 boundary_grob <- grid::getGrob(grobi, "boundary")
                 if(is.null(boundary_grob)) {
                   boundary_grob <- grid::getGrob(grobi, "boundary: polylineGrob arguments")
                 }

                 # axes serialaxes
                 axesGrob <- grid::getGrob(grobi, "axes")
                 if(is.null(axesGrob)) {
                   axesGrob <- grid::getGrob(grobi, "axes: polylineGrob arguments")
                 }

                 serialaxesGrob <- grid::getGrob(grobi, "polyline")
                 if(is.null(serialaxesGrob)) {
                   serialaxesGrob <- grid::getGrob(grobi, "polyline: showArea")
                   serialaxesGrob_name <-  "polyline: showArea"
                 } else {
                   serialaxesGrob_name <-  "polyline"
                 }

                 # set size
                 if("size" %in% linkedStates) {

                   rounding <- roundings[[i]][[1]]

                   boundary_grob <- editGrob(
                     grob = boundary_grob,
                     x = get_unit(boundary_grob$x, as.numeric = FALSE) +
                       unit(rounding$boundary_grob_rounding$x * sqrt(size[i]/default_size()), "mm"),
                     y = get_unit(boundary_grob$y, as.numeric = FALSE) +
                       unit(rounding$boundary_grob_rounding$y * sqrt(size[i]/default_size()), "mm")
                   )

                   axesGrob <- editGrob(
                     grob = axesGrob,
                     x = get_unit(axesGrob$x, as.numeric = FALSE) +
                       unit(rounding$axesGrob_rounding$x * sqrt(size[i]/default_size()), "mm"),
                     y = get_unit(axesGrob$y, as.numeric = FALSE) +
                       unit(rounding$axesGrob_rounding$y * sqrt(size[i]/default_size()), "mm")
                   )

                   serialaxesGrob <- editGrob(
                     grob = serialaxesGrob,
                     x = get_unit(serialaxesGrob$x, as.numeric = FALSE) +
                       unit(rounding$serialaxesGrob_rounding$x * sqrt(size[i]/default_size()), "mm"),
                     y = get_unit(serialaxesGrob$y, as.numeric = FALSE) +
                       unit(rounding$serialaxesGrob_rounding$y * sqrt(size[i]/default_size()), "mm")
                   )
                 }

                 grobi <- if(grepl(grobi$name, pattern = "parallel")){
                   gTree(
                     children = gList(
                       boundary_grob,
                       axesGrob,
                       serialaxesGrob
                     ),
                     name =  grobi$name
                   )
                 } else {
                   gTree(
                     children = gList(
                       serialaxesGrob,
                       boundary_grob,
                       axesGrob
                     ),
                     name =  grobi$name
                   )
                 }

                 # set color
                 if("color" %in% linkedStates) {
                   serialaxesGrob <- grid::getGrob(grobi, serialaxesGrob_name)
                   if(serialaxesGrob_name == "polyline: showArea") {
                     serialaxesGrob$gp$fill <- color[i]
                   } else {
                     serialaxesGrob$gp$col <- color[i]
                   }

                   grobi <- grid::setGrob(
                     gTree = grobi,
                     gPath = serialaxesGrob_name,
                     newGrob = serialaxesGrob
                   )
                 }

                 # set deactive and reactive
                 if("active" %in% linkedStates) {
                   if(!active[i]) {
                     grobi <- gTree(
                       children = gList(
                         do.call(grob, getGrobArgs(grobi$children[[1]])),
                         do.call(grob, getGrobArgs(grobi$children[[2]])),
                         do.call(grob, getGrobArgs(grobi$children[[3]]))
                       ),
                       name = grobi$name
                     )
                   } else {
                     gTree_names <- grobi$childrenOrder

                     grobi <- if("polyline" %in% gTree_names) {

                       grid::setGrob(
                         gTree = grobi,
                         gPath = "polyline",
                         newGrob = do.call(grid::linesGrob, getGrobArgs(grid::getGrob(grobi, "polyline")))
                       )
                     } else if("polyline: showArea" %in% gTree_names) {

                       grid::setGrob(
                         gTree = grobi,
                         gPath = "polyline: showArea",
                         newGrob = do.call(grid::polygonGrob, getGrobArgs(grid::getGrob(grobi, "polyline: showArea")))
                       )
                     } else stop("serialaxes name does not match")

                     if("boundary" %in% gTree_names) {

                       grobi <- grid::setGrob(
                         gTree = grobi,
                         gPath = "boundary",
                         newGrob = do.call(grid::polylineGrob, getGrobArgs(grid::getGrob(grobi, "boundary")))
                       )
                     }

                     if("axes" %in% gTree_names) {

                       grobi <- grid::setGrob(
                         gTree = grobi,
                         gPath = "axes",
                         newGrob = do.call(grid::polylineGrob, getGrobArgs(grid::getGrob(grobi, "axes")))
                       )
                     }
                   }
                 }

                 new.loon.grob$children[[i]] <<- grobi
                 new.output.grob$children[[i]] <<- if("selected" %in% linkedStates && selected[i]) {

                   serialaxesGrob <- grid::getGrob(grobi, serialaxesGrob_name)

                   if(serialaxesGrob_name == "polyline: showArea") {
                     serialaxesGrob$gp$fill <- select_color()
                   } else {
                     serialaxesGrob$gp$col <- select_color()
                   }

                   grid::setGrob(
                     gTree = grobi,
                     gPath = serialaxesGrob_name,
                     newGrob = serialaxesGrob
                   )
                 } else grobi

               } else if(grepl(grobi$name, pattern = "polygon_glyph")) {

                 # set size
                 if("size" %in% linkedStates) {
                   rounding <- roundings[[i]][[1]]


                   grobi <- editGrob(
                     grob = grobi,
                     x = get_unit(grobi$x, as.numeric = FALSE) +
                       unit(rounding$x * sqrt(size[i]/default_size()), "mm"),
                     y = get_unit(grobi$y, as.numeric = FALSE) +
                       unit(rounding$y * sqrt(size[i]/default_size()), "mm")
                   )
                 }

                 # set color
                 if("color" %in% linkedStates) {
                   grobi <- editGrob(
                     grob = grobi,
                     gp = gpar(
                       fill = color[i],
                       col = color[i],
                       fontsize = grobi$gp$lwd
                     )
                   )
                 }

                 # set deactive and reactive
                 if("active" %in% linkedStates) {
                   grobi <- if(!active[i]) {

                     do.call(grob, getGrobArgs(grobi))
                   } else {

                     if(grepl(grobi$name, pattern = "showArea")) {

                       do.call(grid::polygonGrob, getGrobArgs(grobi))
                     } else {

                       do.call(grid::polylineGrob, getGrobArgs(grobi))
                     }
                   }
                 }

                 new.loon.grob$children[[i]] <<- grobi
                 new.output.grob$children[[i]] <<- if("selected" %in% linkedStates && selected[i]) {

                   editGrob(
                     grob = grobi,
                     gp = gpar(
                       fill = select_color(),
                       col = select_color(),
                       fontsize = grobi$gp$lwd
                     )
                   )
                 } else grobi

               } else if(grepl(grobi$name, pattern = "pointrange_glyph")) {

                 point_grob <- grid::getGrob(grobi, "point")
                 line_grob <- grid::getGrob(grobi, "range")

                 # set glyph
                 if("glyph" %in% linkedStates) {

                   point_grob_pch <- pch[i]
                   if(!is.numeric(point_grob_pch)) point_grob_pch <- point_grob$pch
                   point_grob$pch <- point_grob_pch

                   grobi <- grid::setGrob(
                     gTree = grobi,
                     gPath = "point",
                     newGrob = point_grob
                   )
                 } else point_grob_pch <- point_grob$pch

                 # set color
                 if("color" %in% linkedStates) {

                   if(point_grob_pch %in% 21:24) point_grob$gp$fill <- color[i] else point_grob$gp$col <- color[i]

                   line_grob$gp$col <- color[i]

                   grobi <- grid::setGrob(
                     gTree = grid::setGrob(
                       gTree = grobi,
                       gPath = "point",
                       newGrob = point_grob
                     ),
                     gPath = "range",
                     newGrob = line_grob
                   )
                 }

                 # set size
                 if("size" %in% linkedStates) {

                   grobi <- grid::setGrob(
                     gTree = grobi,
                     gPath = "point",
                     newGrob = editGrob(
                       grob = grid::getGrob(grobi, "point"),
                       gp = if(grobi$pch %in% 21:24) {
                         gpar(
                           fill = grobi$gp$fill,
                           cex = size[i],
                           col = grobi$gp$col
                         )
                       } else {
                         gpar(
                           col = grobi$gp$col,
                           cex = size[i]
                         )
                       }
                     )
                   )
                 }

                 # set deactive and reactive
                 if("active" %in% linkedStates) {
                   grobi <- if(!active[i]) {
                     gTree(
                       children = gList(
                         # point
                         do.call(grob, getGrobArgs(grid::getGrob(grobi, "point"))),
                         # range
                         do.call(grob, getGrobArgs(grid::getGrob(grobi, "range")))
                       ),
                       name = grobi$name
                     )
                   } else {
                     gTree(
                       children = gList(
                         # point
                         do.call(pointsGrob, getGrobArgs(grid::getGrob(grobi, "point"))),
                         # range
                         do.call(grid::linesGrob, getGrobArgs(grid::getGrob(grobi, "range")))
                       ),
                       name = grobi$name
                     )
                   }
                 }

                 new.loon.grob$children[[i]] <<- grobi
                 new.output.grob$children[[i]] <<- if("selected" %in% linkedStates && selected[i]) {

                   point_grob <- grid::getGrob(grobi, "point")
                   line_grob <- grid::getGrob(grobi, "range")

                   point_grob$gp$col <- select_color()
                   line_grob$gp$col <- select_color()

                   grobi <- grid::setGrob(
                     gTree = grid::setGrob(
                       gTree = grobi,
                       gPath = "point",
                       newGrob = point_grob
                     ),
                     gPath = "range",
                     newGrob = line_grob
                   )
                 } else grobi

               } else if(grepl(grobi$name, pattern = "text_glyph"))  {

                 # set color and size
                 if("color" %in% linkedStates) {
                   grobi_color <- color[i]
                 } else {
                   grobi_color <- grobi$gp$col
                 }

                 if("size" %in% linkedStates) {
                   grobi_size <- size[i] * loon_default_size()[["adjusted_size"]]
                 } else {
                   grobi_size <- grobi$gp$fontsize
                 }

                 grobi <- editGrob(
                   grob = grobi,
                   gp = gpar(
                     col = grobi_color,
                     fontsize = grobi_size
                   )
                 )

                 if("active" %in% linkedStates) {
                   grobi <- if(!active[i]) {
                     do.call(grid::grob, getGrobArgs(grobi))
                   } else {
                     do.call(grid::textGrob, getGrobArgs(grobi))
                   }
                 }

                 new.loon.grob$children[[i]] <<- grobi
                 new.output.grob$children[[i]] <<- if("selected" %in% linkedStates && selected[i]) {

                   editGrob(
                     grob = grobi,
                     gp = gpar(
                       col = select_color(),
                       fontsize = size[i] * loon_default_size()[["adjusted_size"]]
                     )
                   )
                 } else grobi

               } else if(grepl(grobi$name, pattern = "image_glyph")) {

                 # set color
                 if("color" %in% linkedStates) {
                   grobi <- grid::setGrob(
                     gTree = grobi,
                     gPath = "image_border",
                     newGrob = editGrob(
                       grob = grid::getGrob(grobi, "image_border"),
                       gp = gpar(
                         fill = color[i],
                         col =  NA
                       )
                     )
                   )
                 }

                 # set size
                 if("size" %in% linkedStates) {
                   rounding <- roundings[[i]][[1]]

                   image_border_grob <- grid::getGrob(grobi, "image_border")

                   image_border_grob <- editGrob(
                     grob = image_border_grob,
                     width = get_unit(image_border_grob$width, unit = "mm", as.numeric = FALSE) +
                       unit(rounding$width * sqrt(size[i]/default_size()), "cm"),
                     height = get_unit(image_border_grob$height, unit = "mm", as.numeric = FALSE) +
                       unit(rounding$height * sqrt(size[i]/default_size()), "cm")
                   )

                   image_grob <- editGrob(
                     grob = grid::getGrob(grobi, "image"),
                     width = unit(rounding$width * sqrt(size[i]/default_size()), "cm"),
                     height = unit(rounding$height * sqrt(size[i]/default_size()), "cm")
                   )

                   grobi <- gTree(
                     children = gList(
                       image_border_grob,
                       image_grob
                     ),
                     name =  grobi$name
                   )
                 }

                 # set deactive and reactive
                 if("active" %in% linkedStates) {
                   grobi <- if(!active[i]) {
                     gTree(
                       children = gList(
                         do.call(grob, getGrobArgs(grid::getGrob(grobi, "image_border"))),
                         do.call(grob, getGrobArgs(grid::getGrob(grobi, "image")))
                       ),
                       name = grobi$name
                     )
                   } else {

                     raster_args <- getGrobArgs(grid::getGrob(grobi, "image"))
                     names(raster_args) <- c("image", names(raster_args)[-1])

                     gTree(
                       children = gList(
                         do.call(grid::rectGrob, getGrobArgs(grid::getGrob(grobi, "image_border"))),
                         do.call(rasterGrob, raster_args)
                       ),
                       name = grobi$name
                     )
                   }
                 }

                 new.loon.grob$children[[i]] <<- grobi
                 new.output.grob$children[[i]] <<- if("selected" %in% linkedStates && selected[i]) {

                   grid::setGrob(
                     gTree = grobi,
                     gPath = "image_border",
                     newGrob = editGrob(
                       grob = grid::getGrob(grobi, "image_border"),
                       gp = gpar(
                         fill = select_color(),
                         col =  NA
                       )
                     )
                   )
                 } else grobi
               }
             }
      )

      loon.grob <- grid::setGrob(
        gTree = loon.grob,
        gPath = pointsTreeName,
        newGrob = new.loon.grob
      )
      output.grob <- grid::setGrob(
        gTree = output.grob,
        gPath = pointsTreeName,
        newGrob = new.output.grob
      )
      output.grob <- reorder_grob(output.grob,
                                  index = which(selected),
                                  pointsTreeName = pointsTreeName)

      # update loonWidgetsInfo
      loonWidgetsInfo$pch <- pch
      loonWidgetsInfo$color <- color
      loonWidgetsInfo$size <- size
      loonWidgetsInfo$selected <- selected
      loonWidgetsInfo$active <- active
    }
  }
  list(
    output.grob = output.grob,
    loon.grob = loon.grob,
    loonWidgetsInfo = loonWidgetsInfo
  )
}
