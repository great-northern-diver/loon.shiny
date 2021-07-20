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
                 } else grobi_size <- grobi$gp$fontsize

                 grobi <- grid::editGrob(
                   grob = grobi,
                   gp = if(grobi_pch %in% 21:24) {
                     gpar(
                       fill = grobi_color,
                       fontsize = grobi_size,
                       col = bounder_color()
                     )
                   } else {
                     gpar(
                       col = grobi_color,
                       fontsize = grobi_size
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

                   grid::editGrob(
                     grob = grobi,
                     gp = if(grobi_pch %in% 21:24) {
                       gpar(
                         fill = select_color(),
                         fontsize = grobi_size,
                         col = bounder_color()
                       )
                     } else {
                       gpar(
                         col = select_color(),
                         fontsize = grobi_size
                       )
                     }
                   )
                 } else grobi


               } else if(grepl(grobi$name, pattern = "serialaxes_glyph"))  {

                 # reset boundary
                 boundaryGrob <- grid::getGrob(grobi, "boundary")
                 if(is.null(boundaryGrob)) {
                   boundaryGrob <- grid::getGrob(grobi, "boundary: polylineGrob arguments")
                 }

                 # axes serialaxes
                 axesGrob <- grid::getGrob(grobi, "axes")
                 if(is.null(axesGrob)) {
                   axesGrob <- grid::getGrob(grobi, "axes: polylineGrob arguments")
                 }

                 serialaxesGrob <- grid::getGrob(grobi, "polyline")
                 if(is.null(serialaxesGrob)) {
                   serialaxesGrob <- grid::getGrob(grobi, "polyline: showArea")
                   serialaxesGrobName <-  "polyline: showArea"
                 } else {
                   serialaxesGrobName <-  "polyline"
                 }

                 # set size
                 if("size" %in% linkedStates) {

                   rounding <- roundings[[i]][[1]]

                   boundaryGrob <- grid::editGrob(
                     grob = boundaryGrob,
                     x = get_unit(boundaryGrob$x, as.numeric = FALSE) +
                       unit(rounding$boundaryGrobRounding$x * sqrt(size[i]/loon_default_size("adjust")), "cm"),
                     y = get_unit(boundaryGrob$y, as.numeric = FALSE) +
                       unit(rounding$boundaryGrobRounding$y * sqrt(size[i]/loon_default_size("adjust")), "cm")
                   )

                   axesGrob <- grid::editGrob(
                     grob = axesGrob,
                     x = get_unit(axesGrob$x, as.numeric = FALSE) +
                       unit(rounding$axesGrobRounding$x * sqrt(size[i]/loon_default_size("adjust")), "cm"),
                     y = get_unit(axesGrob$y, as.numeric = FALSE) +
                       unit(rounding$axesGrobRounding$y * sqrt(size[i]/loon_default_size("adjust")), "cm")
                   )

                   serialaxesGrob <- grid::editGrob(
                     grob = serialaxesGrob,
                     x = get_unit(serialaxesGrob$x, as.numeric = FALSE) +
                       unit(rounding$serialaxesGrobRounding$x * sqrt(size[i]/loon_default_size("adjust")), "cm"),
                     y = get_unit(serialaxesGrob$y, as.numeric = FALSE) +
                       unit(rounding$serialaxesGrobRounding$y * sqrt(size[i]/loon_default_size("adjust")), "cm")
                   )
                 }

                 grobi <- if(grepl(grobi$name, pattern = "parallel")){
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

                 # set color
                 if("color" %in% linkedStates) {
                   serialaxesGrob <- grid::getGrob(grobi, serialaxesGrobName)
                   if(serialaxesGrobName == "polyline: showArea") {
                     serialaxesGrob$gp$fill <- color[i]
                   } else {
                     serialaxesGrob$gp$col <- color[i]
                   }

                   grobi <- grid::setGrob(
                     gTree = grobi,
                     gPath = serialaxesGrobName,
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

                   serialaxesGrob <- grid::getGrob(grobi, serialaxesGrobName)

                   if(serialaxesGrobName == "polyline: showArea") {
                     serialaxesGrob$gp$fill <- select_color()
                   } else {
                     serialaxesGrob$gp$col <- select_color()
                   }

                   grid::setGrob(
                     gTree = grobi,
                     gPath = serialaxesGrobName,
                     newGrob = serialaxesGrob
                   )
                 } else grobi

               } else if(grepl(grobi$name, pattern = "polygon_glyph")) {

                 # set size
                 if("size" %in% linkedStates) {
                   rounding <- roundings[[i]][[1]]


                   grobi <- grid::editGrob(
                     grob = grobi,
                     x = get_unit(grobi$x, as.numeric = FALSE) +
                       unit(rounding$x * sqrt(size[i]/loon_default_size("adjust")), "cm"),
                     y = get_unit(grobi$y, as.numeric = FALSE) +
                       unit(rounding$y * sqrt(size[i]/loon_default_size("adjust")), "cm")
                   )
                 }

                 # set color
                 if("color" %in% linkedStates) {
                   grobi <- grid::editGrob(
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

                   grid::editGrob(
                     grob = grobi,
                     gp = gpar(
                       fill = select_color(),
                       col = select_color(),
                       fontsize = grobi$gp$lwd
                     )
                   )
                 } else grobi

               } else if(grepl(grobi$name, pattern = "pointrange_glyph")) {

                 pointGrob <- grid::getGrob(grobi, "point")
                 line_grob <- grid::getGrob(grobi, "range")

                 # set glyph
                 if("glyph" %in% linkedStates) {

                   pointGrob_pch <- pch[i]
                   if(!is.numeric(pointGrob_pch)) pointGrob_pch <- pointGrob$pch
                   pointGrob$pch <- pointGrob_pch

                   grobi <- grid::setGrob(
                     gTree = grobi,
                     gPath = "point",
                     newGrob = pointGrob
                   )
                 } else pointGrob_pch <- pointGrob$pch

                 # set color
                 if("color" %in% linkedStates) {

                   if(pointGrob_pch %in% 21:24) pointGrob$gp$fill <- color[i] else pointGrob$gp$col <- color[i]

                   line_grob$gp$col <- color[i]

                   grobi <- grid::setGrob(
                     gTree = grid::setGrob(
                       gTree = grobi,
                       gPath = "point",
                       newGrob = pointGrob
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
                     newGrob = grid::editGrob(
                       grob = grid::getGrob(grobi, "point"),
                       gp = if(grobi$pch %in% 21:24) {
                         gpar(
                           fill = grobi$gp$fill,
                           fontsize = size[i],
                           col = grobi$gp$col
                         )
                       } else {
                         gpar(
                           col = grobi$gp$col,
                           fontsize = size[i]
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

                   pointGrob <- grid::getGrob(grobi, "point")
                   line_grob <- grid::getGrob(grobi, "range")

                   pointGrob$gp$col <- select_color()
                   line_grob$gp$col <- select_color()

                   grobi <- grid::setGrob(
                     gTree = grid::setGrob(
                       gTree = grobi,
                       gPath = "point",
                       newGrob = pointGrob
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
                   grobi_size <- size[i]
                 } else {
                   grobi_size <- grobi$gp$fontsize
                 }

                 grobi <- grid::editGrob(
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

                   grid::editGrob(
                     grob = grobi,
                     gp = gpar(
                       col = select_color(),
                       fontsize = size[i]
                     )
                   )
                 } else grobi

               } else if(grepl(grobi$name, pattern = "image_glyph")) {

                 # set color
                 if("color" %in% linkedStates) {
                   grobi <- grid::setGrob(
                     gTree = grobi,
                     gPath = "image_border",
                     newGrob = grid::editGrob(
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

                   imageBorderGrob <- grid::getGrob(grobi, "image_border")

                   imageBorderGrob <- grid::editGrob(
                     grob = imageBorderGrob,
                     width = get_unit(imageBorderGrob$width, unit = "mm", as.numeric = FALSE) +
                       unit(rounding$width * sqrt(size[i]/loon_default_size("adjust")), "cm"),
                     height = get_unit(imageBorderGrob$height, unit = "mm", as.numeric = FALSE) +
                       unit(rounding$height * sqrt(size[i]/loon_default_size("adjust")), "cm")
                   )

                   imageGrob <- grid::editGrob(
                     grob = grid::getGrob(grobi, "image"),
                     width = unit(rounding$width * sqrt(size[i]/loon_default_size("adjust")), "cm"),
                     height = unit(rounding$height * sqrt(size[i]/loon_default_size("adjust")), "cm")
                   )

                   grobi <- gTree(
                     children = gList(
                       imageBorderGrob,
                       imageGrob
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
                     newGrob = grid::editGrob(
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
