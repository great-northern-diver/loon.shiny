get_loonWidgetsInfo.l_plot <- function(widgets,
                                       loon.grobs,
                                       ...) {

  ############# **all coordinates are un-flipped** #############
  args <- list(...)
  navbarMenuName <- args$navbarMenuName

  loon.grobs <- adjust_loon_grobs(list(loon.grobs), loonWidgetsInfo = NULL)
  loon.grob <- loon.grobs[[1L]]

  scatterplotGrob <- grid::getGrob(loon.grob, "scatterplot")
  # only one child
  pointsTreeName <- scatterplotGrob$childrenOrder
  points_layer <- grid::getGrob(loon.grob, pointsTreeName)[["children"]]

  x <- y <- size <- pch <- index <- glyphNames <- c()
  glyphArgs <- xyOriginal <- list()

  N <- length(points_layer)
  # In loonGrob, selected points will be drawn on top. Hence, we need to reset it and maintain the original order
  displayOrder <- get_display_order(widgets)
  widgetsSize <- loon::as_grid_size(widgets["size"], "points")

  if(N > 0) {
    lapply(seq(N),
           function(i){

             point_layer <- points_layer[[i]]
             glyphName <- point_layer$name
             glyphNames[i] <<- glyphName

             if(is(point_layer,  "null")) {

               x[i] <<- NA
               y[i] <<- NA
               pch[i] <<- ""
               size[i] <<- NA
             } else {

               if(grepl(glyphName, pattern = "primitive_glyph")) {

                 x[i] <<- point_layer$x
                 y[i] <<- point_layer$y
                 pch[i] <<- point_layer$pch
                 size[i] <<- point_layer$gp$fontsize

                 xyOriginal[[i]] <<-
                   list(
                     x = point_layer$x,
                     y = point_layer$y
                   )
               } else if(grepl(glyphName, pattern = "image_glyph")) {

                 imageGrob <- grid::getGrob(point_layer, "image")
                 imageBorderGrob <- grid::getGrob(point_layer, "image_border")

                 x[i] <<- imageGrob$x
                 y[i] <<- imageGrob$y
                 pch[i] <<- "images"
                 size[i] <<- widgetsSize[i]

                 glyphArgs[[i]] <<- setNames(
                   list(
                     list(
                       width =  as.numeric(imageGrob$width),
                       height = as.numeric(imageGrob$height),
                       raster = imageGrob$raster
                     )
                   ),
                   glyphName
                 )

                 xyOriginal[[i]] <<-
                   list(
                     x = imageGrob$x,
                     y = imageGrob$y,
                     x_border = imageBorderGrob$x,
                     y_border = imageBorderGrob$y
                   )

               } else if(grepl(glyphName, pattern = "text_glyph")) {

                 x[i] <<- point_layer$x
                 y[i] <<- point_layer$y
                 size[i] <<- point_layer$gp$fontsize
                 pch[i] <<- "texts"

                 glyphArgs[[i]] <<- setNames(
                   list(
                     list(
                       text = point_layer$label
                     )
                   ),
                   glyphName
                 )

                 xyOriginal[[i]] <<-
                   list(
                     y = point_layer$y,
                     x = point_layer$x
                   )

               } else if(grepl(glyphName, pattern = "polygon_glyph")) {

                 x[i] <<- get_unit(point_layer$x, as.numeric = TRUE)
                 y[i] <<-get_unit(point_layer$y, as.numeric = TRUE)
                 size[i] <<- widgetsSize[i]
                 pch[i] <<- "polygon"

                 glyphArgs[[i]] <<- setNames(
                   list(
                     list(
                       x = get_unit(point_layer$x, is.unit = FALSE, as.numeric = TRUE),
                       y = get_unit(point_layer$y, is.unit = FALSE, as.numeric = TRUE)
                     )
                   ),
                   glyphName
                 )

                 xyOriginal[[i]] <<-
                   list(
                     x = point_layer$x,
                     y = point_layer$y
                   )

               } else if(grepl(glyphName, pattern = "pointrange_glyph")) {

                 pointGrob <- grid::getGrob(point_layer, "point")
                 range_grob <- grid::getGrob(point_layer, "range")

                 x[i] <<- pointGrob$x
                 y[i] <<- pointGrob$y
                 size[i] <<- pointGrob$gp$fontsize
                 pch[i] <<- pointGrob$pch

                 glyphArgs[[i]] <<- setNames(
                   list(
                     list(
                       x_range = range_grob$x,
                       y_range = range_grob$y
                     )
                   ),
                   glyphName
                 )

                 xyOriginal[[i]] <<-
                   list(
                     x = point_layer$x,
                     y = point_layer$y
                   )

               } else if(grepl(glyphName, pattern = "serialaxes_glyph")) {

                 boundaryGrob <- grid::getGrob(point_layer, "boundary")
                 if(is.null(boundaryGrob)) {
                   boundaryGrob <- grid::getGrob(point_layer, "boundary: polylineGrob arguments")
                 }

                 boundaryGrobRounding <- list(
                   x = get_unit(boundaryGrob$x, unit = "native", is.unit = FALSE, as.numeric = TRUE),
                   y = get_unit(boundaryGrob$y, unit = "native", is.unit = FALSE, as.numeric = TRUE)
                 )

                 # axes serialaxes
                 axesGrob <- grid::getGrob(point_layer, "axes")
                 if(is.null(axesGrob)) {
                   axesGrob <- grid::getGrob(point_layer, "axes: polylineGrob arguments")
                 }

                 axesGrobRounding <-  list(
                   x = get_unit(axesGrob$x, unit = "native",is.unit = FALSE, as.numeric = TRUE),
                   y = get_unit(axesGrob$y, unit = "native",is.unit = FALSE, as.numeric = TRUE)
                 )

                 serialaxesGrob <- grid::getGrob(point_layer, "polyline")
                 if(is.null(serialaxesGrob)) {
                   serialaxesGrob <- grid::getGrob(point_layer, "polyline: showArea")
                 }

                 serialaxesGrobRounding <- list(
                   x = get_unit(serialaxesGrob$x,
                                unit = "native",
                                is.unit = FALSE,
                                as.numeric = TRUE),
                   y = get_unit(serialaxesGrob$y,
                                unit = "native",
                                is.unit = FALSE,
                                as.numeric = TRUE)
                 )
                 # numerical value
                 glyphArgs[[i]] <<- setNames(
                   list(
                     list(
                       boundaryGrobRounding = boundaryGrobRounding,
                       axesGrobRounding =  axesGrobRounding,
                       serialaxesGrobRounding = serialaxesGrobRounding
                     )
                   ),
                   glyphName
                 )

                 # unit value
                 xyOriginal[[i]] <<-
                   list(
                     x = serialaxesGrob$x,
                     y = serialaxesGrob$y
                   )

                 x[i] <<- get_unit(serialaxesGrob$x, as.numeric = TRUE)
                 y[i] <<- get_unit(serialaxesGrob$y, as.numeric = TRUE)
                 size[i] <<- widgetsSize[i]
                 pch[i] <<- ifelse(grepl(glyphName, pattern = "radial"),
                                   "radial",
                                   "parallel")

               } else stop("this glyph is not implemented")
             }
             index[i] <<- as.numeric(gsub("\\D", "", glyphName))
           }
    )

    x <- x[displayOrder]
    y <- y[displayOrder]
    size <- size[displayOrder]
    pch <- pch[displayOrder]
    index <- index[displayOrder]
    glyphNames <- glyphNames[displayOrder]

    glyphArgs <- if(length(glyphArgs) > 0) glyphArgs[displayOrder]
    xyOriginal <- xyOriginal[displayOrder]

  } else {

    x <- NA
    y <- NA
    size <- NA
    pch <- NA
    index <- NA
    glyphNames <- NA
  }
  # glyphs arguments
  glyph <- pch_to_glyph(pch)

  nonePrimitiveGlyphSettings <- if(!is.null(glyphNames)) {

    if(all(!is.na(glyphNames))) {
      if(any(grepl(glyphNames, pattern = "serialaxes"))) {
        # pick the first one (They share the same enclosing and axes)
        which_is_serialaxes <- which(grepl(glyphNames, pattern = "serialaxes"))[1]
        point_layer <- points_layer[[which_is_serialaxes]]

        # showArea or not
        polylineChildren <- point_layer$children[["polyline"]]
        showArea <- if(is.null(polylineChildren)) TRUE else FALSE

        # showEnclosing or not
        boundaryChildren <- point_layer$children[["boundary"]]
        showEnclosing  <- if(is.null(boundaryChildren)) FALSE else TRUE

        # showAxes or not
        axesChildren <- point_layer$children[["axes"]]
        showAxes  <- if(is.null(axesChildren)) FALSE else TRUE

        list(
          showArea  =  showArea,
          showEnclosing = showEnclosing,
          showAxes = showAxes
        )
      } else if(any(grepl(glyphNames, pattern = "pointrange"))){
        # pick the first one (They share the same enclosing and axes)
        which_is_pointrange <- which(grepl(glyphNames, pattern = "pointrange"))[1]

        # showArea of not
        showArea <- if(pch[which_is_pointrange] == 21) TRUE else FALSE
        list(
          showArea  =  showArea
        )
      } else if(any(grepl(glyphNames, pattern = "polygon"))){
        # pick the first one (They share the same enclosing and axes)
        which_is_polygon <- which(grepl(glyphNames, pattern = "polygon"))[1L]
        point_layer <- points_layer[[which_is_polygon]]

        # showArea of not
        showArea <- grepl(point_layer$name, pattern = "showArea")

        list(
          showArea  =  showArea
        )
      } else NA
    } else NA
  } else NA

  # xlim and ylim is swapped
  viewPort <- get_viewPort(loon.grob)
  # dataViewport <- viewPort["dataViewport"]
  dataViewport <- get_vp_from_vpStack(viewPort, "dataViewport")

  xlim <- dataViewport$xscale
  ylim <- dataViewport$yscale

  swapAxes <- widgets['swapAxes']
  if(swapAxes) {

    ylabel <- widgets['xlabel']
    xlabel <- widgets['ylabel']
  } else {

    xlabel <- widgets['xlabel']
    ylabel <- widgets['ylabel']
  }


  # x and y are swapped
  xNoNA <- na.omit(x)
  plotViewXlim <- if(length(xNoNA) == 0) {
    c(-0.05, 0.05)
  } else if(length(xNoNA) == 1) {
    xNoNA + c(-0.05, 0.05)
  } else {
    grDevices::extendrange(xNoNA)
  }

  yNoNA <- na.omit(y)
  plotViewYlim <- if(length(yNoNA) == 0) {
    c(-0.05, 0.05)
  } else if(length(yNoNA) == 1) {
    yNoNA + c(-0.05, 0.05)
  } else {
    grDevices::extendrange(yNoNA)
  }

  # the world view is swapped as well
  worldView <- get_worldViewPort(loon.grob = loon.grob, parent = "scatterplot",
                                 parentExcluded = TRUE)
  worldViewXlim <- range(c(plotViewXlim, worldView$xlim,
                           if(swapAxes) ylim else xlim))
  worldViewYlim <- range(c(plotViewYlim, worldView$ylim,
                           if(swapAxes) xlim else ylim))

  layers <- get_layers(loon.grob)
  names(layers) <- layers

  list(
    itemLabel = widgets['itemLabel'],
    swapInShiny = swapAxes,
    swapInLoon = swapAxes,
    showItemLabels = widgets['showItemLabels'],
    showLabels = widgets['showLabels'],
    showScales = widgets['showScales'],
    showGuides = widgets['showGuides'],
    linkingGroup = widgets['linkingGroup'],
    linkingKey = widgets['linkingKey'],
    color = loon::hex12tohex6(widgets['color']),
    active = widgets['active'],
    selected = widgets['selected'],
    selectByLoon = widgets['selectBy'],
    N = N,
    loonDefaultMargins = list(
      minimumMargins = pixels_2_lines(widgets['minimumMargins']),
      labelMargins = pixels_2_lines(widgets['labelMargins']),
      scalesMargins = pixels_2_lines(widgets['scalesMargins'])
    ),
    labels = list(
      xlabel = xlabel,
      ylabel = ylabel,
      title = if(is.null(args$title)) widgets['title'] else args$title
    ),
    x = x,
    y = y,
    linkingStates = loon::l_getLinkedStates(widgets),
    size = size,
    oldSize = size, # record the original size, never update in the server
    index = index,
    glyphNames = glyphNames,
    pch = pch,
    glyph = glyph,
    pointsTreeName =  pointsTreeName,
    nonePrimitiveGlyphSettings = nonePrimitiveGlyphSettings,
    xlim = xlim,
    ylim = ylim,
    stepX = log_ceiling(diff(xlim)),
    stepY = log_ceiling(diff(ylim)),
    glyphArgs = glyphArgs,
    xyOriginal = xyOriginal,
    xOriginal = x,
    yOriginal = y,
    plotViewXlim = plotViewXlim,
    plotViewYlim = plotViewYlim,
    worldViewXlim = worldViewXlim,
    worldViewYlim = worldViewYlim,
    displayOrder = displayOrder,
    navbarMenuName = navbarMenuName,
    layers = layers,
    glyph_id = loon::l_glyph_ids(widgets),
    sticky = "off",
    lastSelection = integer(0),
    loonColor = list(
      background_color = loon::hex12tohex6(widgets["background"]),
      foreground_color = loon::hex12tohex6(widgets["foreground"]),
      guidesbackground_color = loon::hex12tohex6(widgets["guidesBackground"]),
      guideslines_color = loon::hex12tohex6(widgets["guidelines"])
    ),
    alpha = rep(1, N)
  )
}
