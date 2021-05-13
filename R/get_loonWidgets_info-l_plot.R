get_loonWidgets_info.l_plot <- function(widgets, 
                                        loon_grobs,
                                        colorList, 
                                        ...) {
  
  args <- list(...)
  navbarMenuName <- args$navbarMenuName
  
  loon_grobs <- adjust_loon_grobs(list(loon_grobs), loonWidgets_info = NULL)
  loon_grob <- loon_grobs[[1L]]
  
  scatterplot_grob <- grid::getGrob(loon_grob, "scatterplot")
  # only one child
  pointsTree_name <- scatterplot_grob$childrenOrder
  points_layer <- grid::getGrob(loon_grob, pointsTree_name)[["children"]]
  
  x <- y <- size <- pch <- index <- glyph_name <- c()
  glyph_args <- xy_original <- list()
  
  N <- length(points_layer)
  # In loonGrob, selected points will be drawn on top. Hence, we need to reset it and maintain the original order
  display_order <- get_display_order(widgets)
  
  if(N > 0) {
    lapply(1:N,
           function(i){
             
             point_layer <- points_layer[[i]]
             glyphName <- point_layer$name
             glyph_name[i] <<- glyphName
             
             if(is(point_layer,  "null")) {
               
               x[i] <<- NA
               y[i] <<- NA
               pch[i] <<- ""
               size[i] <<- NA
             } else {
               
               if(stringr::str_detect(glyphName, "primitive_glyph")) {
                 
                 x[i] <<- point_layer$x
                 y[i] <<- point_layer$y
                 pch[i] <<- point_layer$pch
                 size[i] <<- point_layer$gp$cex
                 
                 xy_original[[i]] <<- list(
                   x = point_layer$x,
                   y = point_layer$y
                 )
               } else if(stringr::str_detect(glyphName, "image_glyph")) {
                 
                 image_grob <- grid::getGrob(point_layer, "image")
                 image_border_grob <- grid::getGrob(point_layer, "image_border")
                 
                 x[i] <<- image_grob$x
                 y[i] <<- image_grob$y
                 pch[i] <<- "image"
                 size[i] <<- default_size()
                 
                 glyph_args[[i]] <<- setNames(
                   list(
                     list(
                       width =  as.numeric(image_grob$width),
                       height = as.numeric(image_grob$height),
                       raster = image_grob$raster
                     )
                   ),
                   glyphName
                 )
                 
                 xy_original[[i]] <<- list(
                   x = image_grob$x,
                   y = image_grob$y,
                   x_border = image_border_grob$x,
                   y_border = image_border_grob$y
                 )
                 
               } else if(stringr::str_detect(glyphName, "text_glyph")) {
                 
                 x[i] <<- point_layer$x
                 y[i] <<- point_layer$y
                 size[i] <<- point_layer$gp$fontsize / loon_default_size()[["adjusted_size"]]
                 pch[i] <<- "text"
                 
                 glyph_args[[i]] <<- setNames(
                   list(
                     list(
                       text = point_layer$label
                     )
                   ),
                   glyphName
                 )
                 
                 xy_original[[i]] <<- list(
                   x = point_layer$x,
                   y = point_layer$y
                 )
                 
               } else if(stringr::str_detect(glyphName, "polygon_glyph")) {
                 
                 x[i] <<- get_unit(point_layer$x, as.numeric = TRUE) 
                 y[i] <<-get_unit(point_layer$y, as.numeric = TRUE) 
                 size[i] <<- default_size()
                 pch[i] <<- "polygon"
                 
                 glyph_args[[i]] <<- setNames(
                   list(
                     list(
                       x = get_unit(point_layer$x, is.unit = FALSE, as.numeric = TRUE),
                       y = get_unit(point_layer$y, is.unit = FALSE, as.numeric = TRUE)
                     )
                   ),
                   glyphName
                 )
                 
                 xy_original[[i]] <<- list(
                   x = point_layer$x,
                   y = point_layer$y
                 )
                 
               } else if(stringr::str_detect(glyphName, "pointrange_glyph")) {
                 
                 point_grob <- grid::getGrob(point_layer, "point")
                 range_grob <- grid::getGrob(point_layer, "range")
                 
                 x[i] <<- point_grob$x
                 y[i] <<- point_grob$y
                 size[i] <<- point_grob$gp$cex
                 pch[i] <<- point_grob$pch
                 
                 glyph_args[[i]] <<- setNames(
                   list(
                     list(
                       x_range = range_grob$x,
                       y_range = range_grob$y
                     )
                   ),
                   glyphName
                 )
                 
                 xy_original[[i]] <<- list(
                   x = point_grob$x,
                   y = point_grob$y
                 )
                 
               } else if(stringr::str_detect(glyphName, "serialaxes_glyph")) {
              
                 boundary_grob <- grid::getGrob(point_layer, "boundary")
                 if(is.null(boundary_grob)) {
                   boundary_grob <- grid::getGrob(point_layer, "boundary: polylineGrob arguments")
                 }
                 
                 boundary_grob_rounding <- list(
                   x = get_unit(boundary_grob$x, unit = "native", is.unit = FALSE, as.numeric = TRUE),
                   y = get_unit(boundary_grob$y, unit = "native", is.unit = FALSE, as.numeric = TRUE)
                 )
                 
                 # axes serialaxes
                 axes_grob <- grid::getGrob(point_layer, "axes")
                 if(is.null(axes_grob)) {
                   axes_grob <- grid::getGrob(point_layer, "axes: polylineGrob arguments")
                 }
                 
                 axes_grob_rounding <-  list(
                   x = get_unit(axes_grob$x, unit = "native",is.unit = FALSE, as.numeric = TRUE),
                   y = get_unit(axes_grob$y, unit = "native",is.unit = FALSE, as.numeric = TRUE)
                 )
                 
                 serialaxes_grob <- grid::getGrob(point_layer, "polyline")
                 if(is.null(serialaxes_grob)) {
                   serialaxes_grob <- grid::getGrob(point_layer, "polyline: showArea")
                 }
                 
                 serialaxes_grob_rounding <- list(
                   x = get_unit(serialaxes_grob$x, 
                                unit = "native",
                                is.unit = FALSE, 
                                as.numeric = TRUE),
                   y = get_unit(serialaxes_grob$y, 
                               unit = "native",
                               is.unit = FALSE, 
                               as.numeric = TRUE)
                 )
                 # numerical value
                 glyph_args[[i]] <<- setNames(
                   list(
                     list(
                       boundary_grob_rounding = boundary_grob_rounding,
                       axes_grob_rounding =  axes_grob_rounding,
                       serialaxes_grob_rounding = serialaxes_grob_rounding
                     )
                   ),
                   glyphName
                 )
                
                 # unit value
                 xy_original[[i]] <<- list(
                   x = serialaxes_grob$x,
                   y = serialaxes_grob$y
                 )

                 x[i] <<- get_unit(serialaxes_grob$x, as.numeric = TRUE)
                 y[i] <<- get_unit(serialaxes_grob$y, as.numeric = TRUE)
                 size[i] <<- default_size()
                 pch[i] <<- ifelse(stringr::str_detect(glyphName, "radial"),
                                   "serialaxes radial",
                                   "serialaxes parallel")
                 
               } else stop("this glyph is not implemented")
             }
             index[i] <<- as.numeric(gsub("\\D", "", glyphName))
           }
    )
    
    x <- x[display_order]
    y <- y[display_order]
    size <- size[display_order]
    pch <- pch[display_order]
    index <- index[display_order]
    glyph_name <- glyph_name[display_order]
    
    glyph_args <- if(length(glyph_args) > 0) glyph_args[display_order]
    xy_original <- xy_original[display_order]
    
  } else {
    
    x <- NA
    y <- NA
    size <- NA
    pch <- NA
    index <- NA
    glyph_name <- NA
  }
  # glyphs arguments
  glyph <- pch_to_glyph(pch)
  
  glyph_setting <- if(!is.null(glyph_name)) {
    
    if(all(!is.na(glyph_name))) {
      if(any(stringr::str_detect(glyph_name, "serialaxes"))) {
        # pick the first one (They share the same enclosing and axes)
        which_is_serialaxes <- which(stringr::str_detect(glyph_name, "serialaxes"))[1]
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
      } else if(any(stringr::str_detect(glyph_name, "pointrange"))){
        # pick the first one (They share the same enclosing and axes)
        which_is_pointrange <- which(stringr::str_detect(glyph_name, "pointrange"))[1]
        
        # showArea of not
        showArea <- if(pch[which_is_pointrange] == 21) TRUE else FALSE
        list(
          showArea  =  showArea
        )
      } else NA
    } else NA
  } else NA
  
  # viewport
  swapAxes <- widgets['swapAxes']

  viewPort <- get_viewPort(loon_grob)
  xlim <- viewPort[[2]]$xscale
  ylim <- viewPort[[2]]$yscale
  
  panX <- widgets['panX']
  deltaX <- widgets['deltaX']
  zoomX <- widgets['zoomX']
  
  panY <- widgets['panY']
  deltaY <- widgets['deltaY']
  zoomY <- widgets['zoomY']
  
  if(swapAxes) {
    
    ylabel <- widgets['xlabel']
    xlabel <- widgets['ylabel'] 
  } else {
    
    xlabel <- widgets['xlabel']
    ylabel <- widgets['ylabel']
  }
  
  # if(any(is.na(x))) {
  #   plotView_xlim <- c(panX, panX + deltaX/zoomX)
  #   plotView_ylim <- c(panY, panY + deltaY/zoomY)
  # } else {
  #   
  #   if(length(x) <= 1) {
  #     plotView_xlim <- c(panX, panX + deltaX/zoomX)
  #     plotView_ylim <- c(panY, panY + deltaY/zoomY)
  #   } else {
  #     plotView_xlim <- grDevices::extendrange(x)
  #     plotView_ylim <- grDevices::extendrange(y)
  #   }
  # }
  
  plotView_xlim <- c(panX, panX + deltaX/zoomX)
  plotView_ylim <- c(panY, panY + deltaY/zoomY)
  
  worldView <- get_worldViewPort(loon_grob = loon_grob, parent = "scatterplot", parentExcluded = TRUE)
  worldView_xlim <- range(c(plotView_xlim, worldView$worldView_xlim))
  worldView_ylim <- range(c(plotView_ylim, worldView$worldView_ylim))
  
  layers <- get_layers(loon_grob)
  names(layers) <- layers
  
  list(
    itemLabel = widgets['itemLabel'],
    swap_in_shiny = swapAxes,
    swap_in_loon = swapAxes,
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
    loon_default_margins = list(
      minimumMargins = pixels_2_lines(widgets['minimumMargins']),
      labelMargins = pixels_2_lines(widgets['labelMargins']),
      scalesMargins = pixels_2_lines(widgets['scalesMargins'])
    ),
    loon_color = list(
      background_color = loon::hex12tohex6(widgets['background']),
      foreground_color = loon::hex12tohex6(widgets['foreground']),
      guidesbackground_color  = loon::hex12tohex6(widgets['guidesBackground']),
      guideslines_color = loon::hex12tohex6(widgets['guidelines']),
      select_color = loon::l_getOption("select-color")
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
    index = index,
    glyph_name = glyph_name,
    pch = pch,
    glyph = glyph,
    pointsTree_name =  pointsTree_name,
    glyph_setting = glyph_setting,
    xlim = xlim,
    ylim = ylim,
    step_x = log_ceiling(xlim),
    step_y = log_ceiling(ylim),
    glyph_args = glyph_args,
    xy_original = xy_original,
    x_original = x,
    y_original = y,
    plotView_xlim = plotView_xlim,
    plotView_ylim = plotView_ylim,
    worldView_xlim = worldView_xlim,
    worldView_ylim = worldView_ylim,
    display_order = display_order,
    navbarMenuName = navbarMenuName,
    layers = layers,
    glyph_id = loon::l_glyph_ids(widgets),
    sticky = "off",
    colorList = colorList,
    lastSelection = integer(0)
  )
}
