get_loonWidgets_info.l_graph <- function(widgets, loon_grobs, colorList, ...) {
  
  args <- list(...)
  navbarMenuName <- args$navbarMenuName
  
  loon_grob <- loon_grobs
  nodes_grob <- grid::getGrob(loon_grob, "graph nodes")
  nodes_children <- nodes_grob$children
  
  # points_layer_names <- sapply(points_layer, function(layer) layer$name)
  x <- y <- size <- pch <- index <- c()
  N <- length(nodes_children)
  
  viewPort <- get_viewPort(loon_grob)
  xlim <- viewPort[[2]]$xscale
  ylim <- viewPort[[2]]$yscale
  
  lapply(1:N,
         function(i){
           
           node_layer <- nodes_children[[i]]
           
           if(is(node_layer,  "null")) {
             
             x[i] <<- NA
             y[i] <<- NA
             pch[i] <<- ""
             size[i] <<- NA
           } else {
             
             x[i] <<- node_layer$x
             y[i] <<- node_layer$y
             pch[i] <<- node_layer$pch
             size[i] <<- node_layer$gp$cex
           }
           index[i] <<- as.numeric(gsub("\\D", "", node_layer$name))
         }
  )
  
  glyph <- pch_to_glyph(pch)
  xy_original <- list()
  newGrob <- grid::getGrob(loon_grob, "graph nodes")
  graph_edges <- grid::getGrob(loon_grob, "graph edges")
  
  lapply(1:length(newGrob$children),
         function(i) {
           
           xy_original[[i]] <<- list(
             x = newGrob$children[[i]]$x,
             y = newGrob$children[[i]]$y
           )
         }
  )
  
  # viewport
  swapAxes <- widgets['swapAxes']
  
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
  #   if(length(x) == 1) {
  #     plotView_xlim <- c(panX, panX + deltaX/zoomX)
  #     plotView_ylim <- c(panY, panY + deltaY/zoomY)
  #   } else {
  #     plotView_xlim <- grDevices::extendrange(x)
  #     plotView_ylim <- grDevices::extendrange(y)
  #   }
  # }
  
  plotView_xlim <- c(panX, panX + deltaX/zoomX)
  plotView_ylim <- c(panY, panY + deltaY/zoomY)
  
  worldView <-get_worldViewPort(loon_grob = loon_grob, parent = "graph", parentExcluded = TRUE)
  worldView_xlim <- range(c(plotView_xlim, worldView$worldView_xlim))
  worldView_ylim <- range(c(plotView_ylim, worldView$worldView_ylim))
  
  layers <- get_layers(loon_grob)
  names(layers) <- layers
  
  list(
    itemLabel = widgets['itemLabel'],
    showItemLabels = widgets['showItemLabels'],
    swap_in_shiny = swapAxes,
    swap_in_loon = swapAxes,
    showLabels = widgets['showLabels'],
    showScales = widgets['showScales'],
    showGuides = widgets['showGuides'],
    showOrbit = widgets['showOrbit'],
    linkingGroup = widgets['linkingGroup'],
    linkingKey = widgets['linkingKey'],
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
    color = loon::hex12tohex6(widgets['color']),
    active = widgets['active'],
    selected = widgets['selected'],
    selectByLoon = widgets['selectBy'],
    xlabel = xlabel,
    ylabel = ylabel,
    title = widgets['title'],
    linkingStates = loon::l_getLinkedStates(widgets),
    x = x,
    y = y,
    xlim = xlim,
    ylim = ylim,
    step_x = log_ceiling(xlim),
    step_y = log_ceiling(ylim),
    x_original = x,
    y_original = y,
    pch = pch, 
    glyph = glyph, 
    size = size,
    index = index,
    xy_original = xy_original,
    graph_edges = graph_edges,
    plotView_xlim = plotView_xlim,
    plotView_ylim = plotView_ylim,
    worldView_xlim = worldView_xlim,
    worldView_ylim = worldView_ylim,
    navbarMenuName = navbarMenuName,
    layers = layers,
    colorList = colorList,
    lastSelection = integer(0)
  )
}
