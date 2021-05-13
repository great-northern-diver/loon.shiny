get_loonWidgets_info.l_hist <- function(widgets, loon_grobs, colorList, ...) {
  
  args <- list(...)
  navbarMenuName <- args$navbarMenuName
  # viewport
  swapAxes <- widgets['swapAxes']
  
  panX <- widgets['panX']
  deltaX <- widgets['deltaX']
  zoomX <- widgets['zoomX']
  
  panY <- widgets['panY']
  deltaY <- widgets['deltaY']
  zoomY <- widgets['zoomY']
  
  loon_grob <- loon_grobs
  worldView <-get_worldViewPort(loon_grob = loon_grob, parent = "histogram", parentExcluded = TRUE)
  worldView_xlim <- worldView$worldView_xlim
  worldView_ylim <- worldView$worldView_ylim
  
  plotView_xlim <- c(panX, panX + deltaX/zoomX)
  plotView_ylim <- c(panY, panY + deltaY/zoomY)
  
  layers <- get_layers(loon_grob)
  names(layers) <- layers
  
  list(
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
      xlabel = widgets['xlabel'],
      ylabel = widgets['ylabel'],
      title = if(is.null(args$title)) widgets['title'] else args$title
    ),
    x = widgets['x'],
    active = widgets['active'],
    binwidth = widgets['binwidth'],
    color = loon::hex12tohex6(widgets['color']),
    colorFill = loon::hex12tohex6(widgets['colorFill']),
    colorOutline = loon::hex12tohex6(widgets['colorOutline']),
    selected = widgets['selected'],
    selectByLoon = widgets['selectBy'],
    origin = widgets['origin'],
    linkingStates = loon::l_getLinkedStates(widgets),
    swap_in_shiny = swapAxes,
    swap_in_loon = swapAxes,
    showLabels = widgets['showLabels'],
    showScales = widgets['showScales'],
    showGuides= widgets['showGuides'],
    showOutlines = widgets['showOutlines'],
    showStackedColors = widgets['showStackedColors'],
    xlabel = widgets['xlabel'],
    ylabel = widgets['ylabel'],
    title = widgets['title'],
    yshows = widgets['yshows'],
    visible = TRUE,
    appear = TRUE,
    plotView_xlim = plotView_xlim,
    plotView_ylim = plotView_ylim,
    worldView_xlim = worldView_xlim,
    worldView_ylim = worldView_ylim,
    navbarMenuName = navbarMenuName,
    layers = layers,
    colorList = colorList,
    lastSelection = integer(0),
    yshowsIsModified = FALSE,
    originIsModified = FALSE,
    binwidthIsModified = FALSE
  )
}
