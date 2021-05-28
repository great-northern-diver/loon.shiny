get_loonWidgetsInfo.l_hist <- function(widgets, loon.grobs, ...) {

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

  loon.grob <- loon.grobs

  # they are un-flipped
  xlim <- c(panX, panX + deltaX/zoomX)
  ylim <- c(panY, panY + deltaY/zoomY)

  plotViewXlim <- xlim
  plotViewYlim <- ylim

  # it is not the world view port
  # instead, they are the limits of x and y of all dependent layers (not include the model layer)
  # why so? that is because, for a histogram,
  # the plot view port varies if the yshows switched bewteen density and frequency
  worldView <-get_worldViewPort(loon.grob = loon.grob, parent = "histogram",
                                parentExcluded = TRUE)

  if(swapAxes) {
    layerYlim <- worldView$xlim
    layerXlim <- worldView$ylim
  } else {
    layerXlim <- worldView$xlim
    layerYlim <- worldView$ylim
  }

  layers <- get_layers(loon.grob)
  names(layers) <- layers

  list(
    linkingGroup = widgets['linkingGroup'],
    linkingKey = widgets['linkingKey'],
    loonDefaultMargins = list(
      minimumMargins = pixels_2_lines(widgets['minimumMargins']),
      labelMargins = pixels_2_lines(widgets['labelMargins']),
      scalesMargins = pixels_2_lines(widgets['scalesMargins'])
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
    swapInShiny = swapAxes,
    swapInLoon = swapAxes,
    showLabels = widgets['showLabels'],
    showScales = widgets['showScales'],
    showGuides= widgets['showGuides'],
    showOutlines = widgets['showOutlines'],
    showStackedColors = widgets['showStackedColors'],
    xlabel = widgets['xlabel'],
    ylabel = widgets['ylabel'],
    title = widgets['title'],
    yshows = widgets['yshows'],
    N = widgets['n'],
    visible = TRUE,
    appear = TRUE,
    xlim = xlim,
    ylim = ylim,
    plotViewXlim = plotViewXlim,
    plotViewYlim = plotViewYlim,
    layerXlim = layerXlim,
    layerYlim = layerYlim,
    navbarMenuName = navbarMenuName,
    layers = layers,
    lastSelection = integer(0),
    loonColor = list(
      background_color = loon::hex12tohex6(widgets["background"]),
      foreground_color = loon::hex12tohex6(widgets["foreground"]),
      guidesbackground_color = loon::hex12tohex6(widgets["guidesBackground"]),
      guideslines_color = loon::hex12tohex6(widgets["guidelines"])
    )
  )
}
