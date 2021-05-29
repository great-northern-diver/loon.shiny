get_loonWidgetsInfo.l_graph <- function(widgets, loon.grobs, ...) {

  args <- list(...)
  navbarMenuName <- args$navbarMenuName

  loon.grob <- loon.grobs
  nodes_grob <- grid::getGrob(loon.grob, "graph nodes")
  nodes_children <- nodes_grob$children

  # points_layer_names <- sapply(points_layer, function(layer) layer$name)
  x <- y <- size <- pch <- index <- c()
  N <- length(nodes_children)

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
  xyOriginal <- list()
  newGrob <- grid::getGrob(loon.grob, "graph nodes")
  graph_edges <- grid::getGrob(loon.grob, "graph edges")

  lapply(1:length(newGrob$children),
         function(i) {

           xyOriginal[[i]] <<- list(
             x = newGrob$children[[i]]$x,
             y = newGrob$children[[i]]$y
           )
         }
  )

  # viewport
  swapAxes <- widgets['swapAxes']

  # xlim and ylim is swapped
  viewPort <- get_viewPort(loon.grob)
  xlim <- viewPort[[2]]$xscale
  ylim <- viewPort[[2]]$yscale
  if(swapAxes) {

    ylabel <- widgets['xlabel']
    xlabel <- widgets['ylabel']
  } else {

    xlabel <- widgets['xlabel']
    ylabel <- widgets['ylabel']
  }


  # x and y are swapped, so does xlim and ylim
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
  worldView <- get_worldViewPort(loon.grob = loon.grob,
                                 parentExcluded = TRUE,
                                 recursive = TRUE)
  worldViewXlim <- range(c(plotViewXlim, worldView$xlim))
  worldViewYlim <- range(c(plotViewYlim, worldView$ylim))

  layers <- get_layers(loon.grob, recursive = FALSE)
  names(layers) <- layers

  list(
    itemLabel = widgets['itemLabel'],
    showItemLabels = widgets['showItemLabels'],
    swapInShiny = swapAxes,
    swapInLoon = swapAxes,
    showLabels = widgets['showLabels'],
    showScales = widgets['showScales'],
    showGuides = widgets['showGuides'],
    showOrbit = widgets['showOrbit'],
    linkingGroup = widgets['linkingGroup'],
    linkingKey = widgets['linkingKey'],
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
    color = loon::hex12tohex6(widgets['color']),
    active = widgets['active'],
    selected = widgets['selected'],
    selectByLoon = widgets['selectBy'],
    xlabel = xlabel,
    ylabel = ylabel,
    title = widgets['title'],
    N = N,
    linkingStates = loon::l_getLinkedStates(widgets),
    x = x,
    y = y,
    xlim = xlim,
    ylim = ylim,
    stepX = log_ceiling(diff(xlim)),
    stepY = log_ceiling(diff(ylim)),
    xOriginal = x,
    yOriginal = y,
    pch = pch,
    glyph = glyph,
    size = size,
    index = index,
    xyOriginal = xyOriginal,
    graph_edges = graph_edges,
    plotViewXlim = plotViewXlim,
    plotViewYlim = plotViewYlim,
    worldViewXlim = worldViewXlim,
    worldViewYlim = worldViewYlim,
    navbarMenuName = navbarMenuName,
    layers = layers,
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
