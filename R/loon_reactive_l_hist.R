# the way to build loon_reactive.l_hist is very different from others. The reason is that loonGrob.l_hist is
# a couple of rect grobs; it would be hard to determine the linking index. Hence, we would use the widgets information
# to rebuild l_hist grob.
loon_reactive.l_hist <- function(loon.grob, output.grob, linkingInfo, buttons, position, selectBy,
                                 linkingGroup, input, colorList, tabPanelName, outputInfo) {

  input$plotBrush
  input$plotClick

  loonWidgetsInfo <- outputInfo$loonWidgetsInfo
  pull <- input[[paste0(tabPanelName, "pull")]]

  if(!is.null(output.grob) && (input[["navBarPage"]] != tabPanelName || pull > buttons["pull"])) {

    if(pull > buttons["pull"]) {
      buttons["pull"] <- pull
      linkingGroup <- isolate(input[[paste0(tabPanelName, "linkingGroup")]])
    }

    if(linkingGroup != "none") {

      linkedInfo <- linkingInfo[[linkingGroup]]
      order <- match(loonWidgetsInfo$linkingKey, linkedInfo$linkingKey)

      # set_linkingInfo is slightly different, it returns loonWidget_info and output.grob (instead of loon.grob)
      modifiedLinkingInfo <- set_linkingInfo(
        loon.grob = loon.grob,
        output.grob = output.grob,
        linkedInfo = linkedInfo,
        linkedStates = input[[paste0(tabPanelName, "linkedStates")]],
        tabPanelName = tabPanelName,
        order = order,
        loonWidgetsInfo = loonWidgetsInfo
      )

      selected <- linkedInfo$selected
      brushId <- which(selected)
      selectByColor <- linkedInfo$selectByColor
      loonWidgetsInfo <- modifiedLinkingInfo$loonWidgetsInfo
      output.grob <- modifiedLinkingInfo$output.grob

    } else {

      brushId <- outputInfo$brushId
      selectByColor <- outputInfo$selectByColor
    }
  } else {

    isFirstDraw <- is.null(output.grob)
    output.grob <- loon.grob

    loonColor <- loonWidgetsInfo$loonColor
    # interactive ------------------------------------------------------
    plotAxes1 <- input[[paste0(tabPanelName, "plotAxes1")]]
    plotAxes2 <- input[[paste0(tabPanelName, "plotAxes2")]]
    plotShow <- input[[paste0(tabPanelName, "plotShow")]]

    swapInShiny <- "swap" %in% plotAxes1
    swapInLoon <- loonWidgetsInfo$swapInLoon
    loonWidgetsInfo$swapInShiny <- swapInShiny

    yshows <- input[[paste0(tabPanelName, "yshows")]]
    loonWidgetsInfo$yshows <- yshows

    rangeChangedBydensity <- FALSE
    origin <- input[[paste0(tabPanelName, "origin")]] - 1e-8
    loonWidgetsInfo$origin <- origin
    if(abs(buttons["origin"] - origin) > 1e-6) {
      if(yshows == "density") rangeChangedBydensity <- TRUE
      buttons["origin"] <- origin
    }

    binwidth <- input[[paste0(tabPanelName, "binwidth")]]
    loonWidgetsInfo$binwidth <- binwidth
    if(abs(buttons["binwidth"] - binwidth) > 1e-6) {
      if(yshows == "density") rangeChangedBydensity <- TRUE
      buttons["binwidth"] <- binwidth
    }

    showStackedColors <- "stackedColors" %in% plotShow
    loonWidgetsInfo$showStackedColors <- showStackedColors

    showOutlines <- "outlines" %in% plotShow
    loonWidgetsInfo$showOutlines <- showOutlines

    colorFill <- loonWidgetsInfo$colorFill # showStackedColors is FALSE (thistle)
    colorOutline <- loonWidgetsInfo$colorOutline
    color <- loonWidgetsInfo$color
    N <- loonWidgetsInfo$N

    # set active
    modifyReactive <- input[[paste0(tabPanelName, "modifyReactive")]]

    if (modifyReactive > buttons["reactive"]) {

      buttons["reactive"] <- modifyReactive

      loonWidgetsInfo$active <- rep(TRUE, N)
    }
    active <- loonWidgetsInfo$active

    binInfo <- get_binInfo(data = loonWidgetsInfo$x,
                           origin = origin, active = active,
                           binwidth = binwidth, yshows = yshows)
    binId <- binInfo$binId
    binX <- binInfo$binX
    binHeight <- binInfo$binHeight

    binxy <- get_binxy(binX = binX, binId = binId, binwidth = binwidth,
                       yshows = yshows, color = color, n = N)

    # ++++++++++++++++++++++++++++++++ set guides labels axis and scales ++++++++++++++++++++++++++++++++++++++++++++
    # build Cartesian coordinates
    scaleToPlot <- input[[paste0(tabPanelName, "scaleToPlot")]]
    scaleToWorld <- input[[paste0(tabPanelName, "scaleToWorld")]]
    scaleToLayer <- input[[paste0(tabPanelName, "scaleToLayer")]]

    sliderxlim <- input[[paste0(tabPanelName, "xlim")]]
    sliderylim <- input[[paste0(tabPanelName, "ylim")]]


    plotViewXlim <- grDevices::extendrange(c(binxy$xmin, binxy$xmax))
    plotViewYlim <- grDevices::extendrange(c(binxy$ymin, binxy$ymax))

    loonWidgetsInfo$plotViewXlim <- plotViewXlim
    loonWidgetsInfo$plotViewYlim <- plotViewYlim

    # define current layer
    currentLayerName <- input[[paste0(tabPanelName, "layer")]]
    newLayerLabel <- isolate(input[[paste0(tabPanelName, "newLayerLabel")]])
    layerSet <- input[[paste0(tabPanelName, "layerSet")]]

    if(layerSet > buttons["layerSet"]) {

      buttons["layerSet"] <- layerSet

      if(newLayerLabel == "") {

        message("no valid label")
        layers <- loonWidgetsInfo$layers
        layersName <- names(layers)

        currentLayer <- layers[which(layersName == currentLayerName)]

      } else {
        layers <- loonWidgetsInfo$layers
        layersName <- names(layers)

        whichLayerIsEdited <- which(layersName == currentLayerName)

        layersName[whichLayerIsEdited] <- newLayerLabel
        names(layers) <- layersName
        loonWidgetsInfo$layers <- layers

        currentLayer <- layers[whichLayerIsEdited]
      }
    } else {

      layers <- loonWidgetsInfo$layers
      layersName <- names(layers)

      currentLayer <- layers[which(layersName == currentLayerName)]
    }

    layerMinus <- input[[paste0(tabPanelName, "layerMinus")]]
    if(layerMinus > buttons["layerMinus"]) {

      buttons["layerMinus"] <- layerMinus

      loon.grob <- grid::setGrob(
        gTree = loon.grob,
        gPath = currentLayer,
        newGrob = nullGrob(name = currentLayer)
      )

      output.grob <- grid::setGrob(
        gTree = loon.grob,
        gPath = currentLayer,
        newGrob = nullGrob(name = currentLayer)
      )

      worldView <-get_worldViewPort(loon.grob = loon.grob, parent = "histogram",
                                    parentExcluded = TRUE)

      if(swapInLoon) {
        layerYlim <- worldView$xlim
        layerXlim <- worldView$ylim
      } else {
        layerXlim <- worldView$xlim
        layerYlim <- worldView$ylim
      }

      loonWidgetsInfo$layerXlim <- layerXlim
      loonWidgetsInfo$layerYlim <- layerYlim

    }

    worldViewXlim <- range(c(plotViewXlim, loonWidgetsInfo$layerXlim))
    worldViewYlim <- range(c(plotViewYlim, loonWidgetsInfo$layerYlim))

    # swap layers
    if(swapInLoon != swapInShiny) {
      output.grob <- swap_layer_grob(output.grob, parent = "histogram")
    }

    if(swapInShiny) {

      xlabel <- loonWidgetsInfo$ylabel
      ylabel <- loonWidgetsInfo$xlabel

      if(scaleToPlot > buttons["plot"] || rangeChangedBydensity) {

        if(scaleToPlot > buttons["plot"])
          buttons["plot"] <- scaleToPlot

        ylim <- plotViewXlim
        xlim <- plotViewYlim

      } else if(scaleToWorld > buttons["world"]) {

        buttons["world"] <- scaleToWorld

        ylim <- worldViewXlim
        xlim <- worldViewYlim

      } else if(scaleToLayer > buttons["scaleToLayer"]  && length(currentLayer) > 0) {

        buttons["scaleToLayer"] <- scaleToLayer

        if(currentLayer == "histogram") {

          xlim <- loonWidgetsInfo$plotViewYlim
          ylim <- loonWidgetsInfo$plotViewXlim
        } else {

          layerLimits <- get_layer_worldView(loon.grob, layer = currentLayer)

          xlim <- layerLimits$ylim
          ylim <- layerLimits$xlim
        }

      } else {

        ylim <- sliderxlim
        xlim <- sliderylim
      }

    } else {

      xlabel <- loonWidgetsInfo$xlabel
      ylabel <- loonWidgetsInfo$ylabel

      if(scaleToPlot > buttons["plot"] || rangeChangedBydensity) {

        if(scaleToPlot > buttons["plot"])
          buttons["plot"] <- scaleToPlot

        xlim <- plotViewXlim
        ylim <- plotViewYlim

      } else if(scaleToWorld > buttons["world"]) {

        buttons["world"] <- scaleToWorld

        xlim <- worldViewXlim
        ylim <- worldViewYlim

      } else if (scaleToLayer > buttons["scaleToLayer"]  && length(currentLayer) > 0) {

        buttons["scaleToLayer"] <- scaleToLayer

        if(currentLayer == "histogram") {

          xlim <- loonWidgetsInfo$plotViewXlim
          ylim <- loonWidgetsInfo$plotViewYlim
        } else {

          layerLimits <- get_layer_worldView(loon.grob, layer = currentLayer)

          xlim <- layerLimits$xlim
          ylim <- layerLimits$ylim
        }
      } else {
        xlim <- sliderxlim
        ylim <- sliderylim
      }
    }

    loonWidgetsInfo$xlim <- xlim
    loonWidgetsInfo$ylim <- ylim

    xaxis <- grid.pretty(xlim)
    yaxis <- grid.pretty(ylim)

    title <- loonWidgetsInfo$title

    # reset margins ----------------------------------------------------------
    loonMargins <- loonWidgetsInfo$loonDefaultMargins
    margins <- rep(0, 4)

    # set scales ----------------------------------------------------------
    if("scales" %in% plotAxes1) {

      output.grob <- set_scales_grob(loon.grob = output.grob,
                                     xaxis = xaxis,
                                     yaxis = yaxis)

      margins <- margins + loonMargins$scalesMargins

      loonWidgetsInfo$showScales <- TRUE
    } else {

      output.grob <- grid::setGrob(
        gTree = output.grob,
        gPath = "axes",
        newGrob = nullGrob(name = "axes")
      )

      loonWidgetsInfo$showScales <- FALSE
    }

    # set labels -------------------------------------------------------------
    if("labels" %in% plotAxes2) {

      if(yshows == "density") {
        if(swapInShiny) {
          xlabel <- "Density"
        } else {
          ylabel <- "Density"
        }
      } else {
        # yshows is Frequency
        if(swapInShiny) {
          xlabel <- "Frequency"
        } else {
          ylabel <- "Frequency"
        }
      }

      output.grob <- set_labelsGrob(
        loon.grob = output.grob,
        showScales = loonWidgetsInfo$showScales,
        xlabel = xlabel,
        ylabel = ylabel,
        title = title
      )

      if(is.null(xlabel) || xlabel == "") loonMargins$labelMargins[1] <- loonMargins$minimumMargins[1]
      if(is.null(ylabel) || ylabel == "") loonMargins$labelMargins[2] <- loonMargins$minimumMargins[2]
      if(is.null(title) || title == "") loonMargins$labelMargins[3] <- loonMargins$minimumMargins[3]
      margins <- margins + loonMargins$labelMargins

      loonWidgetsInfo$showLabels <- TRUE
    } else {

      output.grob <- grid::setGrob(
        gTree = output.grob,
        gPath = "labels",
        newGrob = nullGrob(name = "labels")
      )

      loonWidgetsInfo$showLabels <- FALSE
    }

    if(loonWidgetsInfo$showLabels | loonWidgetsInfo$showScales) margins <- apply(cbind(margins, loonMargins$minimumMargins), 1, max)

    # set guides -------------------------------------------------------------
    if("guides" %in% plotAxes2) {

      output.grob <- set_guidesGrob(loon.grob = output.grob,
                                    xaxis = xaxis,
                                    yaxis = yaxis,
                                    loonColor = loonColor)

      loonWidgetsInfo$showGuides <- TRUE
    } else {

      output.grob <- grid::setGrob(
        gTree = output.grob,
        gPath = "guides",
        newGrob = nullGrob(name = "guides")
      )

      loonWidgetsInfo$showGuides <- FALSE
    }

    # set viewport
    output.grob <- set_viewPort_grob(
      loon.grob = output.grob,
      margins = margins,
      xlim = xlim,
      ylim = ylim
    )

    # reset boundary
    output.grob <- set_boundary_grob(loon.grob = output.grob,
                                     margins = margins,
                                     loonColor = loonColor)

    # +++++++++++++++++++++++++++++++++++++++++ set other aesthetic ++++++++++++++++++++++++++++++++++++++++
    brushId <- if(isFirstDraw) {

      outputInfo$brushId
    } else {

      if(is.null(input$plotBrush) & is.null(input$plotClick)) {
        outputInfo$brushId
      } else {

        get_brushId(
          loon.grob = output.grob,
          coord = binxy,
          swapInShiny = swapInShiny,
          position = position,
          brushInfo = input$plotBrush,
          vp = grid::vpStack(
            grid::plotViewport(margins = margins, name = "grid::plotViewport"),
            grid::dataViewport(xscale = xlim,
                               yscale = ylim,
                               name = "dataViewport")
          ),
          clickInfo = input$plotClick
        )
      }
    }

    # dynamic select -----------------------------------------------
    selectDynamic <- input[[paste0(tabPanelName, "selectDynamic")]]
    sticky <- input[[paste0(tabPanelName, "sticky")]]
    # select by color ------------------------------------
    selectByColor <- input[[paste0(tabPanelName, "selectByColor")]]

    if(sticky == "off") {

      if(!is.null(selectByColor)) {

        # when selectByColor is on, we can use brush to clear selection but keep brush id
        loonWidgetsInfo$lastSelection <- if(!is.null(input$plotBrush) || !is.null(input$plotClick)) brushId else integer(0)
        brushId <- which(color %in% selectByColor)

      } else {

        if(!is.null(outputInfo$selectByColor))
          brushId <- loonWidgetsInfo$lastSelection
      }

      if("deselect" == selectDynamic) {
        if(!is.null(input$plotBrush) | !is.null(input$plotClick)) brushId <- integer(0)
      }

    } else {

      # sticky is on
      if(!is.null(selectByColor)) {

        whichIsSelected <- union(which(color %in% selectByColor),
                                 which(loonWidgetsInfo$selected))

      } else {

        whichIsSelected <- which(loonWidgetsInfo$selected)
      }

      if("invert" == selectDynamic) {

        if(is.null(input$plotBrush)) {
          brushId <- whichIsSelected
        } else {
          brushId <- union(setdiff(whichIsSelected, brushId),
                           setdiff(brushId, whichIsSelected))
        }
      } else if("deselect" == selectDynamic) {

        if(is.null(input$plotBrush)) {
          brushId <- whichIsSelected
        } else {
          brushId <- setdiff(whichIsSelected, brushId)
        }

      } else {

        if(is.null(input$plotBrush)) {
          brushId <- whichIsSelected
        } else {
          brushId <- union(whichIsSelected, brushId)
        }
      }
    }

    # static select -----------------------------------------------
    selectStaticAll <- input[[paste0(tabPanelName, "selectStaticAll")]]
    selectStaticNone <- input[[paste0(tabPanelName, "selectStaticNone")]]
    selectStaticInvert <- input[[paste0(tabPanelName, "selectStaticInvert")]]

    if(selectStaticAll > buttons["all"]) {
      buttons["all"] <- selectStaticAll
      brushId <- seq(N)
    } else if(selectStaticNone > buttons["none"]) {
      buttons["none"] <- selectStaticNone
      brushId <- integer(0)
    } else if(selectStaticInvert > buttons["invert"]) {
      buttons["invert"] <- selectStaticInvert
      brushId <- setdiff(seq(N), brushId)
    } else NULL

    loonWidgetsInfo$selected <- rep(FALSE, N)
    loonWidgetsInfo$selected[brushId] <- TRUE

    # modify color ------------------------------------------------
    colorApply <- input[[paste0(tabPanelName, "colorApply")]]
    colorListButtons <- setNames(
      lapply(colorList, function(col) input[[paste0(tabPanelName, col)]]),
      colorList
    )
    colorPicker <- isolate(input[[paste0(tabPanelName, "colorPicker")]])

    if(colorApply > buttons["colorApply"]) {

      buttons["colorApply"] <- colorApply

      color[brushId] <- colorPicker
      loonWidgetsInfo$color <- color
    }

    for(col in colorList) {

      if(colorListButtons[[col]] > buttons[col]) {

        buttons[col] <- colorListButtons[[col]]

        color[brushId] <- col
        loonWidgetsInfo$color <- color
      }
    }

    # set deactive --------------------------------------------
    modifyDeactive <- input[[paste0(tabPanelName, "modifyDeactive")]]
    if(modifyDeactive > buttons["deactive"]) {

      buttons["deactive"] <- modifyDeactive

      if(length(brushId) > 0) {

        active[brushId] <- FALSE
        loonWidgetsInfo$active <- active

        binInfo <- get_binInfo(data = loonWidgetsInfo$x, origin = origin, active = active,
                               binwidth = binwidth, yshows = yshows)
        binId <- binInfo$binId
        binX <- binInfo$binX
        binHeight <- binInfo$binHeight

        binxy <- get_binxy(binX = binX, binId = binId, binwidth = binwidth,
                           yshows = yshows, color = color, n = sum(active))

        plotViewXlim <- grDevices::extendrange(c(binxy$xmin, binxy$xmax))
        plotViewYlim <- grDevices::extendrange(c(binxy$ymin, binxy$ymax))

        loonWidgetsInfo$plotViewXlim <- plotViewXlim
        loonWidgetsInfo$plotViewYlim <- plotViewYlim

      }
    }

    whichIsDeactive <- which(!active)

    # build grob at the end ---------------------------------------------------------------
    output.grob <- get_hist_grob(loon.grob = output.grob, yshows = yshows,
                                 binId = binId, binX = binX, binHeight = binHeight, binwidth = binwidth,
                                 n = N, swapAxes = swapInShiny,
                                 showStackedColors = showStackedColors, showOutlines = showOutlines,
                                 color = color, colorFill = colorFill, colorOutline = colorOutline)

    # highlight selected bin
    output.grob <- highlight_selected_bin_grob(loon.grob = output.grob, yshows = yshows, active = active, selected = loonWidgetsInfo$selected,
                                               binId = binId, binX = binX, binHeight = binHeight, binwidth = binwidth, n = N,
                                               swapAxes = swapInShiny, showStackedColors = showStackedColors, showOutlines = showOutlines,
                                               color = color, colorFill = colorFill, colorOutline = colorOutline,
                                               loonColor = loonColor)

    ## up, down, visible, invisible, ... layer
    layerUp <- input[[paste0(tabPanelName, "layerUp")]]
    layerDown <- input[[paste0(tabPanelName, "layerDown")]]
    layerVisible <- input[[paste0(tabPanelName, "layerVisible")]]
    layerInvisible <- input[[paste0(tabPanelName, "layerInvisible")]]
    layerPlus <- input[[paste0(tabPanelName, "layerPlus")]]

    if(layerUp > buttons["layerUp"]) {

      buttons["layerUp"] <- layerUp

      loon.grob <- move_layerUp_grob(loon.grob = loon.grob,
                                     currentLayer = currentLayer,
                                     parent = "l_hist_layers")

      output.grob <- move_layerUp_grob(loon.grob = output.grob,
                                       currentLayer = currentLayer,
                                       parent = "l_hist_layers")

    }

    if(layerDown > buttons["layerDown"]) {

      buttons["layerDown"] <- layerDown

      loon.grob <- move_layerDown_grob(loon.grob = loon.grob,
                                       currentLayer = currentLayer,
                                       parent = "l_hist_layers")

      output.grob <- move_layerDown_grob(loon.grob = output.grob,
                                         currentLayer = currentLayer,
                                         parent = "l_hist_layers")

    }

    if(layerVisible > buttons["layerVisible"]) {

      buttons["layerVisible"] <- layerVisible

      loon.grob <- move_layerVisible_grob(loon.grob = loon.grob,
                                          currentLayer = currentLayer)

      output.grob <- move_layerVisible_grob(loon.grob = output.grob,
                                            currentLayer = currentLayer)

    }

    if(layerInvisible > buttons["layerInvisible"]) {

      buttons["layerInvisible"] <- layerInvisible

      loon.grob <- move_layerInvisible_grob(loon.grob = loon.grob,
                                            currentLayer = currentLayer)

      output.grob <- move_layerInvisible_grob(loon.grob = output.grob,
                                              currentLayer = currentLayer)

    }

    if(layerPlus > buttons["layerPlus"]) {

      buttons["layerPlus"] <- layerPlus

      message("adding layers has not been inplemented so far")

    }

    # set linking info
    push <- input[[paste0(tabPanelName, "push")]]
    if(push > buttons["push"]) {
      buttons["push"] <- push
      linkingGroup <- isolate(input[[paste0(tabPanelName, "linkingGroup")]])
    } else {
      newLinkingGroup <- isolate(input[[paste0(tabPanelName, "linkingGroup")]])
      if(newLinkingGroup == "none") linkingGroup <- newLinkingGroup else NULL
    }    # set linking info
    linkingInfo <- update_linkingInfo(loon.grob,
                                      tabPanelName = tabPanelName,
                                      linkingInfo = linkingInfo,
                                      linkingGroup = linkingGroup,
                                      linkingKey = loonWidgetsInfo$linkingKey,
                                      selected = loonWidgetsInfo$selected,
                                      color = loonWidgetsInfo$color,
                                      active = loonWidgetsInfo$active,
                                      selectByColor = selectByColor,
                                      linkedStates = input[[paste0(tabPanelName, "linkedStates")]])
  }

  list(
    output.grob = output.grob,
    loon.grob = loon.grob,
    outputInfo = list(
      brushId = brushId,
      selectByColor = selectByColor,
      linkingGroup = linkingGroup,
      linkingInfo = linkingInfo,
      loonWidgetsInfo = loonWidgetsInfo,
      buttons = buttons
    )
  )
}
