loon_reactive.l_graph <- function(loon.grob, output.grob, linkingInfo, buttons, position, selectBy,
                                  linkingGroup, input, colorList, tabPanelName, outputInfo) {

  # for loon_reactive.l_graph
  # most logics are identical
  input$plotBrush
  input$plotClick

  loonWidgetsInfo <- outputInfo$loonWidgetsInfo
  pull <- input[[paste0(tabPanelName, "pull")]]

  initialDisplay <- is.null(output.grob)

  if(!is.null(output.grob) && (input[["navBarPage"]] != tabPanelName|| pull > buttons["pull"])) {

    if(pull > buttons["pull"]) {
      buttons["pull"] <- pull
      linkingGroup <- isolate(input[[paste0(tabPanelName, "linkingGroup")]])
    }

    if(linkingGroup != "none") {

      linkedInfo <- linkingInfo[[linkingGroup]]
      order <- match(loonWidgetsInfo$linkingKey, linkedInfo$linkingKey)

      modifiedLinkingInfo <- set_linkingInfo(
        loon.grob = loon.grob,
        output.grob = output.grob,
        linkedInfo = linkedInfo,
        linkedStates = input[[paste0(tabPanelName, "linkedStates")]],
        tabPanelName = tabPanelName,
        order = order,
        loonWidgetsInfo = loonWidgetsInfo,
        graph_edges = loonWidgetsInfo$graph_edges,
        swapInLoon = loonWidgetsInfo$swapInLoon,
        swapInShiny = loonWidgetsInfo$swapInShiny
      )

      selected <- linkedInfo$selected
      brushId <- which(selected)
      selectByColor <- linkedInfo$selectByColor

      output.grob <- modifiedLinkingInfo$output.grob
      loon.grob <- modifiedLinkingInfo$loon.grob
      loonWidgetsInfo <- modifiedLinkingInfo$loonWidgetsInfo

    } else {

      brushId <- outputInfo$brushId
      selectByColor <- outputInfo$selectByColor
    }
  } else {

    output.grob <- loon.grob
    loonColor <- loonWidgetsInfo$loonColor

    # interactive ------------------------------------------------------
    plotAxes1 <- input[[paste0(tabPanelName, "plotAxes1")]]
    plotAxes2 <- input[[paste0(tabPanelName, "plotAxes2")]]

    # swap, showScales, showLabels and showGuides -------------------------------------
    swapInLoon <- loonWidgetsInfo$swapInLoon
    swapInShiny <- loonWidgetsInfo$swapInShiny <- "swap" %in% plotAxes1
    swap <- ((swapInShiny & !swapInLoon) | (!swapInShiny & swapInLoon))

    N <- loonWidgetsInfo$N
    whichIsDeactive <- which(!loonWidgetsInfo$active)

    output.grob <- set_deactive_grob(
      loon.grob = output.grob,
      index = whichIsDeactive
    )

    loon.grob <- set_deactive_grob(
      loon.grob = loon.grob,
      index = whichIsDeactive
    )

    #labels <- get_labels(output.grob)
    labels <- loonWidgetsInfo$labels
    title <- labels$title

    layerSet <- input[[paste0(tabPanelName, "layerSet")]]
    currentLayerName <- input[[paste0(tabPanelName, "layer")]]
    newLayerLabel <- isolate(input[[paste0(tabPanelName, "newLayerLabel")]])

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

      # update layers
      newLayers <- setdiff(layers, currentLayer)
      newLayersName <- setdiff(layersName, currentLayerName)
      names(newLayers) <- newLayersName
      loonWidgetsInfo$layers <- newLayers

      worldView <- get_worldViewPort(loon.grob = loon.grob,
                                     parentExcluded = TRUE)
      loonWidgetsInfo$worldViewXlim <- range(c(loonWidgetsInfo$plotViewXlim,
                                               worldView$xlim))
      loonWidgetsInfo$worldViewYlim <- range(c(loonWidgetsInfo$plotViewYlim,
                                               worldView$ylim))
    }

    # plot scale to
    scaleToSelect <- input[[paste0(tabPanelName, "scaleToSelect")]]
    scaleToPlot <- input[[paste0(tabPanelName, "scaleToPlot")]]
    scaleToWorld <- input[[paste0(tabPanelName, "scaleToWorld")]]
    scaleToLayer <- input[[paste0(tabPanelName, "scaleToLayer")]]

    sliderxlim <- input[[paste0(tabPanelName, "xlim")]]
    sliderylim <- input[[paste0(tabPanelName, "ylim")]]

    # brushId <- if(initialDisplay) {
    #
    #   outputInfo$brushId
    # } else {
    #   if(is.null(input$plotBrush) & is.null(input$plotClick)) {
    #
    #     outputInfo$brushId
    #   } else {
    #
    #     get_brushId(
    #       loon.grob = output.grob,
    #       coord = list(
    #         x = loonWidgetsInfo$x,
    #         y = loonWidgetsInfo$y
    #       ),
    #       swapInShiny = swapInShiny,
    #       swapInLoon = swapInLoon,
    #       position = position,
    #       brushInfo = input$plotBrush,
    #       vp = get_viewPort(loon.grob = output.grob),
    #       clickInfo = input$plotClick
    #     )
    #   }
    # }
    brushId <- outputInfo$brushId

    if(swap) {

      if(scaleToSelect > buttons["select"]) {

        buttons["select"] <- scaleToSelect

        if(length(brushId) == 0) {
          message("no points selected")

          loonWidgetsInfo$ylim <- sliderxlim
          loonWidgetsInfo$xlim <- sliderylim
        } else {

          loonWidgetsInfo$ylim <- grDevices::extendrange(
            c(
              min(loonWidgetsInfo$x[brushId]) - loonWidgetsInfo$stepX/2,
              max(loonWidgetsInfo$x[brushId]) + loonWidgetsInfo$stepX/2
            )
          )
          loonWidgetsInfo$xlim <- grDevices::extendrange(
            c(
              min(loonWidgetsInfo$y[brushId]) - loonWidgetsInfo$stepY/2,
              max(loonWidgetsInfo$y[brushId]) + loonWidgetsInfo$stepY/2
            )
          )
        }
      } else if(scaleToPlot > buttons["plot"]) {

        buttons["plot"] <- scaleToPlot

        loonWidgetsInfo$ylim <- loonWidgetsInfo$plotViewXlim
        loonWidgetsInfo$xlim <- loonWidgetsInfo$plotViewYlim

      } else if(scaleToWorld > buttons["world"]) {

        buttons["world"] <- scaleToWorld

        loonWidgetsInfo$ylim <- loonWidgetsInfo$worldViewXlim
        loonWidgetsInfo$xlim <- loonWidgetsInfo$worldViewYlim

      } else if (scaleToLayer > buttons["scaleToLayer"]  && length(currentLayer) > 0) {

        buttons["scaleToLayer"] <- scaleToLayer

        if(currentLayer == "graph") {
          loonWidgetsInfo$ylim <- loonWidgetsInfo$plotViewXlim
          loonWidgetsInfo$xlim <- loonWidgetsInfo$plotViewYlim
        } else {
          layerLimits <- get_layer_worldView(loon.grob, layer = currentLayer)
          loonWidgetsInfo$ylim <-layerLimits$xlim
          loonWidgetsInfo$xlim <- layerLimits$ylim
        }

      } else {

        loonWidgetsInfo$ylim <- sliderxlim
        loonWidgetsInfo$xlim <- sliderylim
      }

      # swap label
      ylabel <- labels$xlabel
      xlabel <- labels$ylabel

      # swap output grob
      output.grob <- swapCoords_grob(output.grob,
                                     x = loonWidgetsInfo$y,
                                     y = loonWidgetsInfo$x,
                                     reactive = FALSE)
      # swap layer
      output.grob <- swap_layer_grob(output.grob, parent = "graph")
    } else {

      if(scaleToSelect > buttons["select"]) {

        buttons["select"] <- scaleToSelect

        if(length(brushId) == 0) {
          message("no points selected")
          loonWidgetsInfo$xlim <- sliderxlim
          loonWidgetsInfo$ylim <- sliderylim
        } else {

          loonWidgetsInfo$xlim <- grDevices::extendrange(
            c(
              min(loonWidgetsInfo$x[brushId]) - loonWidgetsInfo$stepX/2,
              max(loonWidgetsInfo$x[brushId]) + loonWidgetsInfo$stepX/2
            )
          )
          loonWidgetsInfo$ylim <- grDevices::extendrange(
            c(
              min(loonWidgetsInfo$y[brushId]) - loonWidgetsInfo$stepY/2,
              max(loonWidgetsInfo$y[brushId]) + loonWidgetsInfo$stepY/2
            )
          )
        }
      } else if(scaleToPlot > buttons["plot"]) {

        buttons["plot"] <- scaleToPlot

        loonWidgetsInfo$xlim <- loonWidgetsInfo$plotViewXlim
        loonWidgetsInfo$ylim <- loonWidgetsInfo$plotViewYlim
      } else if(scaleToWorld > buttons["world"]) {

        buttons["world"] <- scaleToWorld

        loonWidgetsInfo$xlim <- loonWidgetsInfo$worldViewXlim
        loonWidgetsInfo$ylim <- loonWidgetsInfo$worldViewYlim
      } else if(scaleToLayer > buttons["scaleToLayer"]  && length(currentLayer) > 0) {

        buttons["scaleToLayer"] <- scaleToLayer

        if(currentLayer == "graph") {
          loonWidgetsInfo$xlim <- loonWidgetsInfo$plotViewXlim
          loonWidgetsInfo$ylim <- loonWidgetsInfo$plotViewYlim
        } else {
          layerLimits <- get_layer_worldView(loon.grob, layer = currentLayer)
          loonWidgetsInfo$xlim <-layerLimits$xlim
          loonWidgetsInfo$ylim <- layerLimits$ylim
        }

      } else {
        loonWidgetsInfo$xlim <- sliderxlim
        loonWidgetsInfo$ylim <- sliderylim
      }

      xlabel <- labels$xlabel
      ylabel <- labels$ylabel
    }

    xaxis <- grid::grid.pretty(loonWidgetsInfo$xlim)
    yaxis <- grid::grid.pretty(loonWidgetsInfo$ylim)

    # reset margins
    loonMargins <- loonWidgetsInfo$loonDefaultMargins
    margins <- rep(0, 4)

    if("scales" %in% plotAxes2) {

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

    if("labels" %in% plotAxes1) {

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

    if(loonWidgetsInfo$showLabels || loonWidgetsInfo$showScales) {
      margins <- apply(cbind(margins, loonMargins$minimumMargins), 1, max)
    }

    ############ Begin: set brushId ############
    brushId <- if(initialDisplay) {

      outputInfo$brushId

    } else {
      # sweeping or brushing
      if(is.null(input$plotBrush) && is.null(input$plotClick)) {

        outputInfo$brushId

      } else {

        get_brushId(
          loon.grob = output.grob,
          coord = list(
            x = loonWidgetsInfo$x,
            y = loonWidgetsInfo$y
          ),
          swapInShiny = swapInShiny,
          swapInLoon = swapInLoon,
          position = position,
          brushInfo = input$plotBrush,
          vp = grid::vpStack(
            grid::plotViewport(margins = margins, name = "grid::plotViewport"),
            grid::dataViewport(xscale = if(swap) loonWidgetsInfo$ylim else loonWidgetsInfo$xlim,
                         yscale = if(swap) loonWidgetsInfo$xlim else loonWidgetsInfo$ylim,
                         name = "dataViewport")
          ),
          clickInfo = input$plotClick
        )
      }
    }

    sticky <- input[[paste0(tabPanelName, "sticky")]]
    selectByColor <- input[[paste0(tabPanelName, "selectByColor")]]

    # select dynamic
    selectDynamic <- input[[paste0(tabPanelName, "selectDynamic")]]

    if(sticky == "off") {

      if(!is.null(selectByColor)) {

        # when selectByColor is on, we can use brush to clear selection but keep brush id
        loonWidgetsInfo$lastSelection <- if(!is.null(input$plotBrush) | !is.null(input$plotClick)) brushId else integer(0)
        brushId <-  which(loonWidgetsInfo$color %in% selectByColor)

      } else {

        if(!is.null(outputInfo$selectByColor))
          brushId <- loonWidgetsInfo$lastSelection
      }

      if("deselect" == selectDynamic) {
        if(!is.null(input$plotBrush) || !is.null(input$plotClick))
          brushId <- integer(0)
      }

    } else {

      if(!is.null(selectByColor)) {

        whichIsSelected <- union(which(loonWidgetsInfo$color %in% selectByColor), which(loonWidgetsInfo$selected))

      } else {

        whichIsSelected <- which(loonWidgetsInfo$selected)
      }

      if("invert" == selectDynamic) {

        if(is.null(input$plotBrush) | !is.null(input$plotClick)) {
          brushId <- whichIsSelected
        } else {
          brushId <- union(setdiff(whichIsSelected, brushId), setdiff(brushId, whichIsSelected))
        }
      } else if("deselect" == selectDynamic) {

        if(is.null(input$plotBrush) | !is.null(input$plotClick)) {
          brushId <- whichIsSelected
        } else {
          brushId <- setdiff(whichIsSelected, brushId)
        }

      } else {

        if(is.null(input$plotBrush) | !is.null(input$plotClick)) {
          brushId <- whichIsSelected
        } else {
          brushId <- union(whichIsSelected, brushId)
        }
      }
    }

    # select panel -------------------------------------
    selectStaticAll <- input[[paste0(tabPanelName, "selectStaticAll")]]
    selectStaticNone <- input[[paste0(tabPanelName, "selectStaticNone")]]
    selectStaticInvert <- input[[paste0(tabPanelName, "selectStaticInvert")]]

    if(selectStaticAll > buttons["all"]) {

      buttons["all"] <- selectStaticAll

      brushId <- seq(N)
    } else if(selectStaticNone > buttons["none"]) {

      buttons["none"] <- selectStaticNone

      brushId <- numeric(0)
    } else if(selectStaticInvert > buttons["invert"]) {

      buttons["invert"] <- selectStaticInvert

      brushId <- setdiff(seq(N), brushId)
    } else NULL

    # brushId must be active points
    brushId <- setdiff(brushId, whichIsDeactive)
    ############ End: set brushId ############

    loonWidgetsInfo$selected <- rep(FALSE, N)
    loonWidgetsInfo$selected[brushId] <- TRUE

    # highlight color
    output.grob <- set_color_grob(
      loon.grob = output.grob,
      index = brushId,
      newColor = select_color()
    )

    # adjust color -------------------------------
    colorApply <- input[[paste0(tabPanelName, "colorApply")]]
    colorListButtons <- setNames(
      lapply(colorList, function(col) input[[paste0(tabPanelName, col)]]),
      colorList
    )
    colorPicker <- isolate(input[[paste0(tabPanelName, "colorPicker")]])

    if(colorApply > buttons["colorApply"]) {

      buttons["colorApply"] <- colorApply

      loon.grob <- set_color_grob(
        loon.grob = loon.grob,
        index = brushId,
        newColor = colorPicker
      )

      loonWidgetsInfo$color[brushId] <- colorPicker
    }

    for(col in colorList) {

      if(colorListButtons[[col]] > buttons[col]) {

        buttons[col] <- colorListButtons[[col]]

        loon.grob <- set_color_grob(
          loon.grob = loon.grob,
          index = brushId,
          newColor = col
        )

        loonWidgetsInfo$color[brushId] <- col
      }
    }

    alphaApply <- input[[paste0(tabPanelName, "alphaApply")]]
    if(alphaApply > buttons["alphaApply"]) {

      buttons["alphaApply"] <- alphaApply

      alpha <- isolate(input[[paste0(tabPanelName, "alpha")]])

      loon.grob <- set_alpha_grob(
        loon.grob = loon.grob,
        index = brushId,
        newAlpha = alpha
      )

      output.grob <- set_alpha_grob(
        loon.grob = output.grob,
        index = brushId,
        newAlpha = alpha
      )

      loonWidgetsInfo$alpha[brushId] <- alpha
    }

    # adjust deactive--------------------------------
    modifyDeactive <- input[[paste0(tabPanelName, "modifyDeactive")]]

    if(modifyDeactive > buttons["deactive"]) {

      buttons["deactive"] <- modifyDeactive

      output.grob <- set_deactive_grob(
        loon.grob = output.grob,
        index = brushId
      )

      loon.grob <- set_deactive_grob(
        loon.grob = loon.grob,
        index = brushId
      )

      loonWidgetsInfo$active[brushId] <- FALSE
      whichIsDeactive <- union(whichIsDeactive, brushId)
    }

    # set reactive
    modifyReactive <- input[[paste0(tabPanelName, "modifyReactive")]]
    if (modifyReactive > buttons["reactive"]) {

      buttons["reactive"] <- modifyReactive

      output.grob <- set_reactive_grob(
        loon.grob = output.grob,
        index = whichIsDeactive,
        graph_edges = loonWidgetsInfo$graph_edges,
        swap = swap
      )

      loon.grob <- set_reactive_grob(
        loon.grob = loon.grob,
        index = whichIsDeactive,
        graph_edges = loonWidgetsInfo$graph_edges,
        swap = FALSE
      )

      whichIsDeactive <- numeric(0)
      loonWidgetsInfo$active <- rep(TRUE, N)

    }

    showOrbit <- input[[paste0(tabPanelName, "show_nodes_label")]]
    if(showOrbit) {

      output.grob <- set_node_labelsGrob(output.grob, whichIsDeactive)

      loonWidgetsInfo$showOrbit <- TRUE
    } else {

      output.grob <- grid::setGrob(
        gTree = output.grob,
        gPath = "graph labels",
        newGrob = nullGrob(name = "graph labels")
      )

      loonWidgetsInfo$showOrbit <- FALSE
    }

    # modify move
    modifyMoveHalign <- input[[paste0(tabPanelName, "modifyMoveHalign")]]
    modifyMoveValign <- input[[paste0(tabPanelName, "modifyMoveValign")]]
    modifyMoveHdist <- input[[paste0(tabPanelName, "modifyMoveHdist")]]
    modifyMoveVdist <- input[[paste0(tabPanelName, "modifyMoveVdist")]]
    modifyMoveGrid <- input[[paste0(tabPanelName, "modifyMoveGrid")]]
    modifyMoveJitter <- input[[paste0(tabPanelName, "modifyMoveJitter")]]
    modifyMoveReset <- input[[paste0(tabPanelName, "modifyMoveReset")]]

    if(modifyMoveHalign > buttons["halign"]) {

      buttons["halign"] <- modifyMoveHalign

      # to determine if the default widget is swapped
      halignY <- if(swap) mean(loonWidgetsInfo$x[brushId]) else mean(loonWidgetsInfo$y[brushId])

      output.grob <- move_halign_grob(loon.grob = output.grob,
                                      index = brushId,
                                      swap = swap,
                                      halignY = halignY,
                                      temporary = TRUE)

      loon.grob <- move_halign_grob(loon.grob = loon.grob,
                                    index = brushId,
                                    swap = swap,
                                    halignY = halignY,
                                    temporary = FALSE)

      if(swap) loonWidgetsInfo$x[brushId] <- halignY else loonWidgetsInfo$y[brushId] <- halignY

    } else if(modifyMoveValign > buttons["valign"]) {

      buttons["valign"] <- modifyMoveValign

      valignX <- if(swap) mean(loonWidgetsInfo$y[brushId]) else mean(loonWidgetsInfo$x[brushId])

      output.grob <- move_valign_grob(loon.grob = output.grob,
                                      index = brushId,
                                      swap = swap,
                                      valignX = valignX,
                                      temporary = TRUE)

      loon.grob <- move_valign_grob(loon.grob = loon.grob,
                                    index = brushId,
                                    swap = swap,
                                    valignX = valignX,
                                    temporary = FALSE)

      if(swap) loonWidgetsInfo$y[brushId] <- valignX else loonWidgetsInfo$x[brushId] <- valignX

    } else if(modifyMoveHdist > buttons["hdist"]) {

      buttons["hdist"] <- modifyMoveHdist

      hdistY <- if(swap) {

        seq(
          from = min(loonWidgetsInfo$x[brushId]),
          to = max(loonWidgetsInfo$x[brushId]),
          length.out = length(brushId)
        )
      } else {

        seq(
          from = min(loonWidgetsInfo$y[brushId]),
          to = max(loonWidgetsInfo$y[brushId]),
          length.out = length(brushId)
        )
      }

      output.grob <- move_hdist_grob(loon.grob = output.grob,
                                     index = brushId,
                                     swap = swap,
                                     hdistY = hdistY,
                                     temporary = TRUE)

      loon.grob <- move_hdist_grob(loon.grob = loon.grob,
                                   index = brushId,
                                   swap = swap,
                                   hdistY = hdistY,
                                   temporary = FALSE)

      if(swap) loonWidgetsInfo$x[brushId] <- hdistY else loonWidgetsInfo$y[brushId] <- hdistY

    } else if(modifyMoveVdist > buttons["vdist"]) {

      buttons["vdist"] <- modifyMoveVdist

      vdistX <- if(swap) {

        seq(
          from = min(loonWidgetsInfo$y[brushId]),
          to = max(loonWidgetsInfo$y[brushId]),
          length.out = length(brushId)
        )
      } else {

        seq(
          from = min(loonWidgetsInfo$x[brushId]),
          to = max(loonWidgetsInfo$x[brushId]),
          length.out = length(brushId)
        )
      }

      output.grob <- move_vdist_grob(loon.grob = output.grob,
                                     index = brushId,
                                     swap = swap,
                                     vdistX = vdistX,
                                     temporary = TRUE)

      loon.grob <- move_vdist_grob(loon.grob = loon.grob,
                                   index = brushId,
                                   swap = swap,
                                   vdistX = vdistX,
                                   temporary = FALSE)

      if(swap) loonWidgetsInfo$y[brushId] <- vdistX else loonWidgetsInfo$x[brushId] <- vdistX

    } else if(modifyMoveJitter > buttons["jitter"]) {

      buttons["jitter"] <- modifyMoveJitter

      jitterxy <- jitter_coord(
        x = if(swap) loonWidgetsInfo$y else loonWidgetsInfo$x,
        y = if(swap) loonWidgetsInfo$x else loonWidgetsInfo$y,
        index = brushId
      )

      output.grob <- move_jitter_grob(loon.grob = output.grob,
                                      index = brushId,
                                      swap = swap,
                                      jitterxy = jitterxy,
                                      temporary = TRUE)

      loon.grob <- move_jitter_grob(loon.grob = loon.grob,
                                    index = brushId,
                                    swap = swap,
                                    jitterxy = jitterxy,
                                    temporary = FALSE)

      if(swap) {

        loonWidgetsInfo$y[brushId] <- jitterxy$x
        loonWidgetsInfo$x[brushId] <- jitterxy$y
      } else {

        loonWidgetsInfo$x[brushId] <- jitterxy$x
        loonWidgetsInfo$y[brushId] <- jitterxy$y
      }
    } else if(modifyMoveGrid > buttons["grid"]) {

      buttons["grid"] <- modifyMoveGrid

      squarexy <- square_coord(
        x = if(swap) loonWidgetsInfo$y else loonWidgetsInfo$x,
        y = if(swap) loonWidgetsInfo$x else loonWidgetsInfo$y,
        index = brushId
      )

      output.grob <- move_grid_grob(loon.grob = output.grob,
                                    index = brushId,
                                    swap = swap,
                                    squarexy = squarexy,
                                    temporary = TRUE)

      loon.grob <- move_grid_grob(loon.grob = loon.grob,
                                  index = brushId,
                                  swap = swap,
                                  squarexy = squarexy,
                                  temporary = FALSE)

      if(swap) {

        loonWidgetsInfo$y[brushId] <- squarexy$x
        loonWidgetsInfo$x[brushId] <- squarexy$y
      } else {

        loonWidgetsInfo$x[brushId] <- squarexy$x
        loonWidgetsInfo$y[brushId] <- squarexy$y
      }
    } else if(modifyMoveReset > buttons["reset"]) {

      buttons["reset"] <- modifyMoveReset

      output.grob <- move_reset_grob(loon.grob = output.grob,
                                     index = seq(N),
                                     swap = swap,
                                     xyOriginal = loonWidgetsInfo$xyOriginal,
                                     temporary = TRUE)

      loon.grob <- move_reset_grob(loon.grob = loon.grob,
                                   index = seq(N),
                                   swap = swap,
                                   xyOriginal = loonWidgetsInfo$xyOriginal,
                                   temporary = FALSE)

      loonWidgetsInfo$x <- loonWidgetsInfo$xOriginal
      loonWidgetsInfo$y <- loonWidgetsInfo$yOriginal

    } else NULL # none of move buttons is active


    # adjust glyph --------------------------------
    modifyGlyphCircle <- input[[paste0(tabPanelName, "modifyGlyphCircle")]]
    modifyGlyphCcircle <- input[[paste0(tabPanelName, "modifyGlyphCcircle")]]
    modifyGlyphOcircle <- input[[paste0(tabPanelName, "modifyGlyphOcircle")]]
    modifyGlyphSquare <- input[[paste0(tabPanelName, "modifyGlyphSquare")]]
    modifyGlyphCsquare <- input[[paste0(tabPanelName, "modifyGlyphCsquare")]]
    modifyGlyphOsquare <- input[[paste0(tabPanelName, "modifyGlyphOsquare")]]
    modifyGlyphTriangle <- input[[paste0(tabPanelName, "modifyGlyphTriangle")]]
    modifyGlyphCtriangle <- input[[paste0(tabPanelName, "modifyGlyphCtriangle")]]
    modifyGlyphOtriangle <- input[[paste0(tabPanelName, "modifyGlyphOtriangle")]]

    newGlyph <- NULL

    if(modifyGlyphCircle > buttons["circle"]) {
      buttons["circle"] <- modifyGlyphCircle
      newGlyph <- "circle"
    }
    if(modifyGlyphOcircle > buttons["ocircle"]) {
      buttons["ocircle"] <- modifyGlyphOcircle
      newGlyph <- "ocircle"
    }
    if(modifyGlyphCcircle > buttons["ccircle"]) {
      buttons["ccircle"] <- modifyGlyphCcircle
      newGlyph <- "ccircle"
    }
    if(modifyGlyphSquare > buttons["square"]) {
      buttons["square"] <- modifyGlyphSquare
      newGlyph <- "square"
    }
    if(modifyGlyphOsquare > buttons["osquare"]) {
      buttons["osquare"] <- modifyGlyphOsquare
      newGlyph <- "osquare"
    }
    if(modifyGlyphCsquare > buttons["csquare"]) {
      buttons["csquare"] <- modifyGlyphCsquare
      newGlyph <- "csquare"
    }
    if(modifyGlyphTriangle > buttons["triangle"]) {
      buttons["triangle"] <- modifyGlyphTriangle
      newGlyph <- "triangle"
    }
    if(modifyGlyphOtriangle > buttons["otriangle"]) {
      buttons["otriangle"] <- modifyGlyphOtriangle
      newGlyph <- "otriangle"
    }
    if(modifyGlyphCtriangle > buttons["ctriangle"]) {
      buttons["ctriangle"] <- modifyGlyphCtriangle
      newGlyph <- "ctriangle"
    }

    if(!is.null(newGlyph))  {

      newPch <- glyph_to_pch(newGlyph)

      loon.grob <- set_glyph_grob(
        loon.grob = loon.grob,
        index = brushId,
        newPch = newPch,
        tmp = FALSE,
        color = loonWidgetsInfo$color
      )

      output.grob <- set_glyph_grob(
        loon.grob = output.grob,
        index = brushId,
        newPch = newPch,
        tmp = TRUE,
        color = loonWidgetsInfo$color
      )

      loonWidgetsInfo$glyph[brushId] <- newGlyph
      loonWidgetsInfo$pch[brushId] <- newPch
      loonWidgetsInfo$glyphNames[brushId] <- paste0("primitive_glyph ", loonWidgetsInfo$index[brushId])
    }

    # adjust size--------------------------------
    absToPlus <- input[[paste0(tabPanelName, "absToPlus")]]
    if(absToPlus > buttons["absToPlus"]) {

      buttons["absToPlus"] <- absToPlus

      if(length(brushId) > 0) {
        newSize <- min(loonWidgetsInfo$size[brushId]) + default_step_size()
        loonWidgetsInfo$size[brushId] <- rep(newSize, length(brushId))

        loon.grob <- set_size_grob(loon.grob = loon.grob,
                                   index = brushId,
                                   newSize = loonWidgetsInfo$size)

        output.grob <- set_size_grob(loon.grob = output.grob,
                                     index = brushId,
                                     newSize = loonWidgetsInfo$size)
      }
    }

    absToMinus <- input[[paste0(tabPanelName, "absToMinus")]]
    if(absToMinus > buttons["absToMinus"]) {

      buttons["absToMinus"] <- absToMinus

      if(length(brushId) > 0) {
        newSize <- min(loonWidgetsInfo$size[brushId]) - default_step_size()
        if(newSize <= 0) newSize <- minimumSize()
        loonWidgetsInfo$size[brushId] <- rep(newSize, length(brushId))

        loon.grob <- set_size_grob(loon.grob = loon.grob,
                                   index = brushId,
                                   newSize = loonWidgetsInfo$size)

        output.grob <- set_size_grob(loon.grob = output.grob,
                                     index = brushId,
                                     newSize = loonWidgetsInfo$size)
      }
    }

    relToPlus <- input[[paste0(tabPanelName, "relToPlus")]]
    if(relToPlus > buttons["relToPlus"]) {

      buttons["relToPlus"] <- relToPlus

      if(length(brushId) > 0) {

        loonWidgetsInfo$size[brushId] <- loonWidgetsInfo$size[brushId] + default_step_size()

        loon.grob <- set_size_grob(loon.grob = loon.grob,
                                   index = brushId,
                                   newSize = loonWidgetsInfo$size)

        output.grob <- set_size_grob(loon.grob = output.grob,
                                     index = brushId,
                                     newSize = loonWidgetsInfo$size)
      }
    }

    relToMinus <- input[[paste0(tabPanelName, "relToMinus")]]
    if(relToMinus > buttons["relToMinus"]) {

      buttons["relToMinus"] <- relToMinus

      if(length(brushId) > 0) {

        newSize <- loonWidgetsInfo$size[brushId] - default_step_size()
        newSize[which(newSize <= 0)] <- minimumSize()
        loonWidgetsInfo$size[brushId] <- newSize

        loon.grob <- set_size_grob(loon.grob = loon.grob,
                                   index = brushId,
                                   newSize = loonWidgetsInfo$size)

        output.grob <- set_size_grob(loon.grob = output.grob,
                                     index = brushId,
                                     newSize = loonWidgetsInfo$size)
      }
    }

    # reorder selected points
    output.grob <- reorder_grob(output.grob,
                                number = N,
                                brushId)


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
                                     parent = "l_graph_layers")

      output.grob <- move_layerUp_grob(loon.grob = output.grob,
                                       currentLayer = currentLayer,
                                       parent = "l_graph_layers")

    }

    if(layerDown > buttons["layerDown"]) {

      buttons["layerDown"] <- layerDown

      loon.grob <- move_layerDown_grob(loon.grob = loon.grob,
                                       currentLayer = currentLayer,
                                       parent = "l_graph_layers")

      output.grob <- move_layerDown_grob(loon.grob = output.grob,
                                         currentLayer = currentLayer,
                                         parent = "l_graph_layers")

    }

    if(layerVisible > buttons["layerVisible"]) {

      buttons["layerVisible"] <- layerVisible

      loon.grob <- move_layerVisible_grob(loon.grob = loon.grob,
                                          currentLayer = currentLayer,
                                          graph_edges = loonWidgetsInfo$graph_edges,
                                          N = N)

      output.grob <- move_layerVisible_grob(loon.grob = output.grob,
                                            currentLayer = currentLayer,
                                            graph_edges = loonWidgetsInfo$graph_edges,
                                            N = N)

    }

    if(layerInvisible > buttons["layerInvisible"]) {

      buttons["layerInvisible"] <- layerInvisible

      loon.grob <- move_layerInvisible_grob(loon.grob = loon.grob,
                                            currentLayer = currentLayer,
                                            N = N)

      output.grob <- move_layerInvisible_grob(loon.grob = output.grob,
                                              currentLayer = currentLayer,
                                              N = N)

    }

    if(layerPlus > buttons["layerPlus"]) {

      buttons["layerPlus"] <- layerPlus

      message("adding layers has not been inplemented so far")

    }

    # reset vp
    output.grob <- set_viewPort_grob(
      loon.grob = output.grob,
      margins = margins,
      xlim = loonWidgetsInfo$xlim,
      ylim = loonWidgetsInfo$ylim
    )

    # reset boundary
    output.grob <- set_boundaryGrob(loon.grob = output.grob,
                                     margins = margins,
                                     loonColor = loonColor)

    # set linking info
    push <- input[[paste0(tabPanelName, "push")]]
    if(push > buttons["push"]) {
      buttons["push"] <- push
      linkingGroup <- isolate(input[[paste0(tabPanelName, "linkingGroup")]])
    } else {
      newLinkingGroup <- isolate(input[[paste0(tabPanelName, "linkingGroup")]])
      if(newLinkingGroup == "none") linkingGroup <- newLinkingGroup else NULL
    }
    # set linking info
    linkingInfo <- update_linkingInfo(loon.grob,
                                      tabPanelName = tabPanelName,
                                      linkingInfo = linkingInfo,
                                      linkingGroup = linkingGroup,
                                      linkingKey = loonWidgetsInfo$linkingKey,
                                      selected = loonWidgetsInfo$selected,
                                      color = loonWidgetsInfo$color,
                                      size = loonWidgetsInfo$size,
                                      pch = loonWidgetsInfo$pch,
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
