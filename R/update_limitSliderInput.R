update_limitSliderInput <- function(loon.grob, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("update_limitSliderInput", obj)
}

update_limitSliderInput.default <- function(loon.grob, loonWidgetsInfo, input, session, buttons, tabPanelName,
                                            brushId) {

  swapInLoon <- loonWidgetsInfo$swapInLoon
  swapInShiny <- "swap" %in% input[[paste0(tabPanelName, "plotAxes1")]]
  swap <- ((swapInShiny & !swapInLoon) | (!swapInShiny & swapInLoon))

  plotViewXlim <- loonWidgetsInfo$plotViewXlim
  plotViewYlim <- loonWidgetsInfo$plotViewYlim

  layerMinus <- input[[paste0(tabPanelName, "layerMinus")]]
  currentLayerName <- input[[paste0(tabPanelName, "layer")]]
  layers <- loonWidgetsInfo$layers
  currentLayer <- layers[which(names(layers) == currentLayerName)]

  if(layerMinus > buttons["layerMinus"]) {

    if(currentLayer == "scatterplot") {
      warning("`model` layer is a descendant of layer `model` and can not be deleted.",
              call. = FALSE)

      worldViewXlim <- loonWidgetsInfo$worldViewXlim
      worldViewYlim <- loonWidgetsInfo$worldViewYlim

    } else {

      loon.grob <- grid::setGrob(
        gTree = loon.grob,
        gPath = currentLayer,
        newGrob = nullGrob(name = currentLayer)
      )

      # update choices
      shiny::updateSelectInput(
        session,
        inputId = paste0(tabPanelName, "layer"),
        choices = setdiff(layers, currentLayer)
      )

      worldView <- get_worldViewPort(loon.grob = loon.grob, parent = "scatterplot",
                                     parentExcluded = TRUE)

      worldViewXlim <- range(c(plotViewXlim, worldView$xlim))
      worldViewYlim <- range(c(plotViewYlim, worldView$ylim))
    }
  } else {
    worldViewXlim <- loonWidgetsInfo$worldViewXlim
    worldViewYlim <- loonWidgetsInfo$worldViewYlim
  }

  minX <- min(worldViewXlim) - diff(worldViewXlim)
  maxX <- max(worldViewXlim) + diff(worldViewXlim)

  minY <- min(worldViewYlim) - diff(worldViewYlim)
  maxY <- max(worldViewYlim) + diff(worldViewYlim)

  scaleToSelect <- input[[paste0(tabPanelName, "scaleToSelect")]]
  scaleToPlot <- input[[paste0(tabPanelName, "scaleToPlot")]]
  scaleToWorld <- input[[paste0(tabPanelName, "scaleToWorld")]]
  scaleToLayer <- input[[paste0(tabPanelName, "scaleToLayer")]]

  if(scaleToPlot > buttons["plot"]) {

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swap) "ylim" else "xlim",
      step = loonWidgetsInfo$stepX,
      value = plotViewXlim,
      min = minX,
      max = maxX
    )

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swap) "xlim" else "ylim",
      step = loonWidgetsInfo$stepY,
      value = plotViewYlim,
      min = minY,
      max = maxY
    )

  } else if(scaleToWorld > buttons["world"]) {

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swap) "ylim" else "xlim",
      step = loonWidgetsInfo$stepX,
      value = worldViewXlim,
      min = minX,
      max = maxX
    )

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swap) "xlim" else "ylim",
      step = loonWidgetsInfo$stepY,
      value = worldViewYlim,
      min = minY,
      max = maxY
    )

  } else if(scaleToSelect > buttons["select"]) {

    if(length(brushId) > 0) {
      shiny::updateSliderInput(
        session,
        inputId = paste0(tabPanelName, "xlim"),
        label = if(swap) "ylim" else "xlim",
        step = loonWidgetsInfo$stepX,
        value = range(
          c(
            min(loonWidgetsInfo$x[brushId]) - loonWidgetsInfo$stepX/2,
            max(loonWidgetsInfo$x[brushId]) + loonWidgetsInfo$stepX/2
          )
        ),
        min = minX,
        max = maxX
      )

      shiny::updateSliderInput(
        session,
        inputId = paste0(tabPanelName, "ylim"),
        label = if(swap) "xlim" else "ylim",
        step = loonWidgetsInfo$stepY,
        value =  range(
          c(
            min(loonWidgetsInfo$y[brushId]) - loonWidgetsInfo$stepY/2,
            max(loonWidgetsInfo$y[brushId]) + loonWidgetsInfo$stepY/2
          )
        ),
        min = minY,
        max = maxY
      )
    }
  } else if (scaleToLayer > buttons["scaleToLayer"] && length(currentLayer) > 0) {

    layerLimits <- get_layer_worldView(loon.grob, layer = currentLayer)

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swap) "ylim" else "xlim",
      step = loonWidgetsInfo$stepX,
      value = layerLimits$xlim,
      min = minX,
      max = maxX
    )

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swap) "xlim" else "ylim",
      step = loonWidgetsInfo$stepY,
      value = layerLimits$ylim,
      min = minY,
      max = maxY
    )

  } else {

    xscale <- isolate(input[[paste0(tabPanelName, "xlim")]])
    step <- NULL
    if(minX > xscale[1] || xscale[2] > maxX) {
      xscale <- plotViewXlim
      step <- log_ceiling(diff(plotViewXlim))
    }

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swap) "ylim" else "xlim",
      value = xscale,
      step = step,
      min = minX,
      max = maxX
    )

    yscale <- isolate(input[[paste0(tabPanelName, "ylim")]])
    step <- NULL
    if(minY > yscale[1] || yscale[2] > maxY) {
      yscale <- plotViewYlim
      step <- log_ceiling(diff(plotViewYlim))
    }

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swap) "xlim" else "ylim",
      value = yscale,
      step = step,
      min = minY,
      max = maxY
    )
  }
}

update_limitSliderInput.l_hist <- function(loon.grob, loonWidgetsInfo, input, session, buttons, tabPanelName) {

  swapInLoon <- loonWidgetsInfo$swapInLoon
  swapInShiny <- "swap" %in% input[[paste0(tabPanelName, "plotAxes1")]]

  layerXlim <- loonWidgetsInfo$layerXlim
  layerYlim <- loonWidgetsInfo$layerYlim

  yshows <- input[[paste0(tabPanelName, "yshows")]]
  origin <- input[[paste0(tabPanelName, "origin")]] - 1e-8 # a hack
  binwidth <- input[[paste0(tabPanelName, "binwidth")]]

  rangeChangedBydensity <- FALSE
  if(yshows == "density") {
    if(abs(buttons["binwidth"] - binwidth) > 1e-6 || abs(buttons["origin"] - origin) > 1e-6)
      rangeChangedBydensity <- TRUE
  }

  active <- loonWidgetsInfo$active
  binInfo <- get_binInfo(data = loonWidgetsInfo$x,
                         origin = origin, active = active,
                         binwidth = binwidth, yshows = yshows)
  binId <- binInfo$binId
  binX <- binInfo$binX
  binHeight <- binInfo$binHeight

  binxy <- get_binxy(binX = binX, binId = binId, binwidth = binwidth,
                     yshows = yshows, color = loonWidgetsInfo$color,
                     n = sum(active))

  plotViewXlim <- grDevices::extendrange(c(binxy$xmin, binxy$xmax))
  plotViewYlim <- grDevices::extendrange(c(binxy$ymin, binxy$ymax))

  layerMinus <- input[[paste0(tabPanelName, "layerMinus")]]
  currentLayerName <- input[[paste0(tabPanelName, "layer")]]
  layers <- loonWidgetsInfo$layers
  currentLayer <- layers[which(names(layers) == currentLayerName)]

  if(layerMinus > buttons["layerMinus"]) {

    if(currentLayer == "histogram") {

      warning("`model` layer is a descendant of layer `model` and can not be deleted.",
              call. = FALSE)

    } else {

      loon.grob <- grid::setGrob(
        gTree = loon.grob,
        gPath = currentLayer,
        newGrob = nullGrob(name = currentLayer)
      )

      worldView <-get_worldViewPort(loon.grob = loon.grob,
                                    parent = "histogram",
                                    parentExcluded = TRUE)

      layerXlim <- worldView$xlim
      layerYlim <- worldView$ylim
    }
  }

  if(swapInLoon) {

    worldViewXlim <- range(c(plotViewXlim, layerYlim))
    worldViewYlim <- range(c(plotViewYlim, layerXlim))
  } else {

    worldViewXlim <- range(c(plotViewXlim, layerXlim))
    worldViewYlim <- range(c(plotViewYlim, layerYlim))
  }

  minX <- min(worldViewXlim) - diff(worldViewXlim)
  maxX <- max(worldViewXlim) + diff(worldViewXlim)

  minY <- min(worldViewYlim) - diff(worldViewYlim)
  maxY <- max(worldViewYlim) + diff(worldViewYlim)

  scaleToPlot <- input[[paste0(tabPanelName, "scaleToPlot")]]
  scaleToWorld <- input[[paste0(tabPanelName, "scaleToWorld")]]
  scaleToLayer <- input[[paste0(tabPanelName, "scaleToLayer")]]

  # update sliderInput
  if(scaleToPlot > buttons["plot"] || rangeChangedBydensity) {

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swapInShiny) "ylim" else "xlim",
      min = minX,
      max = maxX,
      step = log_ceiling(diff(plotViewXlim)),
      value = plotViewXlim
    )

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swapInShiny) "xlim" else "ylim",
      min = minY,
      max = maxY,
      step = log_ceiling(diff(plotViewYlim)),
      value = plotViewYlim
    )

  } else if (scaleToWorld > buttons["world"]) {

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swapInShiny) "ylim" else "xlim",
      min = minX,
      max = maxX,
      step = log_ceiling(diff(worldViewXlim)),
      value = worldViewXlim
    )

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swapInShiny) "xlim" else "ylim",
      min = minY,
      max = maxY,
      step = log_ceiling(diff(worldViewYlim)),
      value = worldViewYlim
    )

  }  else if (scaleToLayer > buttons["scaleToLayer"] && length(currentLayer) > 0) {

    layerLimits <- get_layer_worldView(loon.grob, layer = currentLayer)

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swapInShiny) "ylim" else "xlim",
      min = minX,
      max = maxX,
      step = log_ceiling(diff(layerLimits$xlim)),
      value = layerLimits$xlim
    )

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swapInShiny) "xlim" else "ylim",
      min = minY,
      max = maxY,
      step = log_ceiling(diff(layerLimits$xlim)),
      value = layerLimits$ylim
    )

  } else {

    xscale <- isolate(input[[paste0(tabPanelName, "xlim")]])
    step <- NULL
    if(minX > xscale[1] || xscale[2] > maxX) {
      xscale <- plotViewXlim
      step <- log_ceiling(diff(plotViewXlim))
    }

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swapInShiny) "ylim" else "xlim",
      value = xscale,
      step = step,
      min = minX,
      max = maxX
    )

    yscale <- isolate(input[[paste0(tabPanelName, "ylim")]])
    step <- NULL
    if(minY > yscale[1] || yscale[2] > maxY) {
      yscale <- plotViewYlim
      step <- log_ceiling(diff(plotViewYlim))
    }

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swapInShiny) "xlim" else "ylim",
      value = yscale,
      step = step,
      min = minY,
      max = maxY
    )
  }
}
