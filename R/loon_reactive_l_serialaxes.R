loon_reactive.l_serialaxes <- function(loon.grob, output.grob, linkingInfo, buttons, position, selectBy,
                                       linkingGroup, input, colorList, tabPanelName, outputInfo) {

  plotBrush <- input$plotBrush
  plotClick <- input$plotClick

  loonWidgetsInfo <- outputInfo$loonWidgetsInfo
  pull <- input[[paste0(tabPanelName, "pull")]]

  initialDisplay <- is.null(output.grob)

  if(!initialDisplay && (input[["navBarPage"]] != tabPanelName || pull > buttons["pull"])) {

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
        loonWidgetsInfo = loonWidgetsInfo
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

    axesLayoutInShiny <- input[[paste0(tabPanelName, "axesLayout")]]
    axesLayoutInLoon <- loonWidgetsInfo$axesLayout

    plotShow <- input[[paste0(tabPanelName, "plot")]]
    showGuides <- "showGuides" %in% plotShow
    showAxes <- "showAxes" %in% plotShow
    showAxesLabels <- "showAxesLabels" %in% plotShow
    showLabels <- "showLabels" %in% plotShow
    showArea <- "showArea" %in% plotShow
    andrews <- "andrews" %in% plotShow

    title <- loonWidgetsInfo$title
    titleGpath <- if(!is.null(grid::getGrob(output.grob, "title"))) {
      "title"
    } else {
      "title: textGrob arguments"
    }

    loonDefaultSerialaxesArgs <- loon_defaultSerialaxesSettings_args()

    if(showLabels & title != "") {
      titleGrob <- grid::textGrob(
        name = titleGpath,
        label = title,
        y = unit(1, "npc") - unit(.8, "lines"),
        gp = gpar(fontsize = loonDefaultSerialaxesArgs$titleFontsize,
                  fontface="bold"),
        vjust = .5
      )
    } else {

      titleGrob <- grob(name = titleGpath)
    }

    output.grob <- grid::setGrob(
      gTree = output.grob,
      gPath = titleGpath,
      newGrob = titleGrob
    )

    scaling <- input[[paste0(tabPanelName, "scaling")]]
    scaledActiveData <- switch(scaling,
                               "variable" = loonWidgetsInfo$variableScaledActiveData,
                               "observation" = loonWidgetsInfo$observationScaledActiveData,
                               "data" = loonWidgetsInfo$dataScaledActiveData,
                               "none" = loonWidgetsInfo$noneScaledActiveData)

    N <- loonWidgetsInfo$N
    whichIsDeactive <- which(!loonWidgetsInfo$active)

    len.xaxis <- loonWidgetsInfo$lenSeqName
    axesLabels <- loonWidgetsInfo$seqName
    andrewsSeriesLength <- loonWidgetsInfo$andrewsSeriesLength

    if(andrews) {
      axesLabels <- round(seq(-pi, pi, length.out = len.xaxis), 2)
      fourierTrans <- loonWidgetsInfo$fourierTrans

      scaledActiveData <- as.matrix(scaledActiveData) %*% fourierTrans$matrix

      dataRange <- range(scaledActiveData, na.rm = TRUE)
      d <- if(diff(dataRange) == 0) 1 else diff(dataRange)

      scaledActiveData <- (scaledActiveData - min(scaledActiveData, na.rm = TRUE))/d
    }

    # set layout axes, showAxes, showGuides, ...
    if(axesLayoutInShiny == "parallel") {

      xaxis <- seq(0, 1, length.out =  len.xaxis)

      axesGpath <- if(axesLayoutInShiny == axesLayoutInLoon) "parallelAxes" else "radialAxes"
      yaxis <- grid.pretty(loonWidgetsInfo$ylim)
      len.yaxis <- length(yaxis)

      # set guides -----------------------------------------------------------

      guidesGrob <- if(showGuides) {

        gTree(
          children = do.call(
            gList,
            lapply(seq(len.xaxis + len.yaxis + 1),
                   function(i) {
                     if(i == 1){
                       grid::rectGrob(gp = gpar(col = NA, fill = loonDefaultSerialaxesArgs$guidesBackground),
                                      name = "bounding box")
                     } else if( i > 1 && i<= (1 + len.xaxis)){
                       condGrob(
                         test = showAxes,
                         grobFun = grid::linesGrob,
                         name = paste("x axis", i - 1),
                         x = unit(rep(xaxis[i - 1],2 ), "native"),
                         y =  unit(c(0, 1), "native"),
                         gp = gpar(col =  loonDefaultSerialaxesArgs$lineColor1,
                                   lwd = loonDefaultSerialaxesArgs$guideLineWidth)
                       )
                     } else {
                       grid::linesGrob(
                         x = unit(c(0, 1), "native"),
                         y =  unit(rep(yaxis[i - (1 + len.xaxis)],2 ), "native"),
                         gp = gpar(col =loonDefaultSerialaxesArgs$lineColor1,
                                   lwd = loonDefaultSerialaxesArgs$guideLineWidth),
                         name = paste("y axis", i - (1 + len.xaxis))
                       )
                     }
                   })),
          name = "guides"
        )
      } else {

        gTree(
          children =  do.call(
            gList,
            lapply(seq(len.xaxis),
                   function(i) {
                     condGrob(
                       test = showAxes,
                       grobFun = grid::linesGrob,
                       name = paste("x axis", i),
                       x = unit(rep(xaxis[i],2 ), "native"),
                       y =  unit(c(0, 1), "native"),
                       gp = gpar(col =  loonDefaultSerialaxesArgs$lineColor2,
                                 lwd = loonDefaultSerialaxesArgs$guideLineWidth)
                     )
                   }
            )
          ),
          name = "guides"
        )
      }
      loonWidgetsInfo$showGuides <- showGuides
      output.grob <- grid::setGrob(
        gTree = output.grob,
        gPath = "guides",
        newGrob = guidesGrob
      )

      # set labels -----------------------------------------------------------
      labelsGrob <- gTree(
        children = do.call(
          gList,
          lapply(seq(len.xaxis),
                 function(i) {
                   condGrob(
                     test = showAxesLabels,
                     grobFun = grid::textGrob,
                     label = axesLabels[i],
                     name = paste("label", i),
                     x = unit(xaxis[i], "native"),
                     y = unit(0, "npc") + unit(1.2, "lines"),
                     gp = gpar(fontsize = loonDefaultSerialaxesArgs$labelFontsize), vjust = 1
                   )
                 }
          )
        ),
        name = "labels"
      )

      loonWidgetsInfo$showLabels <- showLabels

      output.grob <- grid::setGrob(
        gTree = output.grob,
        gPath = "labels",
        newGrob = labelsGrob
      )

      if(andrews) {
        len.xaxis <- andrewsSeriesLength
        x.axis <- seq(0, 1, length.out = len.xaxis)
      } else {
        x.axis <- xaxis
      }
      # set axes -----------------------------------------------------------
      axesGrob <- gTree(
        children = gList(
          do.call(
            gList,
            lapply(seq_len(N),
                   function(i){
                     if (showArea) {

                       xx <- unit(c(x.axis, rev(x.axis)), "native")
                       yy <- unit(c(scaledActiveData[i, ], rep(0, len.xaxis)), "native")

                       loonWidgetsInfo$x[[i]] <<- xx
                       loonWidgetsInfo$y[[i]] <<- yy

                       grid::polygonGrob(
                         x = xx,
                         y = yy,
                         name = paste("polyline: showArea", i),
                         gp = gpar(fill = loonWidgetsInfo$color[i],
                                   col = NA)
                       )
                     } else {

                       xx <- unit(x.axis, "native")
                       yy <- unit(scaledActiveData[i, ], "native")

                       loonWidgetsInfo$x[[i]] <<- xx
                       loonWidgetsInfo$y[[i]] <<- yy

                       grid::linesGrob(
                         x = xx,
                         y = yy,
                         name = paste("polyline", i),
                         gp = gpar(
                           col = loonWidgetsInfo$color[i],
                           lwd = if(is.na(loonWidgetsInfo$size[i])) loonDefaultSerialaxesArgs$linewidthDefault else loonWidgetsInfo$size[i]
                         )
                       )
                     }
                   }
            )
          )
        ),
        name = axesGpath
      )
      loonWidgetsInfo$showAxes <- showAxes

      output.grob <- grid::setGrob(
        gTree = output.grob,
        gPath = axesGpath,
        newGrob = axesGrob
      )
    } else if(axesLayoutInShiny == "radial") {

      xpos <- unit(0.5, "native")
      ypos <- unit(0.5, "native")
      radius <- loonDefaultSerialaxesArgs$radius
      angle <- seq(0, 2*pi, length.out = len.xaxis + 1)[1:len.xaxis]

      axesGpath <- if(axesLayoutInShiny == axesLayoutInLoon) "radialAxes" else "parallelAxes"
      # set guides ---------------------------------------------------------
      guidesGrob <- if(showGuides) {

        gTree(
          children = gList(
            grid::rectGrob(gp = gpar(col = NA, fill = loonDefaultSerialaxesArgs$guidesBackground),
                           name = "bounding box"),
            grid::polygonGrob(xpos + unit(radius * cos(seq(0, 2*pi, length=101)), "npc"),
                              ypos + unit(radius * sin(seq(0, 2*pi, length=101)), "npc"),
                              gp = gpar(fill = NA, col = l_getOption("guidelines"),
                                        lwd = loonDefaultSerialaxesArgs$guideLineWidth),
                              name = "bounding line" # TODO find line width
            ),
            condGrob(
              test = showAxes,
              grobFun = grid::polylineGrob,
              name = "axes",
              x = xpos + unit(c(rep(0, len.xaxis) ,radius * cos(angle)), "npc"),
              y =  ypos + unit(c(rep(0, len.xaxis) ,radius * sin(angle)), "npc"),
              id = rep(1:len.xaxis, 2),
              gp = gpar(col = loonDefaultSerialaxesArgs$lineColor1,
                        lwd = loonDefaultSerialaxesArgs$guideLineWidth)   # TODO Again with width loon should use guide colours
            )
          ),
          name = "guides"
        )
      } else {

        gTree(
          children = gList(
            condGrob(
              test = showAxes,
              grobFun = grid::polylineGrob,
              name = "axes",
              x = unit(c(rep(0, len.xaxis) ,radius * cos(angle)), "npc") + xpos,
              y = unit(c(rep(0, len.xaxis) ,radius * sin(angle)), "npc") + ypos,
              id = rep(1:len.xaxis, 2),
              gp = gpar(col = loonDefaultSerialaxesArgs$lineColor2,
                        lwd = loonDefaultSerialaxesArgs$guideLineWidth)
            )
          ), name = "guides"
        )
      }
      loonWidgetsInfo$showGuides <- showGuides
      output.grob <- grid::setGrob(
        gTree = output.grob,
        gPath = "guides",
        newGrob = guidesGrob
      )

      # set labels ---------------------------------------------------------
      labelsGrob <- gTree(
        children = do.call(
          gList,
          lapply(seq(len.xaxis),
                 function(i) {
                   condGrob(
                     test = showAxesLabels,
                     grobFun = grid::textGrob,
                     name = paste("label", i),
                     label = axesLabels[i],
                     x = unit((radius + loonDefaultSerialaxesArgs$radiusOffset) * cos(angle[i]), "npc") + xpos,
                     y = unit((radius + loonDefaultSerialaxesArgs$radiusOffset) * sin(angle[i]), "npc") + ypos,
                     gp = gpar(fontsize = loonDefaultSerialaxesArgs$labelFontsize), vjust = 0.5
                   )
                 }
          )
        ),
        name = "labels"
      )
      loonWidgetsInfo$showLabels <- showLabels

      output.grob <- grid::setGrob(
        gTree = output.grob,
        gPath = "labels",
        newGrob = labelsGrob
      )

      # set axes ---------------------------------------------------------
      if(andrews) {
        angle <- seq(0, 2*pi, length.out = andrewsSeriesLength + 1)[1:andrewsSeriesLength]
      }
      axesGrob <- gTree(
        children = do.call(
          gList,
          lapply(seq_len(N),
                 function(i){

                   radialxais <- radius * scaledActiveData[i, ] * cos(angle)
                   radialyais <- radius * scaledActiveData[i, ] * sin(angle)

                   xx <- xpos + unit(c(radialxais, radialxais[1]), "npc")
                   yy <- ypos + unit(c(radialyais, radialyais[1]), "npc")

                   loonWidgetsInfo$x[[i]] <<- xx
                   loonWidgetsInfo$y[[i]] <<- yy

                   if(showArea){

                     grid::polygonGrob(
                       x = xx,
                       y = yy,
                       name = paste("polyline: showArea", i),
                       gp = gpar(fill = loonWidgetsInfo$color[i], col = NA)
                     )
                   } else {

                     grid::linesGrob(
                       x = xx,
                       y = yy,
                       name = paste("polyline", i),
                       gp = gpar(
                         col = loonWidgetsInfo$color[i],
                         lwd = if(is.na(loonWidgetsInfo$size[i])) loonDefaultSerialaxesArgs$linewidthDefault else loonWidgetsInfo$size[i]
                       )
                     )
                   }
                 }
          )
        ),
        name = axesGpath
      )

      loonWidgetsInfo$showAxes <- showAxes

      output.grob <- grid::setGrob(
        gTree = output.grob,
        gPath = axesGpath,
        newGrob = axesGrob
      )
    } else NULL

    defaultSerialaxesSettings <- get_defaultSerialaxesSettings(axesLayoutInShiny)

    vp <- grid::vpStack(
      grid::plotViewport(margins = loonDefaultSerialaxesArgs$margins, name = "plotViewport"),
      grid::dataViewport(xscale = defaultSerialaxesSettings$xscale,
                         yscale = defaultSerialaxesSettings$yscale,
                         name = "dataViewport")
    )

    # to make the `convert` function work properly
    grid::pushViewport(vp)

    native.x <- list()
    native.y <- list()

    for(i in seq(N)) {
      native.x[[i]] <- grid::convertX(loonWidgetsInfo$x[[i]], unitTo = "native", TRUE)
      native.y[[i]] <- grid::convertY(loonWidgetsInfo$y[[i]], unitTo = "native", TRUE)
    }

    loonWidgetsInfo$native.x <- native.x
    loonWidgetsInfo$native.y <- native.y

    # query the `offset`
    offset <- get_offset(vp = vp,
                         l = plotBrush$domain$left %||% plotClick$domain$left %||% -0.04,
                         r = plotBrush$domain$right %||% plotClick$domain$right %||% 1.04,
                         b = plotBrush$domain$bottom %||% plotClick$domain$bottom %||% -0.04,
                         t = plotBrush$domain$top %||% plotClick$domain$top %||% 1.04)
    loonWidgetsInfo$offset <- offset

    # sweeping or brushing
    brushId <- if(initialDisplay) {

      outputInfo$brushId

    } else {

      if(is.null(plotBrush) && is.null(plotClick)) {

        outputInfo$brushId

      } else {

        if(!is.null(position))
          get_brushId(
            loon.grob = output.grob,
            coord = list(
              x = loonWidgetsInfo$x,
              y = loonWidgetsInfo$y
            ),
            position = position,
            brushInfo = plotBrush,
            vp = vp,
            offset = offset,
            clickInfo = plotClick,
            N = N
          )
      }
    }

    sticky <- input[[paste0(tabPanelName, "sticky")]]
    selectByColor <- input[[paste0(tabPanelName, "selectByColor")]]

    if(sticky == "off") {

      if(!is.null(selectByColor)) {

        # when selectByColor is on, we can use brush to clear selection but keep brush id
        loonWidgetsInfo$lastSelection <- if(!is.null(plotBrush) || !is.null(plotClick)) brushId else integer(0)
        brushId <- which(loonWidgetsInfo$color %in% selectByColor)
      } else {

        if(!is.null(outputInfo$selectByColor)) brushId <- loonWidgetsInfo$lastSelection
      }
    } else {

      # sticky is on
      if(!is.null(selectByColor)) {
        whichIsSelected <- union(which(loonWidgetsInfo$color %in% selectByColor), which(loonWidgetsInfo$selected))
      } else {
        whichIsSelected <- which(loonWidgetsInfo$selected)
      }

      if(is.null(plotBrush)) {
        brushId <- whichIsSelected
      } else {
        brushId <- union(whichIsSelected, brushId)
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

      brushId <- integer(0)

    } else if(selectStaticInvert > buttons["invert"]) {

      buttons["invert"] <- selectStaticInvert

      brushId <- setdiff(seq(N), brushId)
    } else NULL

    # brushId must be active points
    brushId <- setdiff(brushId, whichIsDeactive)

    loonWidgetsInfo$selected <- rep(FALSE, N)
    loonWidgetsInfo$selected[brushId] <- TRUE

    # highlight color
    output.grob <- set_color_grob(
      loon.grob = output.grob,
      index = brushId,
      newColor = select_color(),
      axesGpath = axesGpath
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
        newColor = colorPicker,
        axesGpath = axesGpath
      )

      loonWidgetsInfo$color[brushId] <- colorPicker
    }

    for(col in colorList) {

      if(colorListButtons[[col]] > buttons[col]) {

        buttons[col] <- colorListButtons[[col]]

        loon.grob <- set_color_grob(
          loon.grob = loon.grob,
          index = brushId,
          newColor = col,
          axesGpath = axesGpath
        )

        loonWidgetsInfo$color[brushId] <- col
      }
    }

    # adjust transparency
    alphaApply <- input[[paste0(tabPanelName, "alphaApply")]]
    if(alphaApply > buttons["alphaApply"]) {

      buttons["alphaApply"] <- alphaApply

      alpha <- isolate(input[[paste0(tabPanelName, "alpha")]])

      loon.grob <- set_alpha_grob(
        loon.grob = loon.grob,
        index = brushId,
        newAlpha = alpha,
        axesGpath = axesGpath
      )

      output.grob <- set_alpha_grob(
        loon.grob = output.grob,
        index = brushId,
        newAlpha = alpha,
        axesGpath = axesGpath
      )

      loonWidgetsInfo$alpha[brushId] <- alpha
    }

    # adjust deactive--------------------------------
    output.grob <- set_deactive_grob(
      loon.grob = output.grob,
      index = whichIsDeactive,
      axesGpath = axesGpath
    )

    loon.grob <- set_deactive_grob(
      loon.grob = loon.grob,
      index = whichIsDeactive,
      axesGpath = axesGpath
    )


    modifyDeactive <- input[[paste0(tabPanelName, "modifyDeactive")]]

    if(modifyDeactive > buttons["deactive"]) {

      buttons["deactive"] <- modifyDeactive

      loon.grob <- set_deactive_grob(
        loon.grob = loon.grob,
        index = brushId,
        axesGpath = axesGpath
      )

      output.grob <- set_deactive_grob(
        loon.grob = output.grob,
        index = brushId,
        axesGpath = axesGpath
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
        axesGpath = axesGpath,
        showArea = showArea
      )

      loon.grob <- set_reactive_grob(
        loon.grob = loon.grob,
        index = whichIsDeactive,
        axesGpath = axesGpath,
        showArea = showArea
      )
      loonWidgetsInfo$active <- rep(TRUE, N)
    }

    # adjust size--------------------------------
    absToPlus <- input[[paste0(tabPanelName, "absToPlus")]]
    if(absToPlus > buttons["absToPlus"]) {

      buttons["absToPlus"] <- absToPlus

      if(length(brushId) > 0) {
        newSize <- min(loonWidgetsInfo$size[brushId]) + default_step_size(line = TRUE)
        loonWidgetsInfo$size[brushId] <- rep(newSize, length(brushId))

        loon.grob <- set_size_grob(loon.grob = loon.grob,
                                   index = brushId,
                                   newSize = loonWidgetsInfo$size,
                                   axesGpath = axesGpath,
                                   showArea = showArea)

        output.grob <- set_size_grob(loon.grob = output.grob,
                                     index = brushId,
                                     newSize = loonWidgetsInfo$size,
                                     axesGpath = axesGpath,
                                     showArea = showArea)
      }
    }

    absToMinus <- input[[paste0(tabPanelName, "absToMinus")]]
    if(absToMinus > buttons["absToMinus"]) {

      buttons["absToMinus"] <- absToMinus

      if(length(brushId) > 0) {
        newSize <- min(loonWidgetsInfo$size[brushId]) - default_step_size(line = TRUE)
        if(newSize <= 0) newSize <- minimumSize()
        loonWidgetsInfo$size[brushId] <- rep(newSize, length(brushId))

        loon.grob <- set_size_grob(loon.grob = loon.grob,
                                   index = brushId,
                                   newSize = loonWidgetsInfo$size,
                                   axesGpath = axesGpath,
                                   showArea = showArea)

        output.grob <- set_size_grob(loon.grob = output.grob,
                                     index = brushId,
                                     newSize = loonWidgetsInfo$size,
                                     axesGpath = axesGpath,
                                     showArea = showArea)
      }
    }

    relToPlus <- input[[paste0(tabPanelName, "relToPlus")]]
    if(relToPlus > buttons["relToPlus"]) {

      buttons["relToPlus"] <- relToPlus

      if(length(brushId) > 0) {

        loonWidgetsInfo$size[brushId] <- loonWidgetsInfo$size[brushId] + default_step_size(line = TRUE)

        loon.grob <- set_size_grob(loon.grob = loon.grob,
                                   index = brushId,
                                   newSize = loonWidgetsInfo$size,
                                   axesGpath = axesGpath,
                                   showArea = showArea)

        output.grob <- set_size_grob(loon.grob = output.grob,
                                     index = brushId,
                                     newSize = loonWidgetsInfo$size,
                                     axesGpath = axesGpath,
                                     showArea = showArea)
      }
    }

    relToMinus <- input[[paste0(tabPanelName, "relToMinus")]]
    if(relToMinus > buttons["relToMinus"]) {

      buttons["relToMinus"] <- relToMinus

      if(length(brushId) > 0) {

        newSize <- loonWidgetsInfo$size[brushId] - default_step_size(line = TRUE)
        newSize[which(newSize <= 0)] <- minimumSize()
        loonWidgetsInfo$size[brushId] <- newSize

        loon.grob <- set_size_grob(loon.grob = loon.grob,
                                   index = brushId,
                                   newSize = loonWidgetsInfo$size,
                                   axesGpath = axesGpath,
                                   showArea = showArea)

        output.grob <- set_size_grob(loon.grob = output.grob,
                                     index = brushId,
                                     newSize = loonWidgetsInfo$size,
                                     axesGpath = axesGpath,
                                     showArea = showArea)
      }
    }

    output.grob <- reorder_grob(output.grob,
                                number = N,
                                brushId,
                                axesGpath = axesGpath)


    output.grob <- grid::setGrob(
      gTree = output.grob,
      gPath = "l_serialaxes",
      newGrob = grid::editGrob(
        grob = grid::getGrob(output.grob, "l_serialaxes"),
        vp = vp
      )
    )

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
                                      active = loonWidgetsInfo$active,
                                      size = loonWidgetsInfo$size,
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
