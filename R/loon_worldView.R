loon_worldView <- function(loon.grob, input, tabPanelName, colorList,
                           loonWidgetsInfo) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("loon_worldView", obj)
}

loon_worldView.l_plot <- function(loon.grob, input, tabPanelName, colorList,
                                  loonWidgetsInfo) {

  # just to make sure this function is evaluated.
  input$plotBrush
  input$plotClick
  input[["navBarPage"]]

  input[[paste0(tabPanelName, "plotAxes2")]]

  input[[paste0(tabPanelName, "scaleToPlot")]]
  input[[paste0(tabPanelName, "scaleToWorld")]]
  input[[paste0(tabPanelName, "scaleToSelect")]]

  input[[paste0(tabPanelName, "xlim")]]
  input[[paste0(tabPanelName, "ylim")]]

  input[[paste0(tabPanelName, "push")]]
  input[[paste0(tabPanelName, "pull")]]

  input[[paste0(tabPanelName, "sticky")]]
  input[[paste0(tabPanelName, "selectByColor")]]
  input[[paste0(tabPanelName, "selectDynamic")]]

  input[[paste0(tabPanelName, "linkingGroup")]]
  input[[paste0(tabPanelName, "linkedStates")]]

  input[[paste0(tabPanelName, "selectStaticAll")]]
  input[[paste0(tabPanelName, "selectStaticNone")]]
  input[[paste0(tabPanelName, "selectStaticInvert")]]

  input[[paste0(tabPanelName, "colorApply")]]
  lapply(colorList, function(col) input[[paste0(tabPanelName, col)]])

  input[[paste0(tabPanelName, "modifyDeactive")]]
  input[[paste0(tabPanelName, "modifyReactive")]]

  input[[paste0(tabPanelName, "alphaApply")]]

  input[[paste0(tabPanelName, "modifyMoveHalign")]]
  input[[paste0(tabPanelName, "modifyMoveValign")]]
  input[[paste0(tabPanelName, "modifyMoveHdist")]]
  input[[paste0(tabPanelName, "modifyMoveVdist")]]
  input[[paste0(tabPanelName, "modifyMoveGrid")]]
  input[[paste0(tabPanelName, "modifyMoveJitter")]]
  input[[paste0(tabPanelName, "modifyMoveReset")]]

  input[[paste0(tabPanelName, "modifyGlyphCircle")]]
  input[[paste0(tabPanelName, "modifyGlyphCcircle")]]
  input[[paste0(tabPanelName, "modifyGlyphOcircle")]]
  input[[paste0(tabPanelName, "modifyGlyphSquare")]]
  input[[paste0(tabPanelName, "modifyGlyphCsquare")]]
  input[[paste0(tabPanelName, "modifyGlyphOsquare")]]
  input[[paste0(tabPanelName, "modifyGlyphTriangle")]]
  input[[paste0(tabPanelName, "modifyGlyphCtriangle")]]
  input[[paste0(tabPanelName, "modifyGlyphOtriangle")]]
  input[[paste0(tabPanelName, "modifyGlyph")]]

  input[[paste0(tabPanelName, "absToPlus")]]
  input[[paste0(tabPanelName, "absToMinus")]]
  input[[paste0(tabPanelName, "relToPlus")]]
  input[[paste0(tabPanelName, "relToMinus")]]

  input[[paste0(tabPanelName, "modifyGlyph")]]
  input[[paste0(tabPanelName, "glyphSet")]]
  input[[paste0(tabPanelName, "nonePrimitiveGlyphSettings")]]

  input[[paste0(tabPanelName, "layerUp")]]
  input[[paste0(tabPanelName, "layerDown")]]
  input[[paste0(tabPanelName, "layerVisible")]]
  input[[paste0(tabPanelName, "layerInvisible")]]
  input[[paste0(tabPanelName, "layerPlus")]]
  input[[paste0(tabPanelName, "layerMinus")]]
  input[[paste0(tabPanelName, "scaleToLayer")]]
  input[[paste0(tabPanelName, "layerSet")]]

  input[[paste0(tabPanelName, "layer")]]

  # remove labels
  loon.grob <- grid::setGrob(
    gTree = loon.grob,
    gPath = "labels",
    newGrob = grid::nullGrob(name = "labels")
  )

  # remove guides
  loon.grob <- grid::setGrob(
    gTree = loon.grob,
    gPath = "guides",
    newGrob = grid::nullGrob(name = "guides")
  )

  # remove axes
  loon.grob <- grid::setGrob(
    gTree = loon.grob,
    gPath = "axes",
    newGrob = grid::nullGrob(name = "axes")
  )

  # remove clip
  loon.grob <- grid::setGrob(
    gTree = loon.grob,
    gPath = "clipping region",
    newGrob = grid::nullGrob(name = "clipping region")
  )

  # bounding box color to grey
  if(is.null(grid::getGrob(loon.grob, "boundary rectangle"))) {
    bound_gPath <- "boundary rectangle: rectGrob arguments"
    bound_grob <- do.call(grid::rectGrob, getGrobArgs(grid::getGrob(loon.grob, gPath = bound_gPath)))
  } else {
    bound_gPath <- "boundary rectangle"
    bound_grob <- grid::getGrob(loon.grob, gPath = bound_gPath)
  }

  loon.grob <- grid::setGrob(
    gTree = loon.grob,
    gPath = bound_gPath,
    newGrob = grid::editGrob(
      grob = bound_grob,
      gp = grid::gpar(
        fill = NA,
        col = "grey90",
        lwd = 1
      )
    )
  )

  swapInLoon <- loonWidgetsInfo$swapInLoon
  swapInShiny <- "swap" %in%  input[[paste0(tabPanelName, "plotAxes1")]]
  swap <- ((swapInShiny & !swapInLoon) | (!swapInShiny & swapInLoon))

  if(swap) {

    yscale <- loonWidgetsInfo$worldViewXlim
    xscale <- loonWidgetsInfo$worldViewYlim

    if(length(yscale) < 2) {
      yscale <- loonWidgetsInfo$plotViewXlim
    }

    if(length(xscale) < 2) {
      xscale <- loonWidgetsInfo$plotViewYlim
    }

  } else {

    xscale <- loonWidgetsInfo$worldViewXlim
    yscale <- loonWidgetsInfo$worldViewYlim

    if(length(yscale) < 2) {
      yscale <- loonWidgetsInfo$plotViewYlim
    }

    if(length(xscale) < 2) {
      xscale <- loonWidgetsInfo$plotViewXlim
    }
  }

  loon.grob <- grid::setGrob(
    gTree = loon.grob,
    gPath = "loon plot",
    newGrob = grid::editGrob(
      grob = grid::getGrob(loon.grob, "loon plot"),
      vp = grid::vpStack(
        grid::plotViewport(margins = rep(1,4), name = "plotViewport"),
        grid::dataViewport(xscale = xscale,
                     yscale = yscale,
                     name = "dataViewport")
      )
    )
  )

  grid::addGrob(
    gTree = loon.grob,
    gPath = "loon plot",
    child = grid::rectGrob(
      x = grid::unit(mean(loonWidgetsInfo$xlim), "native"),
      y = grid::unit(mean(loonWidgetsInfo$ylim), "native"),
      width = grid::unit(diff(loonWidgetsInfo$xlim), "native"),
      height = grid::unit(diff(loonWidgetsInfo$ylim), "native"),
      gp = grid::gpar(
        fill = NA,
        col = bounder_color(),
        lwd = 3
      ),
      name = "world view"
    )
  )
}

loon_worldView.l_graph <- function(loon.grob, input, tabPanelName, colorList,
                                   loonWidgetsInfo) {
  loon_worldView.l_plot(loon.grob, input, tabPanelName, colorList,
                        loonWidgetsInfo)
}

loon_worldView.l_hist <- function(loon.grob, input, tabPanelName, colorList,
                                  loonWidgetsInfo) {

  # just to make sure this function is evaluated.
  input$plotBrush
  input$plotClick
  input[["navBarPage"]]

  input[[paste0(tabPanelName, "plotShow")]]
  input[[paste0(tabPanelName, "yshows")]]
  input[[paste0(tabPanelName, "origin")]]
  input[[paste0(tabPanelName, "binwidth")]]

  input[[paste0(tabPanelName, "push")]]
  input[[paste0(tabPanelName, "pull")]]

  input[[paste0(tabPanelName, "plotAxes2")]]

  input[[paste0(tabPanelName, "scaleToPlot")]]
  input[[paste0(tabPanelName, "scaleToWorld")]]

  input[[paste0(tabPanelName, "xlim")]]
  input[[paste0(tabPanelName, "ylim")]]

  input[[paste0(tabPanelName, "sticky")]]
  input[[paste0(tabPanelName, "selectByColor")]]
  input[[paste0(tabPanelName, "selectDynamic")]]

  input[[paste0(tabPanelName, "linkingGroup")]]
  input[[paste0(tabPanelName, "linkedStates")]]

  input[[paste0(tabPanelName, "selectStaticAll")]]
  input[[paste0(tabPanelName, "selectStaticNone")]]
  input[[paste0(tabPanelName, "selectStaticInvert")]]

  input[[paste0(tabPanelName, "colorApply")]]
  lapply(colorList, function(col) input[[paste0(tabPanelName, col)]])

  input[[paste0(tabPanelName, "modifyDeactive")]]
  input[[paste0(tabPanelName, "modifyReactive")]]

  input[[paste0(tabPanelName, "layerUp")]]
  input[[paste0(tabPanelName, "layerDown")]]
  input[[paste0(tabPanelName, "layerVisible")]]
  input[[paste0(tabPanelName, "layerInvisible")]]
  input[[paste0(tabPanelName, "layerPlus")]]
  input[[paste0(tabPanelName, "layerMinus")]]
  input[[paste0(tabPanelName, "scaleToLayer")]]
  input[[paste0(tabPanelName, "layerSet")]]

  input[[paste0(tabPanelName, "layer")]]

  # remove labels
  loon.grob <- grid::setGrob(
    gTree = loon.grob,
    gPath = "labels",
    newGrob = grid::nullGrob(name = "labels")
  )

  # remove guides
  loon.grob <- grid::setGrob(
    gTree = loon.grob,
    gPath = "guides",
    newGrob = grid::nullGrob(name = "guides")
  )

  # remove axes
  loon.grob <- grid::setGrob(
    gTree = loon.grob,
    gPath = "axes",
    newGrob = grid::nullGrob(name = "axes")
  )

  # remove clip
  loon.grob <- grid::setGrob(
    gTree = loon.grob,
    gPath = "clipping region",
    newGrob = grid::nullGrob(name = "clipping region")
  )

  # bounding box color to grey
  if(is.null(grid::getGrob(loon.grob, "boundary rectangle"))) {
    bound_gPath <- "boundary rectangle: rectGrob arguments"
    bound_grob <- do.call(grid::rectGrob, getGrobArgs(grid::getGrob(loon.grob, gPath = bound_gPath)))
  } else {
    bound_gPath <- "boundary rectangle"
    bound_grob <- grid::getGrob(loon.grob, gPath = bound_gPath)
  }

  loon.grob <- grid::setGrob(
    gTree = loon.grob,
    gPath = bound_gPath,
    newGrob = grid::editGrob(
      grob = bound_grob,
      gp = grid::gpar(
        fill = NA,
        col = "grey90",
        lwd = 1
      )
    )
  )

  swapInShiny <- "swap" %in%  input[[paste0(tabPanelName, "plotAxes1")]]

  if(swapInShiny) {

    yscale <- range(c(loonWidgetsInfo$plotViewXlim, loonWidgetsInfo$layerXlim))
    xscale <- range(c(loonWidgetsInfo$plotViewYlim, loonWidgetsInfo$layerYlim))

  } else {

    xscale <- range(c(loonWidgetsInfo$plotViewXlim, loonWidgetsInfo$layerXlim))
    yscale <- range(c(loonWidgetsInfo$plotViewYlim, loonWidgetsInfo$layerYlim))
  }

  loon.grob <- grid::setGrob(
    gTree = loon.grob,
    gPath = "loon plot",
    newGrob = grid::editGrob(
      grob = grid::getGrob(loon.grob, "loon plot"),
      vp = grid::vpStack(
        grid::plotViewport(margins = rep(1,4), name = "plotViewport"),
        grid::dataViewport(xscale = xscale,
                     yscale = yscale,
                     name = "dataViewport")
      )
    )
  )

  grid::addGrob(
    gTree = loon.grob,
    gPath = "loon plot",
    child = grid::rectGrob(
      x = grid::unit(mean(loonWidgetsInfo$xlim), "native"),
      y = grid::unit(mean(loonWidgetsInfo$ylim), "native"),
      width = grid::unit(diff(loonWidgetsInfo$xlim), "native"),
      height = grid::unit(diff(loonWidgetsInfo$ylim), "native"),
      gp = grid::gpar(
        fill = NA,
        col = bounder_color(),
        lwd = 3
      ),
      name = "world view"
    )
  )
}
