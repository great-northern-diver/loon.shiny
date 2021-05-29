set_linkingInfo.l_hist <- function(loon.grob, output.grob,
                                   linkedInfo, linkedStates, tabPanelName,
                                   order, loonWidgetsInfo, ...) {

  if(length(linkedStates) > 0) {

    color <- if("color" %in% linkedStates) {
      linkedColor <- linkedInfo$color[order]
      NAid <- is.na(linkedColor)
      if(any(NAid)) {
        linkedColor[NAid] <- loonWidgetsInfo$color[NAid]
        linkedColor
      } else linkedColor
    } else loonWidgetsInfo$color

    selected <- if("selected" %in% linkedStates) {

      linkedselected <- linkedInfo$selected[order]
      NAid <- is.na(linkedselected)
      if(any(NAid)) {
        linkedselected[NAid] <- loonWidgetsInfo$selected[NAid]
        linkedselected
      } else linkedselected

    } else loonWidgetsInfo$selected

    active <- if("active" %in% linkedStates) {

      linkedactive <- linkedInfo$active[order]
      NAid <- is.na(linkedactive)
      if(any(NAid)) {
        linkedactive[NAid] <- loonWidgetsInfo$active[NAid]
        linkedactive
      } else linkedactive
    } else loonWidgetsInfo$active

    N <- loonWidgetsInfo$N
    binInfo <- get_binInfo(data = loonWidgetsInfo$x, origin = loonWidgetsInfo$origin, active = active,
                           binwidth = loonWidgetsInfo$binwidth, yshows = loonWidgetsInfo$yshows)
    binId <- binInfo$binId
    binX <- binInfo$binX
    binHeight <- binInfo$binHeight

    binxy <- get_binxy(binX = binX, binId = binId, binwidth = loonWidgetsInfo$binwidth,
                       yshows = loonWidgetsInfo$yshows, color = color, n = N)

    # build grob at the end ---------------------------------------------------------------
    output.grob <- get_hist_grob(loon.grob = output.grob, yshows = loonWidgetsInfo$yshows,
                                 binId = binId, binX = binX, binHeight = binHeight, binwidth = loonWidgetsInfo$binwidth,
                                 n = N, swapAxes = loonWidgetsInfo$swapInShiny,
                                 showStackedColors = loonWidgetsInfo$showStackedColors, showOutlines = loonWidgetsInfo$showOutlines,
                                 color = color, colorFill = loonWidgetsInfo$colorFill, colorOutline = loonWidgetsInfo$colorOutline)

    # highlight selected bin
    output.grob <- highlight_selected_bin_grob(loon.grob = output.grob, yshows = loonWidgetsInfo$yshows, active = active, selected = selected,
                                               binId = binId, binX = binX, binHeight = binHeight, binwidth = loonWidgetsInfo$binwidth, n = N,
                                               swapAxes = loonWidgetsInfo$swapInShiny, showStackedColors = loonWidgetsInfo$showStackedColors,
                                               showOutlines = loonWidgetsInfo$showOutlines,
                                               color = color, colorFill = loonWidgetsInfo$colorFill, colorOutline = loonWidgetsInfo$colorOutline,
                                               loonColor = loonWidgetsInfo$loonColor)

    loonWidgetsInfo$color <- color
    loonWidgetsInfo$selected <- selected
    loonWidgetsInfo$active <- active
  }

  list(
    output.grob =   output.grob,
    loonWidgetsInfo = loonWidgetsInfo
  )
}

