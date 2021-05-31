set_linkingInfo.l_serialaxes <- function(loon.grob, output.grob,
                                         linkedInfo, linkedStates, tabPanelName,
                                         order, loonWidgetsInfo, ...) {

  if(length(linkedStates) > 0) {

    loon.grob_showArea <- get_showArea(loon.grob)
    output.grob_showArea <- get_showArea(output.grob)

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

    size <- if("size" %in% linkedStates) {

      linkedsize <- linkedInfo$size[order]
      NAid <- is.na(linkedsize)
      if(any(NAid)) {
        linkedsize[NAid] <- loonWidgetsInfo$size[NAid]
        linkedsize
      } else linkedsize
    } else loonWidgetsInfo$size

    loon.grob_axesGpath <- if(get_axesLayout(loon.grob) == "parallel") "parallelAxes" else "radialAxes"
    output.grob_axesGpath <- if(get_axesLayout(output.grob) == "parallel") "parallelAxes" else "radialAxes"

    new.loon.grob <- grid::getGrob(loon.grob, loon.grob_axesGpath)
    new.output.grob <- grid::getGrob(output.grob, output.grob_axesGpath)

    lapply(seq(loonWidgetsInfo$N),
           function(i) {

             loon.grobi <- new.loon.grob$children[[i]]
             output.grobi <- new.output.grob$children[[i]]

             if("color" %in% linkedStates & !is.null(color)) {
               grobi_color <- color[i]
             } else {

               grobi_color <- if(loon.grob_showArea) {
                 loon.grobi$gp$fill
               } else loon.grobi$gp$col
             }

             grobi_size <- if("size" %in% linkedStates & !is.null(size)) {
               size[i]
             } else {
               loon.grobi$gp$lwd
             }

             loon.grobi <- grid::editGrob(
               grob = loon.grobi,
               gp = if(loon.grob_showArea) {
                 gpar(fill = grobi_color, col = NA)
               } else {
                 gpar(col = grobi_color, lwd = grobi_size)
               }
             )

             output.grobi <- grid::editGrob(
               grob = output.grobi,
               gp = if(output.grob_showArea) {
                 gpar(fill = grobi_color, col = NA)
               } else {
                 gpar(col = grobi_color, lwd = grobi_size)
               }
             )

             if("active" %in% linkedStates) {

               loon.grobi <- if(!active[i]) {
                 do.call(grob, getGrobArgs(loon.grobi))
               } else {
                 if(loon.grob_showArea) {
                   do.call(grid::polygonGrob, getGrobArgs(loon.grobi))
                 } else {
                   do.call(grid::linesGrob, getGrobArgs(loon.grobi))
                 }
               }

               output.grobi <- if(!active[i]) {
                 do.call(grob, getGrobArgs(output.grobi))
               } else {
                 if(output.grob_showArea) {
                   do.call(grid::polygonGrob, getGrobArgs(output.grobi))
                 } else {
                   do.call(grid::linesGrob, getGrobArgs(output.grobi))
                 }
               }
             }

             new.loon.grob$children[[i]] <<- loon.grobi

             new.output.grob$children[[i]] <<- if("selected" %in% linkedStates & selected[i]) {

               grid::editGrob(
                 grob = output.grobi,
                 gp = if(output.grob_showArea) {
                   gpar(fill = select_color(), col = NA)
                 } else {
                   gpar(col = select_color(), lwd = grobi_size)
                 }
               )
             } else output.grobi
           }
    )

    loon.grob <- grid::setGrob(
      gTree = loon.grob,
      gPath = loon.grob_axesGpath,
      newGrob = new.loon.grob
    )

    output.grob <- grid::setGrob(
      gTree = output.grob,
      gPath = output.grob_axesGpath,
      newGrob = new.output.grob
    )

    output.grob <- reorder_grob(output.grob,
                                index = which(selected),
                                axesGpath = output.grob_axesGpath)

    # update loonWidgetsInfo
    loonWidgetsInfo$color <- color
    loonWidgetsInfo$size <- size
    loonWidgetsInfo$selected <- selected
    loonWidgetsInfo$active <- active

  }

  list(
    output.grob = output.grob,
    loon.grob = loon.grob,
    loonWidgetsInfo = loonWidgetsInfo
  )
}
