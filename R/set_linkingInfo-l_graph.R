set_linkingInfo.l_graph <- function(loon.grob, output.grob,
                                    linkedInfo, linkedStates, tabPanelName,
                                    order, loonWidgetsInfo, ...) {

  if(length(linkedStates) > 0) {

    args <- list(...)
    graph_edges <- args$graph_edges
    swapInShiny <- args$swapInShiny
    swapInLoon <- args$swapInLoon

    swap <- ((swapInShiny && !swapInLoon) || (!swapInShiny && swapInLoon))

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

    pch <- if("pch" %in% linkedStates) {

      linkedpch <- linkedInfo$pch[order]
      NAid <- is.na(linkedpch)
      if(any(NAid)) {
        linkedpch[NAid] <- loonWidgetsInfo$pch[NAid]
        linkedpch
      } else linkedpch
    } else loonWidgetsInfo$pch

    new.loon.grob <- grid::getGrob(loon.grob, "graph nodes")
    new.output.grob <- grid::getGrob(output.grob, "graph nodes")

    lapply(seq(loonWidgetsInfo$N),
           function(i) {
             grobi <- new.loon.grob$children[[i]]

             if("glyph" %in% linkedStates) {
               grobi_pch <- pch[i]
               if(!is.numeric(grobi_pch)) grobi_pch <- grobi$pch
             } else grobi_pch <- grobi$pch

             if("color" %in% linkedStates) {
               grobi_color <- color[i]
             } else {
               if(grobi_pch %in% 21:24) {
                 grobi_color <- grobi$gp$fill
               } else {
                 grobi_color <- grobi$gp$col
               }
             }

             if("size" %in% linkedStates) {
               grobi_size <- size[i]
             } else grobi_size <- grobi$gp$cex

             grobi <- grid::editGrob(
               grob = grobi,
               gp = if(grobi_pch %in% 21:24) {
                 gpar(
                   fill = grobi_color,
                   cex = grobi_size,
                   col = bounder_color()
                 )
               } else {
                 gpar(
                   col = grobi_color,
                   cex = grobi_size
                 )
               },
               pch = grobi_pch
             )

             new.loon.grob$children[[i]] <<- grobi

             new.output.grob$children[[i]] <<- if("selected" %in% linkedStates && selected[i]) {

               grid::editGrob(
                 grob = grobi,
                 gp = if(grobi_pch %in% 21:24) {
                   gpar(
                     fill = select_color(),
                     cex = grobi_size,
                     col = bounder_color()
                   )
                 } else {
                   gpar(
                     col = select_color(),
                     cex = grobi_size
                   )
                 }
               )
             } else grobi
           }
    )

    loon.grob <- grid::setGrob(
      gTree = loon.grob,
      gPath = "graph nodes",
      newGrob = new.loon.grob
    )

    output.grob <- grid::setGrob(
      gTree = output.grob,
      gPath = "graph nodes",
      newGrob = new.output.grob
    )

    if("active" %in% linkedStates) {

      # reactive
      output.grob <- set_reactive_grob(
        loon.grob = output.grob,
        index = which(active),
        graph_edges = graph_edges,
        swap = swap
      )

      loon.grob <- set_reactive_grob(
        loon.grob = loon.grob,
        index = which(active),
        graph_edges = graph_edges,
        swap = FALSE
      )

      # deactive
      output.grob <- set_deactive_grob(
        loon.grob = output.grob,
        index = which(!active)
      )

      loon.grob <- set_deactive_grob(
        loon.grob = loon.grob,
        index = which(!active)
      )
    }

    output.grob <- reorder_grob(output.grob,
                                index = which(selected))

    # update loonWidgetsInfo
    loonWidgetsInfo$pch <- pch
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
