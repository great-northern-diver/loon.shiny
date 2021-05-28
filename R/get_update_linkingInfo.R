get_linkingInfo <- function(linkingGroups, loonWidgetsInfo,
                            tabPanelNames, n) {

  linkingKey <- lapply(seq(n),
                       function(i){
                         loonWidgetsInfo[[i]]$linkingKey
                       }
  )
  uniqueLinkingKey <- unique(unlist(linkingKey))
  N <- length(uniqueLinkingKey)

  uniqueLinkingGroups <- unique(linkingGroups)

  linkingInfo <- setNames(
    lapply(seq(length(uniqueLinkingGroups)),
           function(j) {

             if(uniqueLinkingGroups[j] != "none") {

               linkedGroup <- which(linkingGroups == uniqueLinkingGroups[j])

               selected <- rep(NA, N)
               color <- rep(NA, N)
               size <- rep(NA, N)
               active <- rep(NA, N)
               pch <- rep(NA, N)

               list(
                 selected = selected,
                 color = color,
                 size = size,
                 active = active,
                 pch = pch,
                 selectByColor = NULL,
                 linkingKey = uniqueLinkingKey # linkingKey will not be modified once the plot is rendered
               )
             } else "no linkingInfo"
           }
    ), uniqueLinkingGroups
  )

  linkingInfo
}



update_linkingInfo <- function(loon.grob,
                               tabPanelName,
                               linkingInfo,
                               linkingGroup,
                               selected,
                               color,
                               active,
                               selectByColor,
                               linkedStates, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("update_linkingInfo", obj)
}

update_linkingInfo.l_plot <- function(loon.grob,
                                      tabPanelName,
                                      # they are the central commendar
                                      linkingInfo,
                                      linkingGroup,
                                      # they are `loon.grob`'s aesthetics
                                      linkingKey,
                                      selected,
                                      color,
                                      size,
                                      pch,
                                      active,
                                      selectByColor,
                                      linkedStates) {


  if(linkingGroup != "none") {
    linkedInfo <- linkingInfo[[linkingGroup]]
    linkedKey <- linkedInfo$linkingKey

    # the `linkingKey` is the linkingKey of the `loon.grob`
    # the `linkedKey` is the linkingKey in this group; they may be different
    order <- match(linkingKey, linkedKey)

    # update linkingInfo
    if("color" %in% linkedStates) {
      linkedInfo$color[order] <- color
    }
    if("selected" %in% linkedStates) {
      linkedInfo$selected[order] <- selected
    }
    if("active" %in% linkedStates) {
      linkedInfo$active[order] <- active
    }
    if("size" %in% linkedStates) {
      linkedInfo$size[order] <- size
    }
    if("glyph" %in% linkedStates) {
      linkedInfo$pch[order] <- pch
    }
    linkedInfo$selectByColor <- selectByColor
    linkingInfo[[linkingGroup]] <- linkedInfo
  }
  linkingInfo
}

update_linkingInfo.l_graph <- function(loon.grob,
                                       tabPanelName,
                                       # they are the central commendar
                                       linkingInfo,
                                       linkingGroup,
                                       # they are `loon.grob`'s aesthetics
                                       linkingKey,
                                       selected,
                                       color,
                                       size,
                                       pch,
                                       active,
                                       selectByColor,
                                       linkedStates) {


  update_linkingInfo.l_plot(loon.grob = loon.grob,
                            tabPanelName = tabPanelName,
                            # they are the central commendar
                            linkingInfo = linkingInfo,
                            linkingGroup = linkingGroup,
                            # they are `loon.grob`'s aesthetics
                            linkingKey = linkingKey,
                            selected = selected,
                            color = color,
                            size = size,
                            pch = pch,
                            active = active,
                            selectByColor = selectByColor,
                            linkedStates = linkedStates)
}

update_linkingInfo.l_serialaxes <- function(loon.grob,
                                            tabPanelName,
                                            linkingInfo,
                                            linkingGroup,
                                            linkingKey,
                                            selected,
                                            color,
                                            active,
                                            size,
                                            selectByColor,
                                            linkedStates) {


  if(linkingGroup != "none") {
    linkedInfo <- linkingInfo[[linkingGroup]]
    linkedKey <- linkedInfo$linkingKey

    # the `linkingKey` is the linkingKey of the `loon.grob`
    # the `linkedKey` is the linkingKey in this group; they may be different
    order <- match(linkingKey, linkedKey)

    if("color" %in% linkedStates) {
      linkedInfo$color[order] <- color
    }
    if("selected" %in% linkedStates) {
      linkedInfo$selected[order] <- selected
    }
    if("active" %in% linkedStates) {
      linkedInfo$active[order] <- active
    }
    if("size" %in% linkedStates) {
      linkedInfo$size[order] <- size
    }
    linkedInfo$selectByColor <- selectByColor
    linkingInfo[[linkingGroup]] <- linkedInfo
  }
  linkingInfo
}

update_linkingInfo.l_hist <- function(loon.grob,
                                      tabPanelName,
                                      linkingInfo,
                                      linkingGroup,
                                      linkingKey,
                                      selected,
                                      color,
                                      active,
                                      selectByColor,
                                      linkedStates) {

  if(linkingGroup != "none") {

    linkedInfo <- linkingInfo[[linkingGroup]]
    linkedKey <- linkedInfo$linkingKey

    # the `linkingKey` is the linkingKey of the `loon.grob`
    # the `linkedKey` is the linkingKey in this group; they may be different
    order <- match(linkingKey, linkedKey)

    if("color" %in% linkedStates) {
      linkedInfo$color[order] <- color
    }
    if("selected" %in% linkedStates) {
      linkedInfo$selected[order] <- selected
    }
    if("active" %in% linkedStates) {
      linkedInfo$active[order] <- active
    }
    linkedInfo$selectByColor <- selectByColor
    linkingInfo[[linkingGroup]] <- linkedInfo
  }
  linkingInfo
}
