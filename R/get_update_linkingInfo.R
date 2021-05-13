get_linkingInfo <- function(linkingGroups, loonWidgets_info, tabPanelNames) {
  
  unique_linkingGroups <- unique(linkingGroups)

  linkingInfo <- setNames(
    lapply(seq(length(unique_linkingGroups)),
           function(j) {

             if(unique_linkingGroups[j] != "none") {
               which_group <- which(linkingGroups == unique_linkingGroups[j])
               
               linkingKey <- setNames(
                 lapply(which_group, 
                        function(jj){
                          loonWidgets_info[[jj]]$linkingKey
                        }
                 ),
                 tabPanelNames[which_group]
               )
               
               linkingStates <- setNames(
                 lapply(which_group, 
                        function(jj){
                          loonWidgets_info[[jj]]$linkingStates
                        }
                 ),
                 tabPanelNames[which_group]
               )
               
               unique_linkingKey <- unique(unlist(linkingKey))
               N <- length(unique_linkingKey)
               
               selected <- setNames(rep(NA, N), unique_linkingKey)
               color <- setNames(rep(NA, N), unique_linkingKey)
               size <- setNames(rep(NA, N), unique_linkingKey)
               active <- setNames(rep(NA, N), linkingKey)
               pch <- setNames(rep(NA, N), unique_linkingKey)
               
               list(
                 selected = selected,
                 color = color,
                 size = size,
                 active = active,
                 pch = pch,
                 select_by_color = NULL,
                 linkingKey = linkingKey,
                 linkingStates = linkingStates
               )
             } else "no linkingInfo"
           }
    ), unique_linkingGroups
  )

  linkingInfo
}



update_linkingInfo <- function(loon_grob,
                               tabPanelName,
                               linkingInfo, 
                               linkingGroup, 
                               selected,
                               color,
                               active, 
                               select_by_color, 
                               linkedStates, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("update_linkingInfo", obj)
}

update_linkingInfo.l_plot <- function(loon_grob, 
                                      tabPanelName,
                                      linkingInfo, 
                                      linkingGroup, 
                                      selected,
                                      color, 
                                      size, 
                                      pch, 
                                      active, 
                                      select_by_color,
                                      linkedStates) {
  

  if(linkingGroup != "none") {
    linkedInfo <- linkingInfo[[linkingGroup]]
    linkedKey <- linkedInfo$linkingKey[[tabPanelName]]
    linkedInfo$linkingStates[[tabPanelName]] <- linkedStates
    # update linkingInfo
    if("color" %in% linkedStates) {
      linkedInfo$color[linkedKey] <- color
    }
    if("selected" %in% linkedStates) {
      linkedInfo$selected[linkedKey] <- selected
    }
    if("active" %in% linkedStates) {
      linkedInfo$active[linkedKey] <- active
    }
    if("size" %in% linkedStates) {
      linkedInfo$size[linkedKey] <- size
    }
    if("glyph" %in% linkedStates) {
      linkedInfo$pch[linkedKey] <- pch
    }
    linkedInfo$select_by_color <- select_by_color
    linkingInfo[[linkingGroup]] <- linkedInfo
  }
  linkingInfo
}

update_linkingInfo.l_graph <- function(loon_grob, 
                                      tabPanelName,
                                      linkingInfo, 
                                      linkingGroup, 
                                      selected,
                                      color, 
                                      size, 
                                      pch, 
                                      active, 
                                      select_by_color, 
                                      linkedStates) {
  
  
  if(linkingGroup != "none") {
    linkedInfo <- linkingInfo[[linkingGroup]]
    linkedKey <- linkedInfo$linkingKey[[tabPanelName]]
    linkedInfo$linkingStates[[tabPanelName]] <- linkedStates
    # update linkingInfo
    if("color" %in% linkedStates) {
      linkedInfo$color[linkedKey] <- color
    }
    if("selected" %in% linkedStates) {
      linkedInfo$selected[linkedKey] <- selected
    }
    if("active" %in% linkedStates) {
      linkedInfo$active[linkedKey] <- active
    }
    if("size" %in% linkedStates) {
      linkedInfo$size[linkedKey] <- size
    }
    if("glyph" %in% linkedStates) {
      linkedInfo$pch[linkedKey] <- pch
    }
    linkedInfo$select_by_color <- select_by_color
    linkingInfo[[linkingGroup]] <- linkedInfo
  }
  linkingInfo
}

update_linkingInfo.l_serialaxes <- function(loon_grob, 
                                            tabPanelName,
                                            linkingInfo, 
                                            linkingGroup, 
                                            selected,
                                            color, 
                                            active,
                                            size,
                                            select_by_color, 
                                            linkedStates) {
  
  
  if(linkingGroup != "none") {
    linkedInfo <- linkingInfo[[linkingGroup]]
    linkedKey <- linkedInfo$linkingKey[[tabPanelName]]
    linkedInfo$linkingStates[[tabPanelName]] <- linkedStates
    # update linkingInfo
    if("color" %in% linkedStates) {
      linkedInfo$color[linkedKey] <- color
    }
    if("selected" %in% linkedStates) {
      linkedInfo$selected[linkedKey] <- selected
    }
    if("active" %in% linkedStates) {
      linkedInfo$active[linkedKey] <- active
    }
    if("size" %in% linkedStates) {
      linkedInfo$size[linkedKey] <- size
    }
    linkedInfo$select_by_color <- select_by_color
    linkingInfo[[linkingGroup]] <- linkedInfo
  }
  linkingInfo
}

update_linkingInfo.l_hist <- function(loon_grob, 
                                      tabPanelName,
                                      linkingInfo, 
                                      linkingGroup, 
                                      selected,
                                      color, 
                                      active, 
                                      select_by_color, 
                                      linkedStates) {
  
  if(linkingGroup != "none") {
    linkedInfo <- linkingInfo[[linkingGroup]]
    linkedKey <- linkedInfo$linkingKey[[tabPanelName]]
    linkedInfo$linkingStates[[tabPanelName]] <- linkedStates
    # update linkingInfo
    if("color" %in% linkedStates) {
      linkedInfo$color[linkedKey] <- color
    }
    if("selected" %in% linkedStates) {
      linkedInfo$selected[linkedKey] <- selected
    }
    if("active" %in% linkedStates) {
      linkedInfo$active[linkedKey] <- active
    }
    linkedInfo$select_by_color <- select_by_color
    linkingInfo[[linkingGroup]] <- linkedInfo
  }
  linkingInfo
}