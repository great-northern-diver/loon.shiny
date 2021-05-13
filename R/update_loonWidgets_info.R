update_loonWidgets_info <- function(loonWidgets_info, linkedInfo, tabPanelName) {
  
  linkedKey <- linkedInfo$linkingKey[[tabPanelName]]
  linkedStates <- linkedInfo$linkingStates[[tabPanelName]]
  
  if(length(linkedStates) > 0 & length(linkedKey) > 0) {

    color <- linkedInfo$color[linkedKey]
    size <- linkedInfo$size[linkedKey]
    pch <- linkedInfo$pch[linkedKey]
    selected <- linkedInfo$selected[linkedKey]
    active <- linkedInfo$active[linkedKey]
    
    if("glyph" %in% linkedStates & !is.null(pch)) loonWidgets_info$pch <- pch
    if("color" %in% linkedStates & !is.null(color)) loonWidgets_info$color <- color
    if("size" %in% linkedStates & !is.null(size)) loonWidgets_info$size <- size
    if("selected" %in% linkedStates & !is.null(selected)) loonWidgets_info$selected <- selected
    if("active" %in% linkedStates & !is.null(active)) loonWidgets_info$active <- active
  }
  
  loonWidgets_info
}