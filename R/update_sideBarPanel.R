update_sidebarPanel <- function(loon_grob, buttons, session, input, linkingInfo, 
                                linkingGroup, linkingGroups, tabPanelName, tabPanelNames, output_info) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("update_sidebarPanel", obj)
}

update_sidebarPanel.default <- function(loon_grob, buttons, session, input, linkingInfo, 
                                        linkingGroup, linkingGroups, tabPanelName, tabPanelNames, output_info) NULL

update_sidebarPanel.l_plot <- function(loon_grob, buttons, session, input, linkingInfo, 
                                       linkingGroup, linkingGroups, tabPanelName, tabPanelNames, output_info) {

  if(input[["navBarPage"]] == tabPanelName) {

    loonWidgets_info <- output_info$loonWidgets_info

    # update xlim ylim
    input[[paste0(tabPanelName, "plot_scale_to_plot")]]
    input[[paste0(tabPanelName, "plot_scale_to_world")]]
    input[[paste0(tabPanelName, "plot_scale_to_select")]]
    update_limitSliderInput(loon_grob, loonWidgets_info, input, session, buttons, tabPanelName,
                            brush_id = output_info$brush_id)
    
    
    # update checkbox
    colorList <- loonWidgets_info$colorList
    input[[paste0(tabPanelName, "color")]]
    lapply(colorList, function(col) input[[paste0(tabPanelName, col)]])
    update_colorCheckboxGroupInput(session, linkingGroup, linkingGroups, tabPanelName, tabPanelNames, 
                                   select_by_color = input[[paste0(tabPanelName, "select_by_color")]], 
                                   input, 
                                   loonGrob_color = loonWidgets_info$color, buttons, 
                                   colorList = colorList)
    
    # update label
    input[[paste0(tabPanelName, "layer_set")]]
    update_layerSelectInput(session, buttons, tabPanelName, layers = loonWidgets_info$layers, input)
  }
}

update_sidebarPanel.l_hist <- function(loon_grob, buttons, session, input, linkingInfo,
                                       linkingGroup, linkingGroups, tabPanelName, tabPanelNames, output_info) {

  if(input[["navBarPage"]] == tabPanelName) {

    loonWidgets_info <- output_info$loonWidgets_info
    
    input[[paste0(tabPanelName, "plot_scale_to_plot")]]
    input[[paste0(tabPanelName, "plot_scale_to_world")]]

    binInfo <- get_binInfo(data = loonWidgets_info$x, 
                           origin = input[[paste0(tabPanelName, "origin")]], 
                           active = loonWidgets_info$active, 
                           binwidth = input[[paste0(tabPanelName, "binwidth")]], 
                           yshows = input[[paste0(tabPanelName, "yshows")]])

    loonWidgets_info$plotView_xlim <- grDevices::extendrange(binInfo$binX)
    loonWidgets_info$plotView_ylim <- grDevices::extendrange(c(0, binInfo$binHeight))

    update_limitSliderInput(loon_grob, loonWidgets_info, input, session, buttons, tabPanelName)

    # updateCheckbox
    colorList <- loonWidgets_info$colorList
    input[[paste0(tabPanelName, "color")]]
    lapply(colorList, function(col) input[[paste0(tabPanelName, col)]])
    
    update_colorCheckboxGroupInput(session, linkingGroup, linkingGroups, tabPanelName, tabPanelNames, 
                                   select_by_color = input[[paste0(tabPanelName, "select_by_color")]], 
                                   input, 
                                   loonGrob_color = loonWidgets_info$color, buttons, 
                                   colorList = colorList)
    
    # update label
    input[[paste0(tabPanelName, "layer_set")]]
    update_layerSelectInput(session, buttons, tabPanelName, layers = loonWidgets_info$layers, input)
  }
}

update_sidebarPanel.l_graph <- function(loon_grob, buttons, session, input, linkingInfo, 
                                        linkingGroup, linkingGroups, tabPanelName, tabPanelNames, output_info) {

  update_sidebarPanel.l_plot(loon_grob, buttons, session, input, linkingInfo, 
                             linkingGroup, linkingGroups, tabPanelName, tabPanelNames, output_info)
}

update_sidebarPanel.l_serialaxes <- function(loon_grob, buttons, session, input, linkingInfo, 
                                             linkingGroup, linkingGroups, tabPanelName, tabPanelNames, output_info) {

  if(input[["navBarPage"]] == tabPanelName) {
    
    # updateCheckbox
    loonWidgets_info <- output_info$loonWidgets_info
    
    # updateCheckbox
    colorList <- loonWidgets_info$colorList
    input[[paste0(tabPanelName, "color")]]
    lapply(colorList, function(col) input[[paste0(tabPanelName, col)]])
    
    update_colorCheckboxGroupInput(session, linkingGroup, linkingGroups, tabPanelName, tabPanelNames, 
                                   select_by_color = input[[paste0(tabPanelName, "select_by_color")]], 
                                   input, 
                                   loonGrob_color = loonWidgets_info$color, buttons, 
                                   colorList = colorList)
    
  }
}