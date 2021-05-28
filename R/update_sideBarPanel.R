update_sidebarPanel <- function(loon.grob, buttons, session, input,
                                colorList, linkingInfo,
                                linkingGroup, linkingGroups, tabPanelName, tabPanelNames, outputInfo) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("update_sidebarPanel", obj)
}

update_sidebarPanel.default <- function(loon.grob, buttons, session, input, colorList, linkingInfo,
                                        linkingGroup, linkingGroups, tabPanelName, tabPanelNames, outputInfo) NULL

update_sidebarPanel.l_plot <- function(loon.grob, buttons, session, input, colorList, linkingInfo,
                                       linkingGroup, linkingGroups, tabPanelName, tabPanelNames, outputInfo) {

  if(input[["navBarPage"]] == tabPanelName) {

    loonWidgetsInfo <- outputInfo$loonWidgetsInfo

    # update xlim ylim
    update_limitSliderInput(loon.grob, loonWidgetsInfo, input, session, buttons, tabPanelName,
                            brushId = outputInfo$brushId)


    # update checkbox
    update_colorCheckboxGroupInput(session, linkingGroup, linkingGroups, tabPanelName, tabPanelNames,
                                   selectByColor = input[[paste0(tabPanelName, "selectByColor")]],
                                   input,
                                   loonGrob_color = loonWidgetsInfo$color, buttons,
                                   colorList = colorList)

    # update label
    update_layerSelectInput(session, buttons, tabPanelName, layers = loonWidgetsInfo$layers, input)
  }
}

update_sidebarPanel.l_hist <- function(loon.grob, buttons, session, input, colorList, linkingInfo,
                                       linkingGroup, linkingGroups, tabPanelName, tabPanelNames, outputInfo) {

  if(input[["navBarPage"]] == tabPanelName) {

    loonWidgetsInfo <- outputInfo$loonWidgetsInfo

    binInfo <- get_binInfo(data = loonWidgetsInfo$x,
                           origin = input[[paste0(tabPanelName, "origin")]],
                           active = loonWidgetsInfo$active,
                           binwidth = input[[paste0(tabPanelName, "binwidth")]],
                           yshows = input[[paste0(tabPanelName, "yshows")]])

    # loonWidgetsInfo$plotViewXlim <- range(binInfo$binX)
    # loonWidgetsInfo$plotViewYlim <- range(c(0, binInfo$binHeight))
    update_limitSliderInput(loon.grob, loonWidgetsInfo, input, session, buttons, tabPanelName)

    # updateCheckbox
    update_colorCheckboxGroupInput(session, linkingGroup, linkingGroups, tabPanelName, tabPanelNames,
                                   selectByColor = input[[paste0(tabPanelName, "selectByColor")]],
                                   input,
                                   loonGrob_color = loonWidgetsInfo$color, buttons,
                                   colorList = colorList)

    # update label
    update_layerSelectInput(session, buttons, tabPanelName, layers = loonWidgetsInfo$layers, input)
  }
}

update_sidebarPanel.l_graph <- function(loon.grob, buttons, session, input, colorList, linkingInfo,
                                        linkingGroup, linkingGroups, tabPanelName, tabPanelNames, outputInfo) {

  update_sidebarPanel.l_plot(loon.grob, buttons, session, input, colorList, linkingInfo,
                             linkingGroup, linkingGroups, tabPanelName, tabPanelNames, outputInfo)
}

update_sidebarPanel.l_serialaxes <- function(loon.grob, buttons, session, input, colorList, linkingInfo,
                                             linkingGroup, linkingGroups, tabPanelName, tabPanelNames, outputInfo) {

  if(input[["navBarPage"]] == tabPanelName) {

    # updateCheckbox
    loonWidgetsInfo <- outputInfo$loonWidgetsInfo

    # updateCheckbox
    input[[paste0(tabPanelName, "colorApply")]]
    lapply(colorList, function(col) input[[paste0(tabPanelName, col)]])

    update_colorCheckboxGroupInput(session, linkingGroup, linkingGroups, tabPanelName, tabPanelNames,
                                   selectByColor = input[[paste0(tabPanelName, "selectByColor")]],
                                   input,
                                   loonGrob_color = loonWidgetsInfo$color, buttons,
                                   colorList = colorList)

  }
}
