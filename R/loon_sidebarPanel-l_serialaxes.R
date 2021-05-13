loon_sidebarPanel.l_serialaxes <- function(loon_grob, tabPanelName, selectBy, 
                                           linkingGroup, linkingGroups, loonWidgets_info,
                                           showWorldView) {

  tabPanel(
    title = tabPanelName,
    h6(""),
    # Loon inspector Plot panel
    collapse_button("Plot", tabPanelName),
    tagsDivPlot(loon_grob, tabPanelName, loonWidgets_info, 
                linkingGroup, linkingGroups),
    # Loon inspector Select panel
    collapse_button("Select", tabPanelName),
    tagsDivSelect(loon_grob, tabPanelName, loonWidgets_info),
    # Loon inspector Link panel
    collapse_button("Linking", tabPanelName),
    tagsDivLink(loon_grob, tabPanelName, loonWidgets_info, 
                linkingGroup, linkingGroups),
    # Loon inspector Modify panel
    collapse_button("Modify", tabPanelName),
    tagsDivModify(loon_grob, tabPanelName, loonWidgets_info)
  )
}
