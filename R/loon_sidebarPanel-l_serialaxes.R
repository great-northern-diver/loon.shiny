loon_sidebarPanel.l_serialaxes <- function(loon.grob, tabPanelName,
                                           colorList,
                                           selectBy,
                                           linkingGroup,
                                           linkingGroups,
                                           loonWidgetsInfo,
                                           showWorldView, displayedPanel) {

  tabPanel(
    title = tabPanelName,
    h6(""),
    # Loon inspector Plot panel
    collapse_button("Plot", tabPanelName),
    tagsDivPlot(loon.grob, tabPanelName, loonWidgetsInfo,
                linkingGroup, displayedPanel),
    # Loon inspector Select panel
    collapse_button("Select", tabPanelName),
    tagsDivSelect(loon.grob, tabPanelName, loonWidgetsInfo, displayedPanel),
    # Loon inspector Link panel
    collapse_button("Linking", tabPanelName),
    tagsDivLink(loon.grob, tabPanelName, loonWidgetsInfo,
                linkingGroup, linkingGroups, displayedPanel),
    # Loon inspector Modify panel
    collapse_button("Modify", tabPanelName),
    tagsDivModify(loon.grob, tabPanelName, colorList, loonWidgetsInfo, displayedPanel)
  )
}
