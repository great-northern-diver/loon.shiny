loon_sidebarPanel.l_plot <- function(loon_grob, 
                                     tabPanelName, 
                                     selectBy, 
                                     linkingGroup, 
                                     linkingGroups, 
                                     loonWidgets_info,
                                     showWorldView) {
  
  shiny::tabPanel(
    title = tabPanelName,
    if(showWorldView) {
      shiny::plotOutput(outputId = paste0(tabPanelName, "plot_world_view"),
                        height = "200px")
    },
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
    tagsDivModify(loon_grob, tabPanelName, loonWidgets_info),
    # Loon inspector Layer panel
    collapse_button("Layer", tabPanelName),
    tagsDivLayer(loon_grob, tabPanelName, loonWidgets_info),
    # Loon inspector Glyph panel
    collapse_button("Glyph", tabPanelName),
    tagsDivGlyph(loon_grob, tabPanelName, loonWidgets_info)
  )
}