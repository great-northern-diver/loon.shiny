# return tabPanel with corresponding sidebarPanel
loon_sidebarPanel <- function(loon.grob, tabPanelName,
                              colorList, selectBy,
                              linkingGroup, linkingGroups, loonWidgetsInfo,
                              showWorldView, displayedPanel) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("loon_sidebarPanel", obj)
}

loon_sidebarPanel.default <- function(loon.grob, tabPanelName,
                                      colorList, selectBy,
                                      linkingGroup, linkingGroups, loonWidgetsInfo,
                                      showWorldView, displayedPanel) {
  shiny::tabPanel(
    title = tabPanelName,
    shiny::fixedRow(
      shiny::column(
        6,
        do.call(
          shiny::helpText,
          list(inputId = paste0(tabPanelName, "text"),
               "None Interactive Widget"
          )
        )
      )
    )
  )
}
