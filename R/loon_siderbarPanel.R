# return tabPanel with corresponding sidebarPanel
loon_sidebarPanel <- function(loon_grob, tabPanelName, selectBy, 
                              linkingGroup, linkingGroups, loonWidgets_info, 
                              showWorldView) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("loon_sidebarPanel", obj)
}

loon_sidebarPanel.default <- function(loon_grob, tabPanelName, selectBy, 
                                      linkingGroup, linkingGroups, loonWidgets_info, 
                                      showWorldView) {
  shiny::tabPanel(
    title = tabPanelName,
    shiny::fixedRow(
      shiny::column(
        6,
        do.call(
          shiny::helpText,
          list(inputId = paste0(tabPanelName, "text"),
               "Not interactive widget"
          )
        )
      )
    )
  )
}