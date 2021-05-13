tagsDivPlot.l_serialaxes <- function(loon_grob, tabPanelName, 
                                    loonWidgets_info, linkingGroup, linkingGroups) {
  tags$div(
    id = paste0(tabPanelName, 'Plot'),  
    class="collapse",
    h6(""),
    checkboxGroupInput(
      inputId = paste0(tabPanelName, "plot"),
      label = NULL,
      choices = c("showGuides", "showAxes", "showAxesLabels", "showLabels", "showArea", "andrews"),
      selected = c(
        if(loonWidgets_info$showGuides) "showGuides",
        if(loonWidgets_info$showAxes) "showAxes",
        if(loonWidgets_info$showAxesLabels) "showAxesLabels",
        if(loonWidgets_info$showLabels) "showLabels",
        if(loonWidgets_info$showArea) "showArea",
        if(loonWidgets_info$andrews) "andrews"
      )
    ),
    radioButtons(
      inputId = paste0(tabPanelName, "axesLayout"),
      label = "axes layout",
      choices = c("radial", "parallel"),
      inline = TRUE,
      selected = loonWidgets_info$axesLayout
    ),
    radioButtons(
      inputId = paste0(tabPanelName, "scaling"),
      label = "scaling",
      choices = c("variable", "observation", "data", "none"),
      selected = loonWidgets_info$scaling
    )
  )
}