tagsDivModify.l_hist <- function(loon_grob, tabPanelName, 
                                 loonWidgets_info) {
  
  path <- file.path(find.package(package = 'loon.shiny'), "images")
  
  colorList <- loonWidgets_info$colorList
  
  tags$div(
    id = paste0(tabPanelName, 'Modify'),  
    class="collapse",
    h6(""),
    h6("color:"),
    do.call(
      fixedRow,
      if(length(colorList) >= 1) {
        lapply(colorList, 
               function(col) {
                 column(
                   1,
                   actionButton(paste0(tabPanelName, col),
                                label = "",
                                style= paste(c(paste0('background-color: ', col), "height:25px"), collapse = "; ")
                   )
                 )
               }
        )
      } else {
        list(
          column(
            8,
            helpText("No color button")
          )
        )
      }
    ),
    fixedRow(
      column(
        6,
        colourpicker::colourInput(
          paste0(tabPanelName, "modify_color"),
          label = NULL,
          value = "#00BBDD",
          showColour = "background"
        )
      ),
      column(
        3,
        actionButton(paste0(tabPanelName, "color"),
                     label = "apply",
                     width = "150%",
                     style='font-size:80%; background-color: white')
      )
    ),
    
    fixedRow(
      column(
        2,
        h6("activate:")
      ),
      column(
        3,
        actionButton(
          paste0(tabPanelName, "modify_deactive"),
          label = "deactivate",
          width = "150%",
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        3,
        actionButton(
          paste0(tabPanelName, "modify_reactive"),
          label = "reactivate",
          width = "150%",
          style='font-size:80%; background-color: white'
        )
      )
    ),
    h6("")
  )
}