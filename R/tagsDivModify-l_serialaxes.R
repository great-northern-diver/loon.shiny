tagsDivModify.l_serialaxes <- function(loon_grob, tabPanelName, 
                                       loonWidgets_info) {
  
  path <- file.path(find.package(package = 'loon.shiny'), "images")
  
  colorList <- loonWidgets_info$colorList
  tags$div(
    id = paste0(tabPanelName, 'Modify'),  
    class="collapse",
    h6(""),
    fixedRow(
      column(
        3,
        h6("activate:")
      ),
      column(
        3,
        actionButton(
          paste0(tabPanelName, "modify_deactive"),
          label = "deactivate",
          width = '130%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        3,
        actionButton(
          paste0(tabPanelName, "modify_reactive"),
          label = "reactivate",
          width = '130%',
          style='font-size:80%; background-color: white'
        )
      )
    ),
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
            helpText("No color button"),
            offset = 2
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
                     width = "130%",
                     style='font-size:80%; background-color: white')
      )
    ),
    fixedRow(
      column(
        3,
        h6("linewidth:")
      ),
      column(
        3,
        h6("abs to")
      ),
      column(
        3,
        actionButton(
          paste0(tabPanelName, "abs_to_plus"),
          icon("plus"),
          width = "130%",
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        3,
        actionButton(
          paste0(tabPanelName, "abs_to_minus"),
          icon("minus"),
          width = "130%",
          style='font-size:80%; background-color: white'
        )
      )
    ),
    fixedRow(
      column(
        3,
        h6("rel to"),
        offset = 3
      ),
      column(
        3,
        actionButton(
          paste0(tabPanelName, "rel_to_plus"),
          icon("plus"),
          width = "130%",
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        3,
        actionButton(
          paste0(tabPanelName, "rel_to_minus"),
          icon("minus"),
          width = "130%",
          style='font-size:80%; background-color: white'
        )
      )
    ),
    h6("")
  )
}