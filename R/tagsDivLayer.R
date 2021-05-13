tagsDivLayer <- function(loon_grob, tabPanelName, 
                         loonWidgets_info) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("tagsDivLayer", obj)
}

tagsDivLayer.default <- function(loon_grob, tabPanelName, 
                                 loonWidgets_info) {
  
  path <- file.path(find.package(package = 'loon.shiny'), "images")
  
  colorList <- loonWidgets_info$colorList
  tags$div(
    id = paste0(tabPanelName, 'Layer'),  
    class="collapse",
    h6(""),
    fixedRow(
      column(
        3,
        h5("layers:")
      ),
      column(
        9,
        selectInput(inputId = paste0(tabPanelName, "layer"),
                    label = NULL,
                    choices = loonWidgets_info$layers,
                    selected = NULL, 
                    multiple = FALSE,
                    selectize = TRUE)
      )
    ),
    fixedRow(
      column(
        2,
        actionButton(
          paste0(tabPanelName, "layer_up"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/up.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "layer_down"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/down.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "layer_visible"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/visible.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "layer_invisible"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/invisible.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "layer_plus"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/plus.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      )
    ), 
    fixedRow(
      column(
        2,
        actionButton(
          paste0(tabPanelName, "layer_minus"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/minus.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "layer_scale_to"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/scaleto.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      )
    ),
    fixedRow(
      column(
        4,
        helpText("label name:")
      ),
      column(
        5,
        textInput(
          paste0(tabPanelName, "layer_changed_label"),
          label = NULL,
          value = "",
          width = "150%"
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "layer_set"),
          label = "set",
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      )
    )
  )
}