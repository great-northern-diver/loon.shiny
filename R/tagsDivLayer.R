tagsDivLayer <- function(loon.grob, tabPanelName,
                         loonWidgetsInfo, displayedPanel) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("tagsDivLayer", obj)
}

tagsDivLayer.default <- function(loon.grob, tabPanelName,
                                 loonWidgetsInfo, displayedPanel) {

  path <- file.path(find.package(package = 'loon.shiny'), "images")

  # It is mainly for the test
  # In `devtools::test`, it is looking for the Github source
  # not the library source. Hence, the path is different
  tryCatch(
    suppressWarnings(
      base64enc::dataURI(file=paste0(path, "/LoonIcon.png"), mime="image/png")
    ),
    error = function(e) {
      path <<- file.path(find.package(package = 'loon.shiny'), "inst/images")
    }
  )

  do.call(tags$div,
          remove_null(
            list(
              id = paste0(tabPanelName, 'Layer'),
              class = if(any(grepl("layer", displayedPanel, ignore.case = TRUE))) NULL else "collapse",
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
                              choices = loonWidgetsInfo$layers,
                              selected = NULL,
                              multiple = FALSE,
                              selectize = TRUE)
                )
              ),
              fixedRow(
                column(
                  2,
                  actionButton(
                    paste0(tabPanelName, "layerUp"),
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
                    paste0(tabPanelName, "layerDown"),
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
                    paste0(tabPanelName, "layerVisible"),
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
                    paste0(tabPanelName, "layerInvisible"),
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
                    paste0(tabPanelName, "layerPlus"),
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
                    paste0(tabPanelName, "layerMinus"),
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
                    paste0(tabPanelName, "scaleToLayer"),
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
                    paste0(tabPanelName, "newLayerLabel"),
                    label = NULL,
                    value = "",
                    width = "150%"
                  )
                ),
                column(
                  2,
                  actionButton(
                    paste0(tabPanelName, "layerSet"),
                    label = "set",
                    width = '150%',
                    style='font-size:80%; background-color: white'
                  )
                )
              )
            ), as_list = FALSE)
  )
}
