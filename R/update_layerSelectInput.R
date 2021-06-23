update_layerSelectInput <- function(session, buttons, tabPanelName, layers, input) {

  layerSet <- input[[paste0(tabPanelName, "layerSet")]]
  if(layerSet > buttons["layerSet"]) {

    newLayerLabel <- shiny::isolate(input[[paste0(tabPanelName, "newLayerLabel")]])

    if(newLayerLabel != "") {

      layersName <- names(layers)
      currentLayerName <- input[[paste0(tabPanelName, "layer")]]

      layersName[which(layersName == currentLayerName)] <- newLayerLabel

      shiny::updateSelectInput(
        session,
        inputId = paste0(tabPanelName, "layer"),
        label = NULL,
        choices = layersName,
        selected = newLayerLabel
      )
    }
  }

  layerMinus <- input[[paste0(tabPanelName, "layerMinus")]]
  if(layerMinus > buttons["layerMinus"]) {

    layersName <- names(layers)
    currentLayerName <- input[[paste0(tabPanelName, "layer")]]
    newLayersName <- setdiff(layersName, currentLayerName)

    shiny::updateSelectInput(
      session,
      inputId = paste0(tabPanelName, "layer"),
      choices = newLayersName
    )
  }
}
