update_layerSelectInput <- function(session, buttons, tabPanelName, layers, input) {

  layerSet <- input[[paste0(tabPanelName, "layerSet")]]

  if(layerSet > buttons["layerSet"]) {

    newLayerLabel <- shiny::isolate(input[[paste0(tabPanelName, "newLayerLabel")]])

    if(newLayerLabel != "") {

      layersName <- names(layers)
      currentLayer <- input[[paste0(tabPanelName, "layer")]]

      layersName[which(layersName == currentLayer)] <- newLayerLabel

      shiny::updateSelectInput(
        session,
        inputId = paste0(tabPanelName, "layer"),
        label = NULL,
        choices = layersName,
        selected = newLayerLabel
      )
    }
  }
}
