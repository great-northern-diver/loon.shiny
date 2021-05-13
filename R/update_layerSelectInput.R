update_layerSelectInput <- function(session, buttons, tabPanelName, layers, input) {
  
  if(buttons$layer_button$set != 0) {
    
    new_layer_label <- shiny::isolate(input[[paste0(tabPanelName, "layer_changed_label")]])
    
    if(new_layer_label != "") {

      layers_name <- names(layers)
      current_layer <- input[[paste0(tabPanelName, "layer")]]
      
      layers_name[which(layers_name == current_layer)] <- new_layer_label
      
      shiny::updateSelectInput(
        session,
        inputId = paste0(tabPanelName, "layer"),
        label = NULL,
        choices = layers_name,
        selected = new_layer_label
      )
    }
  }
}