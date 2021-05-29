move_layerDown_grob <- function(loon.grob, currentLayer, parent) {

  parent_layer <- grid::getGrob(loon.grob, parent)
  layersName <- parent_layer$childrenOrder

  if(currentLayer %in% layersName) {

    layer_order <- which(layersName %in% currentLayer)
  } else {

    parent <- getParent(gTree = parent_layer, currentLayer = currentLayer)
    parent_layer <- grid::getGrob(parent_layer, parent)

    layersName <- parent_layer$childrenOrder
    layer_order <- which(layersName %in% currentLayer)
  }

  if(layer_order == 1) {

    warning(paste("Warning: layer" , currentLayer, "cannot be lowered any further."))
    loon.grob
  } else {

    grid::setGrob(
      gTree = loon.grob,
      gPath = parent,
      newGrob = reorderGrob(
        parent_layer,
        order = layer_reorder(layer_order, len_layersName = length(layersName), way = "down")
      )
    )
  }
}
