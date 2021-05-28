move_layerUp_grob <- function(loon.grob, currentLayer, parent) {

  parent_layer <- grid::getGrob(loon.grob, parent)
  layersName <- parent_layer$childrenOrder

  if(currentLayer %in% layersName) {

    layer_order <- which(layersName %in% currentLayer)
    len_layersName <- length(layersName)

  } else {

    parent <- getParent(gTree = parent_layer, currentLayer = currentLayer)
    parent_layer <- grid::getGrob(parent_layer, parent)

    layersName <- parent_layer$childrenOrder
    layer_order <- which(layersName %in% currentLayer)
    len_layersName <- length(layersName)
  }

  if(layer_order == len_layersName) {

    warning(paste("Warning: layer" , currentLayer, "cannot be raised any further."))
    loon.grob
  } else {

    grid::setGrob(
      gTree = loon.grob,
      gPath = parent,
      newGrob = reorderGrob(
        parent_layer,
        order = layer_reorder(layer_order, len_layersName, way = "up")
      )
    )
  }
}

layer_reorder <- function(layer_order, len_layersName, way) {

  if(way == "up") {

    if(layer_order < len_layersName) {
      sequence <- seq(len_layersName)
      sequence[layer_order] <- layer_order + 1
      sequence[layer_order+1] <- layer_order
    } else warnings("layer order is larger or equal to the length of layers")
  } else if(way == "down") {

    if(layer_order > 1) {
      sequence <- seq(len_layersName)
      sequence[layer_order] <- layer_order - 1
      sequence[layer_order-1] <- layer_order
    } else warnings("layer order is smaller or equal to 1")
  }

  sequence
}

getParent <- function(gTree, currentLayer) {

  if(grepl(grobName(gTree), pattern = 'gTree')) {

    layersName <- gTree$childrenOrder
    layers <- gTree$children

    for(i in 1:length(layersName)) {
      layer <- layers[[i]]

      if(currentLayer %in% layer$childrenOrder) {
        return(layer$name)
        break
      } else getParent(layer, currentLayer)
    }
  }
}
