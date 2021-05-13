move_layer_up_grob <- function(loon_grob, current_layer, parent) {

  parent_layer <- getGrob(loon_grob, parent)
  layers_name <- parent_layer$childrenOrder

  if(current_layer %in% layers_name) {

    layer_order <- which(layers_name %in% current_layer)
    len_layers_name <- length(layers_name)

  } else {

    parent <- getParent(gTree = parent_layer, current_layer = current_layer)
    parent_layer <- getGrob(parent_layer, parent)

    layers_name <- parent_layer$childrenOrder
    layer_order <- which(layers_name %in% current_layer)
    len_layers_name <- length(layers_name)
  }

  if(layer_order == len_layers_name) {

    warning(paste("Warning: layer" , current_layer, "cannot be raised any further."))
    loon_grob
  } else {

    setGrob(
      gTree = loon_grob,
      gPath = parent,
      newGrob = reorderGrob(
        parent_layer,
        order = layer_reorder(layer_order, len_layers_name, way = "up")
      )
    )
  }
}

layer_reorder <- function(layer_order, len_layers_name, way) {

  if(way == "up") {

    if(layer_order < len_layers_name) {
      sequence <- seq(len_layers_name)
      sequence[layer_order] <- layer_order + 1
      sequence[layer_order+1] <- layer_order
    } else warnings("layer order is larger or equal to the length of layers")
  } else if(way == "down") {

    if(layer_order > 1) {
      sequence <- seq(len_layers_name)
      sequence[layer_order] <- layer_order - 1
      sequence[layer_order-1] <- layer_order
    } else warnings("layer order is smaller or equal to 1")
  }

  sequence
}

getParent <- function(gTree, current_layer) {

  if(str_detect(grobName(gTree), 'gTree')) {

    layers_name <- gTree$childrenOrder
    layers <- gTree$children

    for(i in 1:length(layers_name)) {
      layer <- layers[[i]]

      if(current_layer %in% layer$childrenOrder) {
        return(layer$name)
        break
      } else getParent(layer, current_layer)
    }
  }
}
