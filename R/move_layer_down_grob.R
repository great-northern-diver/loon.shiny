move_layer_down_grob <- function(loon_grob, current_layer, parent) {

  parent_layer <- getGrob(loon_grob, parent)
  layers_name <- parent_layer$childrenOrder

  if(current_layer %in% layers_name) {

    layer_order <- which(layers_name %in% current_layer)
  } else {

    parent <- getParent(gTree = parent_layer, current_layer = current_layer)
    parent_layer <- getGrob(parent_layer, parent)

    layers_name <- parent_layer$childrenOrder
    layer_order <- which(layers_name %in% current_layer)
  }

  if(layer_order == 1) {

    warning(paste("Warning: layer" , current_layer, "cannot be lowered any further."))
    loon_grob
  } else {

    setGrob(
      gTree = loon_grob,
      gPath = parent,
      newGrob = reorderGrob(
        parent_layer,
        order = layer_reorder(layer_order, len_layers_name = length(layers_name), way = "down")
      )
    )
  }
}
