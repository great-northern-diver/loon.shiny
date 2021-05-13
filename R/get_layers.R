get_layers <- function(loon_grob) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("get_layers", obj)
}

get_layers.default <- function(loon_grob) NULL

get_layers.l_plot <- function(loon_grob) {

  l_plot_layers <- grid::getGrob(loon_grob, "l_plot_layers")

  layers_name <- l_plot_layers$childrenOrder
  layers <- l_plot_layers$children

  loon_layers <- rev(
    unlist(
      lapply(layers,
             function(layer) {
               if(stringr::str_detect(grobName(layer), "null")) {
                 NA
               } else {
                 if(stringr::str_detect(layer$name, "l_layer_group")) {
                   get_group_children(layer)
                 } else layer$name
               }
             }
      )
    )
  )

  unname(loon_layers[which(!is.na(loon_layers))])
}

get_layers.l_plot3D <- function(loon_grob) {
  get_layers.l_plot(loon_grob)
}

get_layers.l_hist <- function(loon_grob) {

  l_hist_layers <- grid::getGrob(loon_grob, "l_hist_layers")

  layers_name <- l_hist_layers$childrenOrder
  layers <- l_hist_layers$children

  loon_layers <- rev(
    unlist(
      lapply(layers,
             function(layer) {
               if(stringr::str_detect(grobName(layer), "null")) {
                 NA
               } else {
                 if(stringr::str_detect(layer$name, "l_layer_group")) {
                   get_group_children(layer)
                 } else layer$name
               }
             }
      )
    )
  )

  unname(loon_layers[which(!is.na(loon_layers))])
}

get_layers.l_graph <- function(loon_grob) {

  l_graph_layers <- grid::getGrob(loon_grob, "l_graph_layers")

  layers_name <- l_graph_layers$childrenOrder
  layers <- l_graph_layers$children

  loon_layers <- rev(
    unlist(
      lapply(layers,
             function(layer) {
               if(stringr::str_detect(grobName(layer), "null")) {
                 NA
               } else {
                 if(stringr::str_detect(layer$name, "l_layer_group")) {
                   get_group_children(layer)
                 } else layer$name
               }
             }
      )
    )
  )

  unname(loon_layers[which(!is.na(loon_layers))])
}

get_group_children <- function(layer) {

  children <- layer$children

  if(length(children) != 0) {
    unlist(
      lapply(1:length(children),
             function(i) {
               child <- children[[i]]
               if(stringr::str_detect(child$name, "l_layer_group")) {
                 get_group_children(child)
               } else child$name
             }
      )
    )
  } else "l_layer_group"
}
