get_layers <- function(loon.grob, recursive = FALSE) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("get_layers", obj)
}

get_layers.default <- function(loon.grob, recursive = FALSE) NULL

get_layers.l_plot <- function(loon.grob, recursive = FALSE) {

  get_loon_layers(loon.grob, "l_plot_layers", recursive = recursive)
}

get_layers.l_plot3D <- function(loon.grob, recursive = FALSE) {
  get_layers.l_plot(loon.grob, recursive = recursive)
}

get_layers.l_hist <- function(loon.grob, recursive = FALSE) {

  get_loon_layers(loon.grob, "l_hist_layers", recursive = recursive)
}

get_layers.l_graph <- function(loon.grob, recursive = FALSE) {

  get_loon_layers(loon.grob, "l_graph_layers", recursive = recursive)
}

get_loon_layers <- function(loon.grob, gPath, recursive = FALSE) {

  x <- grid::getGrob(loon.grob, gPath)
  layers <- x$children

  loon_layers <- rev(
    unlist(
      lapply(layers,
             function(layer) {
               if(grepl(grobName(layer), pattern = "null")) {
                 NA
               } else {
                 if(grepl(layer$name, pattern = "l_layer_group")) {
                   get_group_children(layer, recursive = recursive)
                 } else {
                   if(recursive)
                     get_group_children(layer, recursive = recursive)
                   else
                     layer$name
                 }
               }
             }
      )
    )
  )

  unname(loon_layers[which(!is.na(loon_layers))])
}

get_group_children <- function(layer, recursive = FALSE) {

  children <- layer$children

  if(length(children) > 0) {
    unlist(
      lapply(seq(length(children)),
             function(i) {
               child <- children[[i]]
               if(grepl(child$name, pattern = "l_layer_group")) {
                 get_group_children(child, recursive)
               } else {

                 if(recursive) {
                   if(isgTree(child))
                     get_group_children(child, recursive)
                   else
                     child$name
                 } else {
                   child$name
                 }
               }
             }
      )
    )
  } else layer$name
}
