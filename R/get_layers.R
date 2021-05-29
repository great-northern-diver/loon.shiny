get_layers <- function(loon.grob) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("get_layers", obj)
}

get_layers.default <- function(loon.grob) NULL

get_layers.l_plot <- function(loon.grob) {

  get_loon_layers(loon.grob,
                  "l_plot_layers")
}

get_layers.l_plot3D <- function(loon.grob) {
  get_layers.l_plot(loon.grob)
}

get_layers.l_hist <- function(loon.grob) {

  get_loon_layers(loon.grob,
                  "l_hist_layers")
}

get_layers.l_graph <- function(loon.grob) {

  layers <- get_loon_layers(loon.grob,
                            "l_graph_layers")

  layers <- lapply(layers,
                   function(layer) {
                     if(layer == "graph") {
                       get_group_children(grid::getGrob(loon.grob, layer),
                                          go = TRUE)
                     } else layer
                   })

  unlist(layers)
}

get_loon_layers <- function(loon.grob, gPath) {

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
                   get_group_children(layer)
                 } else layer$name
               }
             }
      )
    )
  )

  unname(loon_layers[which(!is.na(loon_layers))])
}

get_group_children <- function(layer, go = FALSE) {

  children <- layer$children

  if(length(children) > 0) {
    unlist(
      lapply(seq(length(children)),
             function(i) {
               child <- children[[i]]
               if(grepl(child$name, pattern = "l_layer_group")) {
                 get_group_children(child, go)
               } else {

                 if(go) {
                   if(isgTree(child))
                     get_group_children(child, go)
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
