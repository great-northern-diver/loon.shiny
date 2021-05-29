swap_layer_grob <- function(loon.grob, parent) {

  layers <- get_layers(loon.grob)

  if(length(layers) == 1 & all(layers == parent)) {
    loon.grob
  } else {
    lapply(layers,
           function(layer){
             if(layer != parent) {

               grobi <- grid::getGrob(loon.grob, layer)

               if(grepl(layer, pattern = "l_layer_polygon:") ||
                  grepl(layer, pattern = "l_layer_line:") ||
                  grepl(layer, pattern = "l_layer_oval:") ||
                  grepl(layer, pattern = "l_layer_text:") ||
                  grepl(layer, pattern = "l_layer_points:")) {

                 loon.grob <<- grid::setGrob(
                   gTree = loon.grob,
                   gPath = layer,
                   newGrob = editGrob(
                     grob = grobi,
                     x = grobi$y,
                     y = grobi$x
                   )
                 )
               } else if(grepl(layer, pattern = "l_layer_rectangle:")) {

                 loon.grob <<- grid::setGrob(
                   gTree = loon.grob,
                   gPath = layer,
                   newGrob = editGrob(
                     grob = grobi,
                     x = grobi$y,
                     y = grobi$x,
                     height = grobi$width,
                     width = grobi$height
                   )
                 )
               } else if(grepl(layer, pattern = "l_layer_texts:") ||
                         grepl(layer, pattern = "l_layer_polygons:") ||
                         grepl(layer, pattern = "l_layer_lines:")) {

                 loon.grob <<- setGrob(
                   gTree = loon.grob,
                   gPath = layer,
                   newGrob = gTree(
                     children = do.call(
                       gList,
                       lapply(grobi$children,
                              function(child){
                                editGrob(
                                  child,
                                  x = child$y,
                                  y = child$x
                                )
                              })
                     ),
                     name = layer,
                     gp = grobi$gp,
                     vp = grobi$vp
                   )
                 )
               } else if(grepl(layer, pattern = "l_layer_rectangles:")) {

                 loon.grob <<- setGrob(
                   gTree = loon.grob,
                   gPath = layer,
                   newGrob = gTree(
                     children = do.call(
                       gList,
                       lapply(grobi$children,
                              function(child){
                                editGrob(
                                  child,
                                  x = child$y,
                                  y = child$x,
                                  width = child$height,
                                  height = child$width
                                )
                              })
                     ),
                     name = layer,
                     gp = grobi$gp,
                     vp = grobi$vp
                   )
                 )
               } else stop("undefined layer name")
             }
           }
    )

    loon.grob
  }
}
