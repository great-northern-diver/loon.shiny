swap_layer_grob <- function(loon_grob, parent) {

  layers <- get_layers(loon_grob)

  if(length(layers) == 1 & all(layers == parent)) {
    loon_grob
  } else {
    lapply(layers,
           function(layer){
             if(layer != parent) {

               grobi <- getGrob(loon_grob, layer)

               if(str_detect(layer, "l_layer_polygon:") | str_detect(layer, "l_layer_line:") | str_detect(layer, "l_layer_oval:") | str_detect(layer, "l_layer_text:") | str_detect(layer, "l_layer_points:")) {

                 loon_grob <<- setGrob(
                   gTree = loon_grob,
                   gPath = layer,
                   newGrob = editGrob(
                     grob = grobi,
                     x = grobi$y,
                     y = grobi$x
                   )
                 )
               } else if(str_detect(layer, "l_layer_rectangle:")) {

                 loon_grob <<- setGrob(
                   gTree = loon_grob,
                   gPath = layer,
                   newGrob = editGrob(
                     grob = grobi,
                     x = grobi$y,
                     y = grobi$x,
                     height = grobi$width,
                     width = grobi$height
                   )
                 )
               } else if(str_detect(layer, "l_layer_texts:") | str_detect(layer, "l_layer_polygons:") | str_detect(layer, "l_layer_lines:")) {

                 loon_grob <<- setGrob(
                   gTree = loon_grob,
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
               } else if(str_detect(layer, "l_layer_rectangles:")) {

                 loon_grob <<- setGrob(
                   gTree = loon_grob,
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

    loon_grob
  }
}
