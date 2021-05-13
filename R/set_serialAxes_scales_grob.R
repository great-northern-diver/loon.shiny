set_serialAxes_scales_grob <- function(loon_grob, pointsTree_name, glyph_name, set_axes, swap, which_is_deactive) {
  
  newGrob <- getGrob(loon_grob, pointsTree_name)
  
  serialaxes_and_active <- setdiff(which(str_detect(glyph_name, "serialaxes")), which_is_deactive)
  
  if(length(serialaxes_and_active) > 0) {
    
    lapply(serialaxes_and_active,
           function(i) {
             serialaxes_tree <- newGrob$children[[i]]
             
             # radial serialaxes
             axes_grob <- getGrob(serialaxes_tree, "axes")
             if(is.null(axes_grob)) {
               axes_grob <- getGrob(serialaxes_tree, "axes: polylineGrob arguments")
               axes_grob_name <- "axes: polylineGrob arguments"
             } else {
               axes_grob_name <- "axes"
             }
             
             newGrob$children[[i]] <<- if(set_axes) {
               setGrob(
                 gTree = newGrob$children[[i]],
                 gPath = axes_grob_name,
                 newGrob = do.call(
                   polylineGrob,
                   args = getGrobArgs(axes_grob)
                 )
               )
             } else {
               setGrob(
                 gTree = newGrob$children[[i]],
                 gPath = axes_grob_name,
                 newGrob = do.call(
                   grob,
                   args = getGrobArgs(axes_grob)
                 )
               )
             }
             
             if(swap & set_axes) {
               
               newGrob$children[[i]] <<- setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = axes_grob_name,
                   newGrob = editGrob(
                     grob = getGrob(newGrob$children[[i]], axes_grob_name),
                     y = get_unit(axes_grob$x, as.numeric = FALSE) + 
                       get_unit(axes_grob$y, is.unit = FALSE, as.numeric = FALSE),
                     x =  get_unit(axes_grob$y, as.numeric = FALSE) + 
                       get_unit(axes_grob$x, is.unit = FALSE, as.numeric = FALSE)
                   )
                 )
             }
           }
    )
    
    
  } else NULL # serialaxes glyphs are all deactive; no changes are necessary
  
  setGrob(
    gTree = loon_grob,
    gPath = pointsTree_name,
    newGrob = newGrob
  )
}
