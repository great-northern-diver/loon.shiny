set_serialAxes_enclosing_grob <- function(loon_grob, pointsTree_name, glyph_name, set_boundary, swap, which_is_deactive){

  newGrob <- getGrob(loon_grob, pointsTree_name)

  serialaxes_and_active <- setdiff(which(str_detect(glyph_name, "serialaxes")), which_is_deactive)
  
  if(length(serialaxes_and_active) > 0) {

    lapply(serialaxes_and_active,
           function(i) {
             serialaxes_tree <- newGrob$children[[i]]
             boundary_grob <- getGrob(serialaxes_tree, "boundary")
             if(is.null(boundary_grob)) {
               boundary_grob <- getGrob(serialaxes_tree, "boundary: polylineGrob arguments")
               boundary_grob_name <- "boundary: polylineGrob arguments"
             } else {
               boundary_grob_name <- "boundary"
             }
             
             newGrob$children[[i]] <<- if(set_boundary) {
               setGrob(
                 gTree = newGrob$children[[i]],
                 gPath = boundary_grob_name,
                 newGrob = do.call(
                   polylineGrob,
                   args = getGrobArgs(boundary_grob)
                 )
               )
             } else {
               setGrob(
                 gTree = newGrob$children[[i]],
                 gPath = boundary_grob_name,
                 newGrob = do.call(
                   grob,
                   args = getGrobArgs(boundary_grob)
                 )
               )
             }


             if(swap & set_boundary) {

               newGrob$children[[i]] <<- setGrob(
                 gTree = newGrob$children[[i]],
                 gPath =  boundary_grob_name,
                 newGrob = editGrob(
                   grob = getGrob(newGrob$children[[i]], boundary_grob_name),
                   y = get_unit(boundary_grob$x, as.numeric = FALSE) + 
                     get_unit(boundary_grob$y, is.unit = FALSE, as.numeric = FALSE),
                   x = get_unit(boundary_grob$y, as.numeric = FALSE) + 
                     get_unit(boundary_grob$x, is.unit = FALSE, as.numeric = FALSE)
                 )
               )
             }
           }
    )

  } else NULL

  setGrob(
    gTree = loon_grob,
    gPath = pointsTree_name,
    newGrob = newGrob
  )
}
