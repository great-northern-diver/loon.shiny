set_serialAxes_enclosing_grob <- function(loon.grob, pointsTreeName, glyphNames, showEnclosing, swap, whichIsDeactive){

  newGrob <- grid::getGrob(loon.grob, pointsTreeName)

  serialaxes_and_active <- setdiff(which(grepl(glyphNames, pattern = "serialaxes")), whichIsDeactive)

  if(length(serialaxes_and_active) > 0) {

    lapply(serialaxes_and_active,
           function(i) {
             serialaxes_tree <- newGrob$children[[i]]
             boundary_grob <- grid::getGrob(serialaxes_tree, "boundary")
             if(is.null(boundary_grob)) {
               boundary_grob <- grid::getGrob(serialaxes_tree, "boundary: polylineGrob arguments")
               boundary_grob_name <- "boundary: polylineGrob arguments"
             } else {
               boundary_grob_name <- "boundary"
             }

             newGrob$children[[i]] <<- if(showEnclosing) {
               grid::setGrob(
                 gTree = newGrob$children[[i]],
                 gPath = boundary_grob_name,
                 newGrob = do.call(
                   grid::polylineGrob,
                   args = getGrobArgs(boundary_grob)
                 )
               )
             } else {
               grid::setGrob(
                 gTree = newGrob$children[[i]],
                 gPath = boundary_grob_name,
                 newGrob = do.call(
                   grob,
                   args = getGrobArgs(boundary_grob)
                 )
               )
             }


             if(swap & showEnclosing) {

               newGrob$children[[i]] <<- grid::setGrob(
                 gTree = newGrob$children[[i]],
                 gPath =  boundary_grob_name,
                 newGrob = editGrob(
                   grob = grid::getGrob(newGrob$children[[i]], boundary_grob_name),
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

  grid::setGrob(
    gTree = loon.grob,
    gPath = pointsTreeName,
    newGrob = newGrob
  )
}
