set_serialAxes_enclosing_grob <- function(loon.grob, pointsTreeName, glyphNames, showEnclosing, swap, whichIsDeactive){

  newGrob <- grid::getGrob(loon.grob, pointsTreeName)

  serialaxes_and_active <- setdiff(which(grepl(glyphNames, pattern = "serialaxes")), whichIsDeactive)

  if(length(serialaxes_and_active) > 0) {

    lapply(serialaxes_and_active,
           function(i) {
             serialaxes_tree <- newGrob$children[[i]]
             boundaryGrob <- grid::getGrob(serialaxes_tree, "boundary")
             if(is.null(boundaryGrob)) {
               boundaryGrob <- grid::getGrob(serialaxes_tree, "boundary: polylineGrob arguments")
               boundaryGrob_name <- "boundary: polylineGrob arguments"
             } else {
               boundaryGrob_name <- "boundary"
             }

             newGrob$children[[i]] <<- if(showEnclosing) {
               grid::setGrob(
                 gTree = newGrob$children[[i]],
                 gPath = boundaryGrob_name,
                 newGrob = do.call(
                   grid::polylineGrob,
                   args = getGrobArgs(boundaryGrob)
                 )
               )
             } else {
               grid::setGrob(
                 gTree = newGrob$children[[i]],
                 gPath = boundaryGrob_name,
                 newGrob = do.call(
                   grob,
                   args = getGrobArgs(boundaryGrob)
                 )
               )
             }


             if(swap & showEnclosing) {

               newGrob$children[[i]] <<- grid::setGrob(
                 gTree = newGrob$children[[i]],
                 gPath =  boundaryGrob_name,
                 newGrob = grid::editGrob(
                   grob = grid::getGrob(newGrob$children[[i]], boundaryGrob_name),
                   y = get_unit(boundaryGrob$x, as.numeric = FALSE) +
                     get_unit(boundaryGrob$y, is.unit = FALSE, as.numeric = FALSE),
                   x = get_unit(boundaryGrob$y, as.numeric = FALSE) +
                     get_unit(boundaryGrob$x, is.unit = FALSE, as.numeric = FALSE)
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
