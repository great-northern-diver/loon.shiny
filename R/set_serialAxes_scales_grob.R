set_serialAxes_scales_grob <- function(loon.grob, pointsTreeName,
                                       glyphNames, showAxes,
                                       swap, whichIsDeactive) {

  newGrob <- grid::getGrob(loon.grob, pointsTreeName)

  serialaxes_and_active <- setdiff(which(grepl(glyphNames, pattern = "serialaxes")), whichIsDeactive)

  if(length(serialaxes_and_active) > 0) {

    lapply(serialaxes_and_active,
           function(i) {
             serialaxes_tree <- newGrob$children[[i]]

             # radial serialaxes
             axesGrob <- grid::getGrob(serialaxes_tree, "axes")
             if(is.null(axesGrob)) {
               axesGrob <- grid::getGrob(serialaxes_tree, "axes: polylineGrob arguments")
               axesGrob_name <- "axes: polylineGrob arguments"
             } else {
               axesGrob_name <- "axes"
             }

             newGrob$children[[i]] <<- if(showAxes) {
               grid::setGrob(
                 gTree = newGrob$children[[i]],
                 gPath = axesGrob_name,
                 newGrob = do.call(
                   grid::polylineGrob,
                   args = getGrobArgs(axesGrob)
                 )
               )
             } else {
               grid::setGrob(
                 gTree = newGrob$children[[i]],
                 gPath = axesGrob_name,
                 newGrob = do.call(
                   grob,
                   args = getGrobArgs(axesGrob)
                 )
               )
             }

             if(swap & showAxes) {

               newGrob$children[[i]] <<- grid::setGrob(
                   gTree = newGrob$children[[i]],
                   gPath = axesGrob_name,
                   newGrob = editGrob(
                     grob = grid::getGrob(newGrob$children[[i]], axesGrob_name),
                     y = get_unit(axesGrob$x, as.numeric = FALSE) +
                       get_unit(axesGrob$y, is.unit = FALSE, as.numeric = FALSE),
                     x =  get_unit(axesGrob$y, as.numeric = FALSE) +
                       get_unit(axesGrob$x, is.unit = FALSE, as.numeric = FALSE)
                   )
                 )
             }
           }
    )


  } else NULL # serialaxes glyphs are all deactive; no changes are necessary

  grid::setGrob(
    gTree = loon.grob,
    gPath = pointsTreeName,
    newGrob = newGrob
  )
}
