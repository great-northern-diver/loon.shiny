set_pointrange_grob <- function(loon.grob, pointsTreeName, glyphNames,
                                showArea, color, whichIsDeactive) {

  newGrob <- grid::getGrob(loon.grob, pointsTreeName)

  active <- setdiff(which(grepl(glyphNames, pattern = "pointrange")), whichIsDeactive)

  if(length(active) > 0) {

    lapply(active,
           function(i) {

             grobi <- newGrob$children[[i]]

             pointGrob <- grid::getGrob(grobi, "point")
             gp <- pointGrob$gp

             if(showArea) {

               pointGrob$pch <- 1
               gp$col <- color[i]
               gp$fill <- NA

             } else {

               pointGrob$pch <- 21
               gp$fill <- color[i]
               gp$col <- bounder_color()

             }

             newGrob$children[[i]] <<- grid::setGrob(
               gTree = grobi,
               gPath = "point",
               newGrob = grid::editGrob(
                 grob = pointGrob,
                 gp = gp
               )
             )
           }
    )

  } else NULL

  grid::setGrob(
    gTree = loon.grob,
    gPath = pointsTreeName,
    newGrob = newGrob
  )
}
