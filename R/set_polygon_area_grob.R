set_polygon_area_grob <- function(loon.grob, pointsTreeName, glyphNames, showArea, whichIsDeactive) {

  newGrob <- grid::getGrob(loon.grob, pointsTreeName)

  active <- setdiff(which(grepl(glyphNames, pattern = "polygon")), whichIsDeactive)

  if(length(active) > 0) {

    lapply(active,
           function(i) {

             grobi <- newGrob$children[[i]]
             gp <- grobi$gp

             if(showArea) {

               # line to polygon
               if(!is(grobi, "polygon")) {

                 gp$fill <- gp$col

                 newGrob$children[[i]] <<- grid::editGrob(
                   grob =  do.call(
                     grid::polygonGrob,
                     args = Filter(Negate(is.null), getGrobArgs(grobi))
                   ),
                   gp = gp
                 )
               }

             } else {

               # polygon to line
               if(!is(grobi, "lines")) {

                 gp$col <- gp$fill %||% gp$col

                 newGrob$children[[i]] <<- grid::editGrob(
                   grob = do.call(
                     grid::linesGrob,
                     args =  polygon2line(Filter(Negate(is.null),
                                                 getGrobArgs(grobi)))
                   ),
                   gp = gp
                 )
               }
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

polygon2line <- function(grobArgs) {

  x <- grobArgs$x
  y <- grobArgs$y

  centerX <- unique(get_unit(x, "native", as.numeric = TRUE))
  xx <- get_unit(x, "native", is.unit = FALSE, as.numeric = TRUE)
  roundingX <- c(xx, xx[1])

  centerY <- unique(get_unit(y, "native", as.numeric = TRUE))
  yy <- get_unit(y, "native", is.unit = FALSE, as.numeric = TRUE)
  roundingY <- c(yy, yy[1])

  grobArgs$x <- NULL
  grobArgs$y <- NULL

  c(
    list(
      x = unit(centerX, "native") + unit(roundingX, "mm"),
      y = unit(centerY, "native") + unit(roundingY, "mm")
    ),
    grobArgs
  )

}

