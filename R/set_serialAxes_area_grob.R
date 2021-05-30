set_serialAxes_area_grob <- function(loon.grob, pointsTreeName, glyphNames, showArea, whichIsDeactive) {

  newGrob <- grid::getGrob(loon.grob, pointsTreeName)

  active <- setdiff(which(grepl(glyphNames, pattern = "serialaxes")), whichIsDeactive)

  if(length(active) > 0) {

    lapply(active,
           function(i) {
             grobi <- newGrob$children[[i]]


             serialaxesGrob <- grid::getGrob(grobi, "polyline")
             if(is.null(serialaxesGrob)) {
               serialaxesGrob <- grid::getGrob(grobi, "polyline: showArea")
               serialaxesGrobName <-  "polyline: showArea"
             } else {
               serialaxesGrobName <-  "polyline"
             }

             gp <- serialaxesGrob$gp

             if(grepl(grobi$name, pattern = "parallel")) {
               if(showArea) {
                 if(!is(serialaxesGrob, "polygon")) {

                   gp$fill <- gp$col

                   newGrob$children[[i]] <<- grid::setGrob(
                     gTree = grobi,
                     gPath = serialaxesGrobName,
                     newGrob =  grid::editGrob(
                       grob =  do.call(
                         grid::polygonGrob,
                         args = polyline2gon4parallel(Filter(Negate(is.null),
                                                      getGrobArgs(serialaxesGrob)))
                       ),
                       gp = gp
                     )
                   )
                 }
               } else {
                 if(!is(serialaxesGrob, "lines")) {

                   gp$col <- gp$fill %||% gp$col

                   newGrob$children[[i]] <<- grid::setGrob(
                     gTree = grobi,
                     gPath = serialaxesGrobName,
                     newGrob = grid::editGrob(
                       grob = do.call(
                         grid::linesGrob,
                         args = polygon2line4parallel(Filter(Negate(is.null),
                                                      getGrobArgs(serialaxesGrob)))
                       ),
                       gp = gp
                     )
                   )
                 }
               }
             } else {
               # radial axes
               if(showArea) {
                 if(!is(serialaxesGrob, "polygon")) {

                   gp$fill <- gp$col

                   newGrob$children[[i]] <<- grid::setGrob(
                     gTree = grobi,
                     gPath = serialaxesGrobName,
                     newGrob = grid::editGrob(
                       grob = do.call(
                         grid::polygonGrob,
                         args = Filter(Negate(is.null),
                                       getGrobArgs(serialaxesGrob))
                       ),
                       gp = gp
                     )
                   )
                 }
               } else {
                 if(!is(serialaxesGrob, "lines")) {

                   gp$col <- gp$fill %||% gp$col

                   newGrob$children[[i]] <<- grid::setGrob(
                     gTree = grobi,
                     gPath = serialaxesGrobName,
                     newGrob = grid::editGrob(
                       grob = do.call(
                         grid::linesGrob,
                         args = Filter(Negate(is.null),
                                       getGrobArgs(serialaxesGrob))
                       ),
                       gp = gp
                     )
                   )
                 }
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

polygon2line4parallel <- function(grobArgs) {
  x <- grobArgs$x
  y <- grobArgs$y

  centerX <- unique(get_unit(x, "native", as.numeric = TRUE))
  xx <- get_unit(x, "native", is.unit = FALSE, as.numeric = TRUE)
  roundingX <- xx[seq(length(xx)/2)]

  centerY <- unique(get_unit(y, "native", as.numeric = TRUE))
  yy <- get_unit(y, "native", is.unit = FALSE, as.numeric = TRUE)
  roundingY <- yy[seq(length(yy)/2)]

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


polyline2gon4parallel <- function(grobArgs) {
  x <- grobArgs$x
  y <- grobArgs$y

  scaleY <- serialAxes_scale()

  centerX <- unique(get_unit(x, "native", as.numeric = TRUE))
  xx <- get_unit(x, "native", is.unit = FALSE, as.numeric = TRUE)
  roundingX <- c(xx, rev(xx))

  centerY <- unique(get_unit(y, "native", as.numeric = TRUE))
  yy <- get_unit(y, "native", is.unit = FALSE, as.numeric = TRUE)
  roundingY <- c(yy, scaleY * rep(1,length(yy)))

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

serialAxes_scale <- function() {
  -3.2
}
