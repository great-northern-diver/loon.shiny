set_serialAxes_area_grob <- function(loon.grob, pointsTreeName, glyphNames, showArea, swap = NULL, whichIsDeactive) {

  newGrob <- grid::getGrob(loon.grob, pointsTreeName)

  serialaxes_and_active <- setdiff(which(grepl(glyphNames, pattern = "serialaxes")), whichIsDeactive)

  if(length(serialaxes_and_active) > 0) {

    lapply(serialaxes_and_active,
           function(i) {
             serialaxes_tree <- newGrob$children[[i]]

             serialaxesGrob <- grid::getGrob(newGrob$children[[i]], "polyline")
             if(is.null(serialaxesGrob)) {
               serialaxesGrob <- grid::getGrob(newGrob$children[[i]], "polyline: showArea")
               serialaxesGrob_name <-  "polyline: showArea"
             } else {
               serialaxesGrob_name <-  "polyline"
             }

             if(grepl(serialaxes_tree$name, pattern = "parallel")) {
               if(showArea) {
                 if(!is(serialaxesGrob, "polygon")) {
                   newGrob$children[[i]] <<- grid::setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = serialaxesGrob_name,
                     newGrob =  editGrob(
                       grob =  do.call(
                         grid::polygonGrob,
                         args = polyline2gon(Filter(Negate(is.null),
                                                    x = getGrobArgs(serialaxesGrob)))
                       ),
                       gp = gpar(
                         fill = serialaxesGrob$gp$col
                       )
                     )
                   )
                 }
               } else {
                 if(!is(serialaxesGrob, "lines")) {
                   newGrob$children[[i]] <<- grid::setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = serialaxesGrob_name,
                     newGrob = editGrob(
                       grob = do.call(
                         grid::linesGrob,
                         args = polygon2line(Filter(Negate(is.null),
                                                    x = getGrobArgs(serialaxesGrob)))
                       ),
                       gp = gpar(
                         col = serialaxesGrob$gp$fill
                       )
                     )
                   )
                 }
               }
             } else {
               # radial axes
               if(showArea) {
                 if(!is(serialaxesGrob, "polygon")) {
                   newGrob$children[[i]] <<- grid::setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = serialaxesGrob_name,
                     newGrob = editGrob(
                       grob = do.call(
                         grid::polygonGrob,
                         args = Filter(Negate(is.null),
                                       x = getGrobArgs(serialaxesGrob))
                       ),
                       gp = gpar(
                         fill = serialaxesGrob$gp$col
                       )
                     )
                   )
                 }
               } else {
                 if(!is(serialaxesGrob, "lines")) {
                   newGrob$children[[i]] <<- grid::setGrob(
                     gTree = newGrob$children[[i]],
                     gPath = serialaxesGrob_name,
                     newGrob = editGrob(
                       grob = do.call(
                         grid::linesGrob,
                         args = Filter(Negate(is.null), x = getGrobArgs(serialaxesGrob))
                       ),
                       gp = gpar(
                         col = serialaxesGrob$gp$fill
                       )
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

polygon2line <- function(grobArgs) {
  x <- grobArgs$x
  y <- grobArgs$y

  center_x <- unique(get_unit(x, "native", as.numeric = TRUE))
  xx <- get_unit(x, "native", is.unit = FALSE, as.numeric = TRUE)
  rounding_x <- xx[1:(length(xx)/2)]

  center_y <- unique(get_unit(y, "native", as.numeric = TRUE))
  yy <- get_unit(y, "native", is.unit = FALSE, as.numeric = TRUE)
  rounding_y <- yy[1:(length(yy)/2)]

  grobArgs$x <- NULL
  grobArgs$y <- NULL

  c(
    list(
      x = unit(center_x, "native") + unit(rounding_x, "mm"),
      y = unit(center_y, "native") + unit(rounding_y, "mm")
    ),
    grobArgs
  )

}

polygon2line <- function(grobArgs) {
  x <- grobArgs$x
  y <- grobArgs$y

  center_x <- unique(get_unit(x, "native", as.numeric = TRUE))
  xx <- get_unit(x, "native", is.unit = FALSE, as.numeric = TRUE)
  rounding_x <- xx[1:(length(xx)/2)]

  center_y <- unique(get_unit(y, "native", as.numeric = TRUE))
  yy <- get_unit(y, "native", is.unit = FALSE, as.numeric = TRUE)
  rounding_y <- yy[1:(length(yy)/2)]

  grobArgs$x <- NULL
  grobArgs$y <- NULL

  c(
    list(
      x = unit(center_x, "native") + unit(rounding_x, "mm"),
      y = unit(center_y, "native") + unit(rounding_y, "mm")
    ),
    grobArgs
  )
}

polyline2gon <- function(grobArgs) {
  x <- grobArgs$x
  y <- grobArgs$y

  scaleY <- serialAxes_scale()

  center_x <- unique(get_unit(x, "native", as.numeric = TRUE))
  xx <- get_unit(x, "native", is.unit = FALSE, as.numeric = TRUE)
  rounding_x <- c(xx, rev(xx))

  center_y <- unique(get_unit(y, "native", as.numeric = TRUE))
  yy <- get_unit(y, "native", is.unit = FALSE, as.numeric = TRUE)
  rounding_y <- c(yy, scaleY * rep(1,length(yy)))

  grobArgs$x <- NULL
  grobArgs$y <- NULL

  c(
    list(
      x = unit(center_x, "native") + unit(rounding_x, "mm"),
      y = unit(center_y, "native") + unit(rounding_y, "mm")
    ),
    grobArgs
  )
}

serialAxes_scale <- function() {
  -3.2
}
