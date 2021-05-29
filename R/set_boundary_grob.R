set_boundaryGrob <- function(loon.grob, margins, loonColor) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_boundaryGrob", obj)
}

set_boundaryGrob.default <- function(loon.grob, margins, loonColor) {

  gPath <- if(!is.null(grid::getGrob(loon.grob, "boundary rectangle"))) {
    "boundary rectangle"
  }  else "boundary rectangle: rectGrob arguments"

  # reset boundary
  grid::setGrob(
    gTree = loon.grob,
    gPath = gPath,
    newGrob = if(sum(margins) > 0) {
      grid::rectGrob(name = gPath,
               gp=gpar(col = loonColor$foreground_color[1],
                       fill = NA,
                       lwd=1)
      )
    } else {
      nullGrob(name = gPath)
    }
  )
}
