set_boundary_grob <- function(loon_grob, margins, loon_color) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("set_boundary_grob", obj)
}

set_boundary_grob.default <- function(loon_grob, margins, loon_color) {

  gPath <- if(!is.null(getGrob(loon_grob, "boundary rectangle"))) {
    "boundary rectangle"
  }  else "boundary rectangle: rectGrob arguments"

  # reset boundary
  setGrob(
    gTree = loon_grob,
    gPath = gPath,
    newGrob = if(sum(margins) > 0) {
      rectGrob(name = gPath,
               gp=gpar(col = loon_color$foreground_color[1],
                       fill = NA,
                       lwd=1)
      )
    } else {
      nullGrob(name = gPath)
    }
  )
}
