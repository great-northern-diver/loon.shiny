get_showGuides <- function(loon.grob) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("get_showGuides", obj)
}

get_showGuides.default <- function(loon.grob) {
  guidesGrob <- grid::getGrob(loon.grob, "guides")
  !all(grepl(guidesGrob$childrenOrder, pattern = ":"))
}

get_showGuides.l_serialaxes <- function(loon.grob) {
  guidesGrob <- grid::getGrob(loon.grob, "guides")
  "bounding box" %in% guidesGrob$childrenOrder
}

set_guidesGrob <- function(loon.grob, xaxis, yaxis, loonColor) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_guidesGrob", obj)
}

set_guidesGrob.default <- function(loon.grob, xaxis, yaxis, loonColor) {

  grid::setGrob(
    gTree = loon.grob,
    gPath = "guides",
    newGrob = gTree(
      children = gList(
        grid::rectGrob(gp = gpar(col = NA,
                                 fill = loonColor$guidesbackground_color)
        ),
        do.call(
          gList,
          lapply(xaxis,
                 function(xax) {
                   grid::linesGrob(x = unit(rep(xax, 2), "native"),
                                   y =  unit(c(0, 1), "npc"),
                                   gp = gpar(col = loonColor$guideslines_color, lwd = 2))

                 }
          )
        ),
        do.call(
          gList,
          lapply(yaxis,
                 function(yax) {
                   grid::linesGrob(x = unit(c(0, 1), "npc") ,
                                   y =  unit(rep(yax,2), "native"),
                                   gp = gpar(col = loonColor$guideslines_color, lwd = 2))

                 }
          )
        )
      ),
      name = "guides"
    )
  )
}
