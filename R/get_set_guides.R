get_showGuides <- function(loon_grob) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("get_showGuides", obj)
}

get_showGuides.default <- function(loon_grob) {
  guides_grob <- getGrob(loon_grob, "guides")
  !all(str_detect(guides_grob$childrenOrder, ":"))
}

get_showGuides.l_serialaxes <- function(loon_grob) {
  guides_grob <- getGrob(loon_grob, "guides")
  "bounding box" %in% guides_grob$childrenOrder
}

set_guides_grob <- function(loon_grob, xaxis, yaxis, loon_color) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("set_guides_grob", obj)
}

set_guides_grob.default <- function(loon_grob, xaxis, yaxis, loon_color) {

  setGrob(
    gTree = loon_grob,
    gPath = "guides",
    newGrob = gTree(
      children = gList(
        rectGrob(gp = gpar(col = NA,
                           fill = loon_color$guidesbackground_color)
        ),
        do.call(
          gList,
          lapply(xaxis,
                 function(xax) {
                   linesGrob(x = unit(rep(xax, 2), "native"),
                             y =  unit(c(0, 1), "npc"),
                             gp = gpar(col = loon_color$guideslines_color, lwd = 2))

                 }
          )
        ),
        do.call(
          gList,
          lapply(yaxis,
                 function(yax) {
                   linesGrob(x = unit(c(0, 1), "npc") ,
                             y =  unit(rep(yax,2), "native"),
                             gp = gpar(col = loon_color$guideslines_color, lwd = 2))

                 }
          )
        )
      ),
      name = "guides"
    )
  )
}
