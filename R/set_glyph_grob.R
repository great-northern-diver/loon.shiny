set_glyph_grob <- function(loon.grob, index, newPch, tmp, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_glyph_grob", obj)
}

set_glyph_grob.l_plot <- function(loon.grob, index, newPch, tmp, ...) {

  args <- list(...)
  pointsTreeName <- args$pointsTreeName

  if(pointsTreeName != "points: missing glyphs" & length(index) > 0) {

    color <- args$color
    size <- args$size
    pch <- args$pch
    grob_index <- args$grob_index

    newGrob <- grid::getGrob(loon.grob, pointsTreeName)
    point_size <- loon_default_size()[["point_size"]]

    lapply(index,
           function(i) {

             if(grepl(newGrob$children[[i]]$name, pattern = "primitive_glyph")) {

               newGrob$children[[i]] <<- grid::editGrob(
                 grob = newGrob$children[[i]],
                 gp = if(newPch %in% 21:24) {
                   grid::gpar(
                     fill = if(tmp) select_color() else color[i],
                     cex = size[i],
                     col = bounder_color()
                   )
                 } else {
                   grid::gpar(
                     col = if(tmp) select_color() else color[i],
                     cex = size[i]
                   )
                 },
                 pch = newPch
               )
             } else {

               x <- args$x
               y <- args$y

               newGrob$children[[i]] <<- pointsGrob(
                 x = unit(x[i], "native"),
                 y = unit(y[i], "native"),
                 pch = newPch,
                 gp = if(newPch %in% 21:24) {
                   grid::gpar(
                     fill = if(tmp) select_color() else color[i],
                     cex = size[i] ,
                     col = bounder_color()
                   )
                 } else {
                   grid::gpar(
                     col = if(tmp) select_color() else color[i],
                     cex = size[i]
                   )
                 },
                 name = paste0("primitive_glyph ", grob_index[i])
               )
             }
           }
    )

    grid::setGrob(
      gTree = loon.grob,
      gPath = pointsTreeName,
      newGrob = newGrob
    )
  } else {
    loon.grob
  }
}


set_glyph_grob.l_graph <- function(loon.grob, index, newPch, tmp, ...) {

  if(length(index) > 0) {

    args <- list(...)
    pch <- args$pch
    size <- args$size
    color <- args$color

    newGrob <- grid::getGrob(loon.grob, "graph nodes")
    # point_size <- loon_default_size()[["point_size"]]

    lapply(index,
           function(i) {

             newGrob$children[[i]] <<- grid::editGrob(
               grob = newGrob$children[[i]],
               gp = if(newPch %in% 21:24) {
                 grid::gpar(
                   fill = if(tmp) select_color() else color[i],
                   cex = size[i],
                   col = bounder_color()
                 )
               } else {
                 grid::gpar(
                   col = if(tmp) select_color() else color[i],
                   cex = size[i]
                 )
               },
               pch = newPch
             )
           }
    )

    grid::setGrob(
      gTree = loon.grob,
      gPath = "graph nodes",
      newGrob = newGrob
    )
  } else {
    loon.grob
  }
}
