set_glyph_grob <- function(loon.grob, index, newPch, tmp, color, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_glyph_grob", obj)
}

set_glyph_grob.l_plot <- function(loon.grob, index, newPch, tmp, color, ...) {

  args <- list(...)
  pointsTreeName <- args$pointsTreeName
  len <- length(index)

  if(pointsTreeName != "points: missing glyphs" && len > 0) {

    size <- args$size
    alpha <- args$alpha
    grobIndex <- args$grobIndex

    newGrob <- grid::getGrob(loon.grob, pointsTreeName)

    lapply(index,
           function(i) {

             grobi <- newGrob$children[[i]]
             gp <- grobi$gp

             if(grepl(grobi$name, pattern = "primitive_glyph")) {

               if(newPch %in% 21:24) {

                 gp$fill <- if(tmp) select_color() else color[i]
                 gp$col <- bounder_color()

               } else {

                 gp$col <- if(tmp) select_color() else color[i]
               }

               newGrob$children[[i]] <<- grid::editGrob(
                 grob = grobi,
                 gp = gp,
                 pch = newPch
               )
             } else {

               x <- args$x
               y <- args$y

               if(newPch %in% 21:24) {

                 gp <- grid::gpar(
                   fill = if(tmp) select_color() else color[i],
                   col = bounder_color(),
                   cex = size[i],
                   alpha = alpha[i]
                 )

               } else {

                 gp <- grid::gpar(
                   col = if(tmp) select_color() else color[i],
                   cex = size[i],
                   alpha = alpha[i]
                 )
               }

               newGrob$children[[i]] <<- grid::pointsGrob(
                 x = unit(x[i], "native"),
                 y = unit(y[i], "native"),
                 pch = newPch,
                 gp = gp,
                 name = paste0("primitive_glyph ", grobIndex[i])
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


set_glyph_grob.l_graph <- function(loon.grob, index, newPch, tmp, color, ...) {

  if(length(index) > 0) {

    newGrob <- grid::getGrob(loon.grob, "graph nodes")
    # point_size <- loon_default_size()[["point_size"]]

    lapply(index,
           function(i) {

             grobi <- newGrob$children[[i]]
             gp <- grobi$gp

             if(newPch %in% 21:24) {

               gp$fill <- if(tmp) select_color() else color[i]
               gp$col <- bounder_color()

             } else {

               gp$col <- if(tmp) select_color() else color[i]
             }

             newGrob$children[[i]] <<- grid::editGrob(
               grob = grobi,
               gp = gp,
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
