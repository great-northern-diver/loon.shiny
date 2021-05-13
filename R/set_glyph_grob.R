set_glyph_grob <- function(loon_grob, index, new_pch, tmp, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("set_glyph_grob", obj)
}

set_glyph_grob.l_plot <- function(loon_grob, index, new_pch, tmp, ...) {

  args <- list(...)
  pointsTree_name <- args$pointsTree_name

  if(pointsTree_name != "points: missing glyphs" & length(index) > 0) {
    
    color <- args$color
    size <- args$size
    pch <- args$pch
    grob_index <- args$grob_index
    
    newGrob <- getGrob(loon_grob, pointsTree_name)

    loon_color <- args$loon_color
    point_size <- loon_default_size()[["point_size"]]

    lapply(index,
           function(i) {

             if(str_detect(newGrob$children[[i]]$name, "primitive_glyph")) {
               
               newGrob$children[[i]] <<- editGrob(
                 grob = newGrob$children[[i]],
                 gp = if(new_pch %in% 21:24 & !(pch[i] %in% 21:24)) {
                   gpar(
                     fill = if(tmp) loon_color$select_color[1] else color[i],
                     cex = size[i],
                     col = loon_color$foreground_color[1]
                   )
                 } else {
                   gpar(
                     col = if(tmp) loon_color$select_color[1] else color[i],
                     cex = size[i]
                   )
                 },
                 pch = new_pch
               )
             } else {
               
               x <- args$x
               y <- args$y
               
               newGrob$children[[i]] <<- pointsGrob(
                 x = unit(x[i], "native"),
                 y = unit(y[i], "native"),
                 pch = new_pch,
                 gp = if(new_pch %in% 21:24) {
                   gpar(
                     fill = if(tmp) loon_color$select_color[1] else color[i],
                     cex = size[i] ,
                     col = loon_color$foreground_color[1]
                   )
                 } else {
                   gpar(
                     col = if(tmp) loon_color$select_color[1] else color[i],
                     cex = size[i]
                   )
                 },
                 name = paste0("primitive_glyph ", grob_index[i])
               )
             }
           }
    )

    setGrob(
      gTree = loon_grob,
      gPath = pointsTree_name,
      newGrob = newGrob
    )
  } else {
    loon_grob
  }
}


set_glyph_grob.l_graph <- function(loon_grob, index, new_pch, tmp, ...) {

  if(length(index) > 0) {
    
    args <- list(...)
    pch <- args$pch
    size <- args$size
    color <- args$color
    
    newGrob <- getGrob(loon_grob, "graph nodes")
    
    loon_color <- args$loon_color
    # point_size <- loon_default_size()[["point_size"]]
    
    lapply(index,
           function(i) {
             
             newGrob$children[[i]] <<- editGrob(
               grob = newGrob$children[[i]],
               gp = if(new_pch %in% 21:24 & !(pch[i] %in% 21:24)) {
                 gpar(
                   fill = if(tmp) loon_color$select_color[1] else color[i],
                   cex = size[i],
                   col = loon_color$foreground_color[1]
                 )
               } else {
                 gpar(
                   col = if(tmp) loon_color$select_color[1] else color[i],
                   cex = size[i]
                 )
               },
               pch = new_pch
             )
           }
    )
    
    setGrob(
      gTree = loon_grob,
      gPath = "graph nodes",
      newGrob = newGrob
    )
  } else {
    loon_grob
  }
}
