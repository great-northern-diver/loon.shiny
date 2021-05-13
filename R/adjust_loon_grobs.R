adjust_loon_grobs <- function(loon_grobs, loonWidgets_info = NULL) {

  loon_grobs <- lapply(loon_grobs, function(loon_grob) pointsGrob_to_gTree(loon_grob))
  
  if(!is.null(loonWidgets_info)) {
    
    loon_grobs <- lapply(1:length(loon_grobs),
                         function(i) {
                           
                           loon_grob <- loon_grobs[[i]]
                           widget_info <- loonWidgets_info[[i]]
                           which_is_selected <- if(is.null(widget_info$selected)) integer(0) else which(widget_info$selected)

                           resetOrder_grob(loon_grob, widget_info, index = which_is_selected)
                         }
    )
    
    stats::setNames(loon_grobs, names(loonWidgets_info))
    
  } else loon_grobs
}

pointsGrob_to_gTree <- function(loon_grob) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("pointsGrob_to_gTree", obj)
}

pointsGrob_to_gTree.default <- function(loon_grob) loon_grob

pointsGrob_to_gTree.l_plot <- function(loon_grob) {
  
  scatterplot_grob <- grid::getGrob(loon_grob, "scatterplot")
  childrenName <- scatterplot_grob$childrenOrder
  
  if(childrenName != "points: mixed glyphs" & childrenName != "points: missing glyphs") {
    # extend pointsGrob to gTree
    args <- getGrobArgs(scatterplot_grob$children[[scatterplot_grob$childrenOrder]])
    
    grid::setGrob(loon_grob,
            gPath = scatterplot_grob$childrenOrder,
            newGrob = gTree(
              children = do.call(
                gList,
                lapply(seq(length(args$x)),
                       function(i) {
                         pointsGrob(
                           x = args$x[i],
                           y = args$y[i],
                           pch = args$pch[i],
                           size = args$size,
                           name = paste0("primitive_glyph ", i),
                           gp = if(args$pch[i] %in% 21:24) {
                             gpar(
                               fill = args$gp$fill[i],
                               col = args$gp$col,
                               cex = args$gp$cex[i]
                             )
                           } else {
                             gpar(
                               col = args$gp$col[i],
                               cex = args$gp$cex[i]
                             )
                           },
                           vp = args$vp
                         )
                       }
                )
              ),
              name = scatterplot_grob$childrenOrder
            )
    )
  } else loon_grob
}

resetOrder_grob <- function(loon_grob, widget_info, index) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("resetOrder_grob", obj)
}

resetOrder_grob.default <- function(loon_grob, widget_info, index) loon_grob

resetOrder_grob.l_plot <- function(loon_grob, widget_info, index) {
  
  
  scatterplot_grob <- grid::getGrob(loon_grob, "scatterplot")
  # only one child
  pointsTree_name <- scatterplot_grob$childrenOrder
  
  display_order <- widget_info$display_order
  newGrob <- grid::getGrob(loon_grob, pointsTree_name)
  
  loon_grob <- grid::setGrob(
    gTree = loon_grob,
    gPath = pointsTree_name,
    newGrob = gTree(
      children = gList(
        newGrob$children[display_order]
      ),
      name = newGrob$name
    )
  )
  
  if(length(index) > 0) {
    
    set_color_grob(
      loon_grob = loon_grob,
      index = index,
      color = widget_info$color[index],
      pointsTree_name = pointsTree_name,
      loon_color = widget_info$loon_color
    )
  } else loon_grob
}

resetOrder_grob.l_graph <- function(loon_grob, widget_info, index) {
  
  if(length(index) > 0) {
    
    set_color_grob(
      loon_grob = loon_grob,
      index = index,
      color = widget_info$color[index],
      loon_color = widget_info$loon_color
    )
  } else loon_grob
}

resetOrder_grob.l_serialaxes <- function(loon_grob, widget_info, index) {
  
  
  axesLayout <- get_axesLayout(loon_grob)
  axes_gPath <- if(axesLayout == "parallel") "parallelAxes" else "radialAxes"
  
  display_order <- widget_info$display_order
  newGrob <- grid::getGrob(loon_grob, axes_gPath)
  
  loon_grob <- grid::setGrob(
    gTree = loon_grob,
    gPath = axes_gPath,
    newGrob = gTree(
      children = gList(
        newGrob$children[display_order]
      ),
      name = newGrob$name
    )
  )
  
  if(length(index) > 0) {
    
    set_color_grob(
      loon_grob = loon_grob,
      index = index,
      color = widget_info$color[index],
      axes_gPath = axes_gPath,
      loon_color = widget_info$loon_color
    )
  } else loon_grob
}