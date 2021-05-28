set_deactive_grob <- function(loon.grob, index, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_deactive_grob", obj)
}

set_deactive_grob.l_plot <- function(loon.grob, index, ...) {

  args <- list(...)
  pointsTreeName <- args$pointsTreeName

  if(pointsTreeName != "points: missing glyphs" & length(index) != 0) {

    newGrob <- grid::getGrob(loon.grob, pointsTreeName)

    lapply(index,
           function(i) {

             if(grepl(newGrob$children[[i]]$name, pattern = "primitive_glyph")) {

               newGrob$children[[i]] <<- do.call(grob, getGrobArgs(newGrob$children[[i]]))

             } else if(grepl(newGrob$children[[i]]$name, pattern = "pointrange_glyph")) {

               newGrob$children[[i]] <<- gTree(
                 children = gList(
                   # point
                   do.call(grob, getGrobArgs(grid::getGrob(newGrob$children[[i]], "point"))),
                   # range
                   do.call(grob, getGrobArgs(grid::getGrob(newGrob$children[[i]], "range")))
                 ),
                 name = newGrob$children[[i]]$name
               )

             } else if(grepl(newGrob$children[[i]]$name, pattern = "text_glyph")) {

               newGrob$children[[i]] <<- do.call(grob, getGrobArgs(newGrob$children[[i]]))

             } else if(grepl(newGrob$children[[i]]$name, pattern = "serialaxes_glyph")) {

               newGrob$children[[i]] <<- gTree(
                 children = gList(
                   do.call(grob, getGrobArgs(newGrob$children[[i]]$children[[1]])),
                   do.call(grob, getGrobArgs(newGrob$children[[i]]$children[[2]])),
                   do.call(grob, getGrobArgs(newGrob$children[[i]]$children[[3]]))
                 ),
                 name = newGrob$children[[i]]$name
               )

             } else if(grepl(newGrob$children[[i]]$name, pattern = "polygon_glyph")) {

               newGrob$children[[i]] <<- do.call(grob, getGrobArgs(newGrob$children[[i]]))

             } else if(grepl(newGrob$children[[i]]$name, pattern = "image_glyph")) {

               newGrob$children[[i]] <<- gTree(
                 children = gList(
                   do.call(grob, getGrobArgs(grid::getGrob(newGrob$children[[i]], "image_border"))),
                   do.call(grob, getGrobArgs(grid::getGrob(newGrob$children[[i]], "image")))
                 ),
                 name = newGrob$children[[i]]$name
               )
             } else stop("not inplemented")
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


set_deactive_grob.l_hist <- function(loon.grob, index, ...) {

  if(length(index) >0) {

    args <- list(...)
    yshows <- args$yshows
    active <- args$active
    binId <- args$binId
    binX <- args$binX
    binHeight <- args$binHeight
    binwidth <- args$binwidth
    n <- args$n
    swapAxes <- args$swapAxes
    showStackedColors <- args$showStackedColors
    showOutlines <- args$showOutlines
    color <- args$color
    colorFill <- args$colorFill
    colorOutline <- args$colorOutline


  } else {
    loon.grob
  }
}

set_deactive_grob.l_graph <- function(loon.grob, index) {

  if(length(index) > 0) {

    nodesGrob <- grid::getGrob(loon.grob, "graph nodes")

    lapply(index,
           function(i) {
             nodesGrob$children[[i]] <<- do.call(grob, getGrobArgs(nodesGrob$children[[i]]))
           }
    )

    loon.grob <- grid::setGrob(
      gTree = loon.grob,
      gPath = "graph nodes",
      newGrob = nodesGrob
    )

    labelsGrob <- grid::getGrob(loon.grob, "graph labels")

    lapply(index,
           function(i) {
             labelsGrob$children[[i]] <<- do.call(grob, getGrobArgs(labelsGrob$children[[i]]))
           }
    )

    loon.grob <- grid::setGrob(
      gTree = loon.grob,
      gPath = "graph labels",
      newGrob = labelsGrob
    )


    edgesGrob <- grid::getGrob(loon.grob, "graph edges")

    lapply(1:length(edgesGrob$children),
           function(i) {

             edgesGrob$children[[i]] <<- if(i %in% index) {
               do.call(grob, getGrobArgs(edgesGrob$children[[i]]))
             } else {

               grobi <- edgesGrob$children[[i]]

               if(!is.null(grobi$x) & !is.null(grobi$y)) {

                 active_id <- which(!grobi$id %in% index)

                 if(length(active_id) > 0) {
                   editGrob(
                     grob = grobi,
                     x = grobi$x[active_id],
                     y = grobi$y[active_id],
                     id = grobi$id[active_id],
                     gp = gpar(
                       col = grobi$gp$col[active_id[which(active_id <= length(grobi$gp$col))]],
                       lwd = edgesGrob$children[[i]]$gp$lwd
                     )
                   )
                 } else grob(name = grobi$name)
               } else grobi
             }
           }
    )

    grid::setGrob(
      gTree = loon.grob,
      gPath = "graph edges",
      newGrob = edgesGrob
    )
  } else {
    loon.grob
  }
}


set_deactive_grob.l_serialaxes <- function(loon.grob, index, ...) {

  if(length(index) > 0) {

    args <- list(...)
    axesGpath <- args$axesGpath
    axesGrob <- grid::getGrob(loon.grob, axesGpath)

    lapply(index,
           function(i) {
             axesGrob$children[[i]] <<- do.call(grob, getGrobArgs(axesGrob$children[[i]]))
           }
    )

    grid::setGrob(
      gTree = loon.grob,
      gPath = axesGpath,
      newGrob = axesGrob
    )
  } else {
    loon.grob
  }
}
