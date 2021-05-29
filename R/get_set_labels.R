get_labels <- function(loon.grob) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("get_labels", obj)
}

get_labels.default <- function(loon.grob) {
  showLabels <- TRUE
  xlabel_grob <- grid::getGrob(loon.grob, "x label")

  if(is.null(xlabel_grob)) {
    showLabels <- FALSE
    xlabel_grob <- grid::getGrob(loon.grob, "x label: textGrob arguments")
  }
  xlabel <- xlabel_grob$label

  ylabel_grob <- if(showLabels) {
    grid::getGrob(loon.grob, "y label")
  } else {
    grid::getGrob(loon.grob, "y label: textGrob arguments")
  }
  ylabel <- ylabel_grob$label

  title <- if(showLabels) {
    titleGrob <- grid::getGrob(loon.grob, "title")
  } else {
    titleGrob <- grid::getGrob(loon.grob, "title: textGrob arguments")
  }
  title <- title$label

  list(
    xlabel = xlabel,
    ylabel = ylabel,
    title = title
  )
}

get_labels.l_serialaxes <- function(loon.grob) {

  showLabels <- TRUE
  titleGrob <- grid::getGrob(loon.grob, "title")

  if(is.null(titleGrob)) {
    showLabels <- FALSE
    titleGrob <- grid::getGrob(loon.grob, "title: textGrob arguments")
  }

  title <- titleGrob$label

  list(
    title = title,
    showLabels = showLabels
  )
}


set_labelsGrob <- function(loon.grob, showScales, xlabel, ylabel, title) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_labelsGrob", obj)
}

set_labelsGrob.default <- function(loon.grob, showScales, xlabel, ylabel, title) {

  xylab_loc <- if (showScales) c(-3.5, -6.5) else c(-1, -1)

  grid::setGrob(
    gTree = loon.grob,
    gPath = "labels",
    newGrob = gTree(
      children = do.call(
        gList,
        lapply(1:length(grid::getGrob(loon.grob, "labels")[["childrenOrder"]]),
               function(i){
                 grobi <- grid::getGrob(loon.grob, "labels")[["children"]][[i]]
                 grobi_args <-  getGrobArgs(grobi)

                 if(grepl(grobi$name ,pattern = "x label")) {

                   if(is.null(xlabel)) xlabel <- ""

                   grobi_args$label <- xlabel
                   grobi_args$x <- unit(0.5, "npc")
                   grobi_args$y <- unit(xylab_loc[1], "lines")
                   grobi_args$rot <- 0

                   do.call(
                     grid::textGrob,
                     grobi_args
                   )
                 } else if(grepl(grobi$name ,pattern = "y label")) {

                   if(is.null(ylabel)) ylabel <- ""

                   grobi_args$label <- ylabel
                   grobi_args$x <- unit(xylab_loc[2], "lines")
                   grobi_args$y <- unit(0.5, "npc")
                   grobi_args$rot <- 90

                   do.call(
                     grid::textGrob,
                     grobi_args
                   )
                 }  else if(grepl(grobi$name ,pattern = "title")) {

                   if(is.null(title)) title <- ""

                   grobi_args$label <- title
                   do.call(
                     grid::textGrob,
                     grobi_args
                   )
                 }
               }
        )
      ),  name = "labels"
    )
  )
}

set_labelsGrob.l_serialaxes <- function(loon.grob, title) {

  gPath <- "title"
  titleGrob <- grid::getGrob(loon.grob, gPath)

  if(is.null(titleGrob)) {
    gPath <- "title: textGrob arguments"
    titleGrob <- grid::getGrob(loon.grob, gPath)
  }

  grid::setGrob(
    gTree = loon.grob,
    gPath = gPath,
    newGrob = do.call(
      grid::textGrob,
      getGrobArgs(titleGrob)
    )
  )
}


