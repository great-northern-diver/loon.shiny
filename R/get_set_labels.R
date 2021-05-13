get_labels <- function(loon_grob) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("get_labels", obj)
}

get_labels.default <- function(loon_grob) {
  showLabels <- TRUE
  xlabel_grob <- getGrob(loon_grob, "x label")

  if(is.null(xlabel_grob)) {
    showLabels <- FALSE
    xlabel_grob <- getGrob(loon_grob, "x label: textGrob arguments")
  }
  xlabel <- xlabel_grob$label

  ylabel_grob <- if(showLabels) {
    getGrob(loon_grob, "y label")
  } else {
    getGrob(loon_grob, "y label: textGrob arguments")
  }
  ylabel <- ylabel_grob$label

  title <- if(showLabels) {
    title_grob <- getGrob(loon_grob, "title")
  } else {
    title_grob <- getGrob(loon_grob, "title: textGrob arguments")
  }
  title <- title$label

  list(
    xlabel = xlabel,
    ylabel = ylabel,
    title = title
  )
}

get_labels.l_serialaxes <- function(loon_grob) {

  showLabels <- TRUE
  title_grob <- getGrob(loon_grob, "title")

  if(is.null(title_grob)) {
    showLabels <- FALSE
    title_grob <- getGrob(loon_grob, "title: textGrob arguments")
  }

  title <- title_grob$label

  list(
    title = title,
    showLabels = showLabels
  )
}


set_labels_grob <- function(loon_grob, showScales, xlabel, ylabel, title) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("set_labels_grob", obj)
}

set_labels_grob.default <- function(loon_grob, showScales, xlabel, ylabel, title) {

  xylab_loc <- if (showScales) c(-3.5, -6.5) else c(-1, -1)

  setGrob(
    gTree = loon_grob,
    gPath = "labels",
    newGrob = gTree(
      children = do.call(
        gList,
        lapply(1:length(getGrob(loon_grob, "labels")[["childrenOrder"]]),
               function(i){
                 grobi <- getGrob(loon_grob, "labels")[["children"]][[i]]
                 grobi_args <-  getGrobArgs(grobi)

                 if(str_detect(grobi$name ,"x label")) {
                   
                   if(is.null(xlabel)) xlabel <- ""

                   grobi_args$label <- xlabel
                   grobi_args$x <- unit(0.5, "npc")
                   grobi_args$y <- unit(xylab_loc[1], "lines")
                   grobi_args$rot <- 0

                   do.call(
                     grid::textGrob,
                     grobi_args
                   )
                 } else if(str_detect(grobi$name ,"y label")) {

                   if(is.null(ylabel)) ylabel <- ""
                   
                   grobi_args$label <- ylabel
                   grobi_args$x <- unit(xylab_loc[2], "lines")
                   grobi_args$y <- unit(0.5, "npc")
                   grobi_args$rot <- 90

                   do.call(
                     grid::textGrob,
                     grobi_args
                   )
                 }  else if(str_detect(grobi$name ,"title")) {
                   
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

set_labels_grob.l_serialaxes <- function(loon_grob, title) {

  gPath <- "title"
  title_grob <- getGrob(loon_grob, gPath)

  if(is.null(title_grob)) {
    gPath <- "title: textGrob arguments"
    title_grob <- getGrob(loon_grob, gPath)
  }

  setGrob(
    gTree = loon_grob,
    gPath = gPath,
    newGrob = do.call(
      textGrob,
      getGrobArgs(title_grob)
    )
  )
}


