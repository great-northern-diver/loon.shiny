get_scales <- function(loon.grob) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("get_scales", obj)
}

get_scales.default <- function(loon.grob) {
  showScales <- TRUE
  xaxis_grob <- grid::getGrob(loon.grob, "x axis")
  if(is.null(xaxis_grob)) {
    showScales <- FALSE
    xaxis_grob <- grid::getGrob(loon.grob, "x axis: xaxisGrob arguments")
  }
  xaxis <- xaxis_grob$at

  yaxis_grob <- if(showScales) {
    grid::getGrob(loon.grob, "y axis")
  } else {
    grid::getGrob(loon.grob, "y axis: yaxisGrob arguments")
  }
  yaxis <- yaxis_grob$at

  list(
    xaxis = xaxis,
    yaxis = yaxis,
    showScales = showScales
  )
}

set_scales_grob <- function(loon.grob, xaxis, yaxis) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("set_scales_grob", obj)
}

set_scales_grob.default <- function(loon.grob, xaxis, yaxis){

  grid::setGrob(
    gTree = loon.grob,
    gPath = "axes",
    newGrob = gTree(
      children = do.call(
        gList,
        lapply(1:length(grid::getGrob(loon.grob, "axes")[["childrenOrder"]]),
               function(i){
                 grobi <- grid::getGrob(loon.grob, "axes")[["children"]][[i]]
                 grobi_args <- getGrobArgs(grobi)
                 if(!all(names(grobi_args) %in% formalArgs(xaxisGrob))) {
                   grobi_args <- grobi_args[formalArgs(xaxisGrob)]
                 }
                 if(grepl(grobi$name ,pattern = "x axis")) {

                   grobi_args$at <- xaxis
                   do.call(
                     grid::xaxisGrob,
                     grobi_args
                   )
                 } else {

                   grobi_args$at <- yaxis
                   do.call(
                     grid::yaxisGrob,
                     grobi_args
                   )
                 }
               }
        )
      ), name = "axes"
    )
  )
}
