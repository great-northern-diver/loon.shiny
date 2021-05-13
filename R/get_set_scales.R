get_scales <- function(loon_grob) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("get_scales", obj)
}

get_scales.default <- function(loon_grob) {
  showScales <- TRUE
  xaxis_grob <- getGrob(loon_grob, "x axis")
  if(is.null(xaxis_grob)) {
    showScales <- FALSE
    xaxis_grob <- getGrob(loon_grob, "x axis: xaxisGrob arguments")
  }
  xaxis <- xaxis_grob$at

  yaxis_grob <- if(showScales) {
    getGrob(loon_grob, "y axis")
  } else {
    getGrob(loon_grob, "y axis: yaxisGrob arguments")
  }
  yaxis <- yaxis_grob$at

  list(
    xaxis = xaxis,
    yaxis = yaxis,
    showScales = showScales
  )
}

set_scales_grob <- function(loon_grob, xaxis, yaxis) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("set_scales_grob", obj)
}

set_scales_grob.default <- function(loon_grob, xaxis, yaxis){

  setGrob(
    gTree = loon_grob,
    gPath = "axes",
    newGrob = gTree(
      children = do.call(
        gList,
        lapply(1:length(getGrob(loon_grob, "axes")[["childrenOrder"]]),
               function(i){
                 grobi <- getGrob(loon_grob, "axes")[["children"]][[i]]
                 grobi_args <- getGrobArgs(grobi)
                 if(!all(names(grobi_args) %in% formalArgs(xaxisGrob))) {
                   grobi_args <- grobi_args[formalArgs(xaxisGrob)]
                 }
                 if(str_detect(grobi$name ,"x axis")) {

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
