get_outputInfo <- function(loon.grob, loonWidgetsInfo) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("get_outputInfo", obj)
}

get_outputInfo.default <- function(loon.grob, loonWidgetsInfo) {
  
  brushId <- if(is.null(loonWidgetsInfo$selected)) integer(0) else which(loonWidgetsInfo$selected)
  
  list(
    brushId = brushId,
    loonWidgetsInfo = loonWidgetsInfo
  )
}
