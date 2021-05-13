get_output_info <- function(loon_grob, loonWidgets_info) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("get_output_info", obj)
}

get_output_info.default <- function(loon_grob, loonWidgets_info) {
  
  brush_id <- if(is.null(loonWidgets_info$selected)) integer(0) else which(loonWidgets_info$selected)
  
  list(
    brush_id = brush_id,
    loonWidgets_info = loonWidgets_info
  )
}