
get_display_order <- function(widget) {
  od <- get_model_display_order(widget)
  order(od)
}


reset_order <- function(N, index) {
  
  if(length(index) > 0) {
    newIndex <- 1:N
    newIndex[which(newIndex %in% index)] <- NA
    newIndex <- c(newIndex, index)
    newIndex <- newIndex[which(!is.na(newIndex))]
    order(newIndex)
  } else 1:N
}