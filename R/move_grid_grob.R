move_grid_grob <- function(loon.grob, index, swap, squarexy, temporary, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("move_grid_grob", obj)
}



move_grid_grob.l_plot <- function(loon.grob, index, swap, squarexy, temporary, ...)  {
  
  move_jitter_grob.l_plot(loon.grob,
                          index,
                          swap,
                          jitterxy  = squarexy,
                          temporary, ...)
}

move_grid_grob.l_graph <- function(loon.grob, index, swap, squarexy, temporary, ...)  {

  move_jitter_grob.l_graph(loon.grob,
                           index,
                           swap,
                           jitterxy  = squarexy,
                           temporary, ...)
}

square_coord <- function(x, y, index) {


  max_x <- max(x[index])
  max_y <- max(y[index])
  min_x <- min(x[index])
  min_y <- min(y[index])
  n <- length(index)

  nm <- grDevices::n2mfrow(n)
  row_n <- nm[1]
  col_n <- nm[2]
  stepX <- (max_x - min_x)/col_n
  stepY <- (max_y - min_y)/row_n
  x <- c()
  y <- c()

  for(i in 1:row_n) {
    for(j in 1:col_n) {
      if((i-1)* col_n + j > n) {
        break
      } else {
        x[(i-1)*col_n + j] <- min_x + stepX * (j - 1)
        y[(i-1)*col_n + j] <- min_y + stepY * (i - 1)
      }
    }
  }

  list(x = x,
       y = y)

}
