move_grid_grob <- function(loon_grob, index, swap, square_xy, temporary, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("move_grid_grob", obj)
}



move_grid_grob.l_plot <- function(loon_grob, index, swap, square_xy, temporary, ...)  {
  
  move_jitter_grob.l_plot(loon_grob,
                          index,
                          swap,
                          jitter_xy  = square_xy,
                          temporary, ...)
}

move_grid_grob.l_graph <- function(loon_grob, index, swap, square_xy, temporary, ...)  {

  move_jitter_grob.l_graph(loon_grob,
                           index,
                           swap,
                           jitter_xy  = square_xy,
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
  step_x <- (max_x - min_x)/col_n
  step_y <- (max_y - min_y)/row_n
  x <- c()
  y <- c()

  for(i in 1:row_n) {
    for(j in 1:col_n) {
      if((i-1)* col_n + j > n) {
        break
      } else {
        x[(i-1)*col_n + j] <- min_x + step_x * (j - 1)
        y[(i-1)*col_n + j] <- min_y + step_y * (i - 1)
      }
    }
  }

  list(x = x,
       y = y)

}
