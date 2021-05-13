get_binInfo <- function(data, origin, active, binwidth, yshows) {
  
  which_is_active <- which(active)
  data <- data[active]
  n <- length(data)
  binId <- list()
  min_x <- min(data)
  start_bin <- if(origin >= min_x) {
    i <- 0
    while (min_x + i * binwidth < origin) i <- i + 1
    origin - i * binwidth
  } else {
    i <- 0
    while (origin + i * binwidth < min_x) i <- i + 1
    origin + (i-1) * binwidth
  }
  end_bin <- max(data)
  
  i <- 1
  binX <- c()
  while(start_bin + (i-1) * binwidth <= end_bin){
    left <- start_bin + (i - 1) * binwidth
    right <- start_bin + i * binwidth
    binId[[i]] <- which_is_active[which((data < right & data >= left) == TRUE)]
    binX <- c(binX, left, right)
    i <- i + 1
  }
  
  binHeight <- if(yshows == "frequency") sapply(binId, "length") else sapply(binId, "length") / (n * binwidth)
  
  list(
    binId = binId,
    binX = unique(binX),
    binHeight = binHeight
  )
}