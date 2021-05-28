get_binxy <- function(binX, binId, binwidth, yshows, color, n) {

  if(length(binX) != length(binId) + 1) stop("Unexpected")

  colorOrder <- as.character(levels(as.factor(color)))
  ymax <- list()
  ymin <- list()
  x <- list()
  binNames <- list()
  binId_group <- list()

  not_show <- lapply(seq(length(binId)),
                     function(i){
                       if(length(binId[[i]]) != 0) {
                         colorBinHeight <- if(yshows == "frequency") {
                           table(color[binId[[i]]])
                         } else {
                           table(color[binId[[i]]])/(n * binwidth)
                         }
                         # reorder color bin height
                         colorBinHeight <- colorBinHeight[order(unname(sapply(names(colorBinHeight),
                                                                              function(name) which(colorOrder %in% name))))]
                         # cumsum
                         cumsumColorBinHeight <- cumsum(colorBinHeight)
                         len <- length(cumsumColorBinHeight)

                         ymax[[i]] <<- cumsumColorBinHeight
                         ymin[[i]] <<- c(0, cumsumColorBinHeight[-len])
                         binNames[[i]] <<- rep(i, len)
                         binId_group[[i]] <<- lapply(1:len, function(j) binId[[i]][which(color[binId[[i]]] == names(colorBinHeight)[j])])
                         x[[i]] <<- rep(mean(binX[i], binX[i+1]), len)
                       } else {
                         cumsumColorBinHeight <- 0

                         ymax[[i]] <<- 0
                         ymin[[i]] <<- 0
                         binNames[[i]] <<- -1
                         binId_group[[i]] <<- list()
                         x[[i]] <<- rep(mean(binX[i], binX[i+1]), 1)
                       }
                     }
  )

  x <- unlist(x)
  # xmax <- x + binwidth/2
  # xmin <- x - binwidth/2
  xmax <- x + binwidth
  xmin <- x

  list(
    xmin = xmin,
    xmax = xmax,
    ymin = unname(unlist(ymin)),
    ymax = unname(unlist(ymax)),
    binNames = unlist(binNames),
    binId_group = binId_group
  )
}
