adjust_arrangeGrobArgs <- function(arrangeGrobArgs, n) {


  layout_matrix <- arrangeGrobArgs$layout_matrix
  nrow <- arrangeGrobArgs$nrow
  ncol <- arrangeGrobArgs$ncol
  widths <- arrangeGrobArgs$widths
  heights <- arrangeGrobArgs$heights

  # refer to `arrangGrob`
  if (!is.null(ncol) && !is.null(widths)) {
    stopifnot(length(widths) == ncol)
  }
  if (!is.null(nrow) && !is.null(heights)) {
    stopifnot(length(heights) == nrow)
  }
  if (is.null(ncol) && !is.null(widths)) {
    ncol <- length(widths)
  }
  if (is.null(nrow) && !is.null(heights)) {
    nrow <- length(heights)
  }
  if (is.null(nrow) && !is.null(ncol)) {
    nrow <- ceiling(n/ncol)
  }
  if (is.null(ncol) && !is.null(nrow)) {
    ncol <- ceiling(n/nrow)
  }
  stopifnot(nrow * ncol >= n)

  if (is.null(nrow) && is.null(ncol) && is.null(widths) &&
      is.null(heights)) {
    nm <- grDevices::n2mfrow(n)
    nrow <- nm[1]
    ncol <- nm[2]
  }

  if(is.null(widths) && is.null(heights)) {
    arrangeGrobArgs$nrow <- nrow
    arrangeGrobArgs$ncol <- ncol
    return(arrangeGrobArgs)
  }



  # wrap the information of `nrow`, `ncol`, `widths` and `heights` in `layout_matrix`
  if(is.null(layout_matrix)) {
    # nrow and ncol are provided
    layout_matrix <- matrix(NA,
                            nrow = nrow,
                            ncol = ncol,
                            byrow = TRUE)

    for(i in seq(nrow)) {
      for(j in seq(ncol)) {
        k <- (ncol) * (i - 1) + j
        if(k <= n)
          layout_matrix[i, j] <- k
      }
    }
  }


  ncol <- ncol(layout_matrix)
  # row modification
  if(!is.null(widths)) {

    if(length(widths) > ncol) {
      layout_matrix <- do.call(cbind,
                               c(list(layout_matrix),
                                 rep(NA, length(widths) - ncol)
                               )
      )
    }

    layout_matrix <- t(apply(layout_matrix, 1, function(x) rep(x, widths)))
  }

  nrow <- nrow(layout_matrix)
  # column modification
  if(!is.null(heights)) {

    if(length(heights) > nrow) {
      layout_matrix <- do.call(rbind,
                               c(list(layout_matrix),
                                 rep(NA, length(heights) - nrow)
                               )
      )
    }

    layout_matrix <- apply(layout_matrix, 2, function(x) rep(x, heights))
  }


  list(layout_matrix = layout_matrix)
}
