get_currentSiderBar <- function(positions, input, noneInteractiveGrobs_index) {

  pos <- integer(0)
  plotClick <- input$plotClick

  if(!is.null(plotClick) && !is.null(positions)) {

    n <- dim(positions)[1]
    for(i in 1:n) {
      position <- positions[i, ]
      xmin <- position$l * (plotClick$domain$right - plotClick$domain$left) + plotClick$domain$left
      xmax <- position$r * (plotClick$domain$right - plotClick$domain$left) + plotClick$domain$left
      ymin <- -position$b * (plotClick$domain$top - plotClick$domain$bottom) + plotClick$domain$top
      ymax <- -position$t * (plotClick$domain$top - plotClick$domain$bottom) + plotClick$domain$top

      if(plotClick$x <= xmax & plotClick$x >= xmin & plotClick$y <= ymax & plotClick$y >= ymin) {
        pos <- i
        break
      }
    }

    if(length(pos) != 0) {
      if(pos %in% noneInteractiveGrobs_index) pos <- integer(0)
    }

  }

  plotBrush <- input$plotBrush

  if(!is.null(plotBrush)) {

    n <- dim(positions)[1]
    for(i in 1:n) {
      position <- positions[i, ]
      xmin <- position$l * (plotBrush$domain$right - plotBrush$domain$left) + plotBrush$domain$left
      xmax <- position$r * (plotBrush$domain$right - plotBrush$domain$left) + plotBrush$domain$left
      ymin <- -position$b * (plotBrush$domain$top - plotBrush$domain$bottom) + plotBrush$domain$top
      ymax <- -position$t * (plotBrush$domain$top - plotBrush$domain$bottom) + plotBrush$domain$top

      xx <- mean(c(plotBrush$xmin, plotBrush$xmax))
      yy <- mean(c(plotBrush$ymin, plotBrush$ymax))

      if(xx <= xmax & xx >= xmin & yy <= ymax & yy >= ymin) {
        pos <- i
        break
      }
    }

    if(length(pos) != 0) {
      if(pos %in% noneInteractiveGrobs_index) pos <- integer(0)
    }
  }

  pos
}
