get_currentSiderBar <- function(positions, input, noneInteractiveGrobs_index) {

  pos <- integer(0)

  if(!is.null(input$plotClick)) {

    input$plotClick

    n <- dim(positions)[1]
    for(i in 1:n) {
      position <- positions[i, ]
      xmin <- position$l * (input$plotClick$domain$right - input$plotClick$domain$left) + input$plotClick$domain$left
      xmax <- position$r * (input$plotClick$domain$right - input$plotClick$domain$left) + input$plotClick$domain$left
      ymin <- -position$b * (input$plotClick$domain$top - input$plotClick$domain$bottom) + input$plotClick$domain$top
      ymax <- -position$t * (input$plotClick$domain$top - input$plotClick$domain$bottom) + input$plotClick$domain$top

      if(input$plotClick$x <= xmax & input$plotClick$x >= xmin & input$plotClick$y <= ymax & input$plotClick$y >= ymin) {
        pos <- i
        break
      }
    }

    if(length(pos) != 0) {
      if(pos %in% noneInteractiveGrobs_index) pos <- integer(0)
    }
    
  }
  
  if(!is.null(input$plotBrush)) {
    
    input$plotBrush
    
    n <- dim(positions)[1]
    for(i in 1:n) {
      position <- positions[i, ]
      xmin <- position$l * (input$plotBrush$domain$right - input$plotBrush$domain$left) + input$plotBrush$domain$left
      xmax <- position$r * (input$plotBrush$domain$right - input$plotBrush$domain$left) + input$plotBrush$domain$left
      ymin <- -position$b * (input$plotBrush$domain$top - input$plotBrush$domain$bottom) + input$plotBrush$domain$top
      ymax <- -position$t * (input$plotBrush$domain$top - input$plotBrush$domain$bottom) + input$plotBrush$domain$top
      
      xx <- mean(c(input$plotBrush$xmin, input$plotBrush$xmax))
      yy <- mean(c(input$plotBrush$ymin, input$plotBrush$ymax))

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
