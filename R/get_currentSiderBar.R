get_currentSiderBar <- function(positions, input, noneInteractiveGrobs_index) {

  pos <- integer(0)

  if(!is.null(input$plot_click)) {

    input$plot_click

    n <- dim(positions)[1]
    for(i in 1:n) {
      position <- positions[i, ]
      xmin <- position$l * (input$plot_click$domain$right - input$plot_click$domain$left) + input$plot_click$domain$left
      xmax <- position$r * (input$plot_click$domain$right - input$plot_click$domain$left) + input$plot_click$domain$left
      ymin <- -position$b * (input$plot_click$domain$top - input$plot_click$domain$bottom) + input$plot_click$domain$top
      ymax <- -position$t * (input$plot_click$domain$top - input$plot_click$domain$bottom) + input$plot_click$domain$top

      if(input$plot_click$x <= xmax & input$plot_click$x >= xmin & input$plot_click$y <= ymax & input$plot_click$y >= ymin) {
        pos <- i
        break
      }
    }

    if(length(pos) != 0) {
      if(pos %in% noneInteractiveGrobs_index) pos <- integer(0)
    }
    
  }
  
  if(!is.null(input$plot_brush)) {
    
    input$plot_brush
    
    n <- dim(positions)[1]
    for(i in 1:n) {
      position <- positions[i, ]
      xmin <- position$l * (input$plot_brush$domain$right - input$plot_brush$domain$left) + input$plot_brush$domain$left
      xmax <- position$r * (input$plot_brush$domain$right - input$plot_brush$domain$left) + input$plot_brush$domain$left
      ymin <- -position$b * (input$plot_brush$domain$top - input$plot_brush$domain$bottom) + input$plot_brush$domain$top
      ymax <- -position$t * (input$plot_brush$domain$top - input$plot_brush$domain$bottom) + input$plot_brush$domain$top
      
      xx <- mean(c(input$plot_brush$xmin, input$plot_brush$xmax))
      yy <- mean(c(input$plot_brush$ymin, input$plot_brush$ymax))

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
