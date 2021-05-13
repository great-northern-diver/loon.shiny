get_worldViewPort <- function(loon_grob, parent, parentExcluded = TRUE) {

  layers <- get_layers(loon_grob)

  viewPort <- get_viewPort(loon_grob = loon_grob)
  plotView_xlim <- viewPort[[2]]$xscale
  plotView_ylim <- viewPort[[2]]$yscale

  xlim <- ylim <- list()

  lapply(1:length(layers),
         function(i){

           layer <- layers[i]

           if(layer != parent) {
             
             layer_lim <- get_layer_worldView(loon_grob, layer)
             xlim[[i]] <<- layer_lim$xlim
             ylim[[i]] <<- layer_lim$ylim
             
           } else {

             if(parentExcluded) {
               xlim[[i]] <<- numeric(0)
               ylim[[i]] <<- numeric(0)
             } else {
               xlim[[i]] <<- plotView_xlim
               ylim[[i]] <<- plotView_ylim
             }
           }
         }
  )

  if(length(layers) == 1 & all(layers == parent) & parentExcluded) {

    worldView_xlim <- numeric(0)
    worldView_ylim <- numeric(0)
  } else {
    
    xlim <- unlist(xlim)
    ylim <- unlist(ylim)

    if(length(xlim) != 0 & length(ylim) != 0) {
      worldView_xlim <- c(min(xlim), max(xlim))
      worldView_ylim <- c(min(ylim), max(ylim))
      
      if(!all(worldView_xlim == plotView_xlim)) {
        worldView_xlim <- grDevices::extendrange(worldView_xlim, f = 0.1)
      }
      
      if(!all(worldView_ylim == plotView_ylim)) {
        worldView_ylim <- grDevices::extendrange(worldView_ylim, f = 0.1)
      }
    } else {
      worldView_xlim <- numeric(0)
      worldView_ylim <- numeric(0)
    }
  }

  list(
    worldView_xlim = worldView_xlim,
    worldView_ylim = worldView_ylim
  )
}

get_layer_worldView <- function(loon_grob, layer) {
  
  grobi <- getGrob(loon_grob, layer)
  
  if(str_detect(layer, "l_layer_polygon:") | str_detect(layer, "l_layer_line:") | str_detect(layer, "l_layer_oval:") | str_detect(layer, "l_layer_points:")) {
    
    x <- as.numeric(grobi$x)
    y <- as.numeric(grobi$y)
    
    if(length(x) == 1) {
      dx <- log_ceiling(x, 1)
      dy <- log_ceiling(y, 1)
      list(
        xlim = c(x - dx, x + dx),
        ylim = c(y - dy, y + dy) 
      )
    } else {
      list(
        xlim = grDevices::extendrange(x),
        ylim = grDevices::extendrange(y)
      )
    }
    
  } else if(str_detect(layer, "l_layer_rectangle:")) {
    
    x <- as.numeric(grobi$x)
    y <- as.numeric(grobi$y)
    width <- as.numeric(grobi$width)
    height <- as.numeric(grobi$height)
    
    list(
      xlim = grDevices::extendrange(c(x, x + width)),
      ylim = grDevices::extendrange(c(y, y + height))
    )
    
  } else if(str_detect(layer, "l_layer_text:")) {
    
    x <- get_unit(grobi$x, "native", as.numeric = TRUE)
    y <- get_unit(grobi$y, "native", as.numeric = TRUE)
    
    if(length(grobi$x) == 1) {
      
      dx <- log_ceiling(x, 1)
      dy <- log_ceiling(y, 1)
      
      list(
        xlim = c(x - dx, x + dx),
        ylim = c(y - dy, y + dy) 
      )
    } else {
      
      list(
        xlim = grDevices::extendrange(x),
        ylim = grDevices::extendrange(y)
      )
    }
    
  } else if(str_detect(layer, "l_layer_polygons:") | str_detect(layer, "l_layer_lines:")) {
    
    x <- y <- list()
    
    lapply(1:length(grobi$children),
           function(i){
             child <- grobi$children[[i]]
             x[[i]] <<- as.numeric(child$x)
             y[[i]] <<- as.numeric(child$y)
           }
    )
    
    x <- unlist(x)
    y <- unlist(y)
    
    list(
      xlim = grDevices::extendrange(x),
      ylim = grDevices::extendrange(y)
    )
    
  } else if(str_detect(layer, "l_layer_rectangles:")) {
    
    x <- y <- list()
    width <- height <- list()
    
    lapply(1:length(grobi$children),
           function(i){
             child <- grobi$children[[i]]
             x[[i]] <<- as.numeric(child$x)
             y[[i]] <<- as.numeric(child$y)
             width[[i]] <<- as.numeric(child$width)
             height[[i]] <<- as.numeric(child$height)
           }
    )
    
    x <- unlist(x)
    y <- unlist(y)
    width <- unlist(width)
    height <- unlist(height)
    
    list(
      xlim = grDevices::extendrange(c(x, x + width)),
      ylim = grDevices::extendrange(c(y, y + height))
    )
  } else if(str_detect(layer, "l_layer_texts:")) {
    
    x <- y <- list()
    
    lapply(1:length(grobi$children),
           function(i){
             child <- grobi$children[[i]]
             x[[i]] <<- get_unit(child$x, "native", as.numeric = TRUE)
             y[[i]] <<- get_unit(child$y, "native", as.numeric = TRUE)
           }
    )
    
    x <- unlist(x)
    y <- unlist(y)
    
    list(
      xlim = grDevices::extendrange(x),
      ylim = grDevices::extendrange(y)
    )
  } else {
    
    list(
      xlim = numeric(0),
      ylim = numeric(0)
    )
  }
}