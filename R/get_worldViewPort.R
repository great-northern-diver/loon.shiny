get_worldViewPort <- function(loon.grob, parent = "", parentExcluded = TRUE) {

  layers <- get_layers(loon.grob)

  loonplot <- grid::getGrob(loon.grob, "loon plot")
  vp <- loonplot$vp
  grid::pushViewport(vp)

  viewPort <- get_viewPort(loon.grob = loon.grob)
  plotViewXlim <- viewPort[[2]]$xscale
  plotViewYlim <- viewPort[[2]]$yscale

  xlim <- ylim <- list()

  lapply(seq(length(layers)),
         function(i){

           layer <- layers[i]

           if(layer != parent) {

             layerLimits <- get_layer_worldView(loon.grob, layer)
             xlim[[i]] <<- layerLimits$xlim
             ylim[[i]] <<- layerLimits$ylim

           } else {

             if(parentExcluded) {
               xlim[[i]] <<- numeric(0)
               ylim[[i]] <<- numeric(0)
             } else {
               xlim[[i]] <<- plotViewXlim
               ylim[[i]] <<- plotViewYlim
             }
           }
         }
  )

  if(length(layers) == 1 && all(layers == parent) && parentExcluded) {

    xscale <- numeric(0)
    yscale <- numeric(0)

  } else {

    xlim <- unlist(xlim)
    ylim <- unlist(ylim)

    if(length(xlim) != 0 && length(ylim) != 0) {
      xscale <- range(c(min(xlim), max(xlim)))
      yscale <- range(c(min(ylim), max(ylim)))

    } else {
      xscale <- numeric(0)
      yscale <- numeric(0)
    }
  }

  list(
    xlim = xscale,
    ylim = yscale
  )
}

get_layer_worldView <- function(loon.grob, layer) {

  grobi <- grid::getGrob(loon.grob, layer)

  if(grepl(layer, pattern = "l_layer_polygon:") ||
     grepl(layer, pattern = "l_layer_line:") ||
     grepl(layer, pattern = "l_layer_oval:") ||
     grepl(layer, pattern = "l_layer_points:")) {

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

  } else if(grepl(layer, pattern = "l_layer_rectangle:")) {

    x <- as.numeric(grobi$x)
    y <- as.numeric(grobi$y)
    width <- as.numeric(grobi$width)
    height <- as.numeric(grobi$height)

    list(
      xlim = grDevices::extendrange(c(x - width/2, x + width/2)),
      ylim = grDevices::extendrange(c(y - height/2, y + height/2))
    )

  } else if(grepl(layer, pattern = "l_layer_text:")) {

    tryCatch(
      {
        x <- grid::convertX(grobi$x, "native", valueOnly = TRUE)
        y <- grid::convertX(grobi$y, "native", valueOnly = TRUE)
      },
      error = function(e) {
        x <<- get_unit(grobi$x, "native", as.numeric = TRUE)
        y <<- get_unit(grobi$y, "native", as.numeric = TRUE)
      }
    )

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

  } else if(grepl(layer, pattern = "l_layer_polygons:") ||
            grepl(layer, pattern = "l_layer_lines:")) {

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

  } else if(grepl(layer, pattern = "l_layer_rectangles:")) {

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
      xlim = grDevices::extendrange(c(x - width/2, x + width/2)),
      ylim = grDevices::extendrange(c(y - height/2, y + height/2))
    )
  } else if(grepl(layer, pattern = "l_layer_texts:")) {

    x <- y <- list()

    lapply(1:length(grobi$children),
           function(i){

             child <- grobi$children[[i]]
             tryCatch(
               {
                 xx <- grid::convertX(child$x, "native", valueOnly = TRUE)
                 yy <- grid::convertX(child$y, "native", valueOnly = TRUE)
               },
               error = function(e) {
                 xx <<- get_unit(child$x, "native", as.numeric = TRUE)
                 yy <<- get_unit(child$y, "native", as.numeric = TRUE)
               }
             )

             x[[i]] <<- xx
             y[[i]] <<- yy
           }
    )

    x <- unlist(x)
    y <- unlist(y)

    list(
      xlim = grDevices::extendrange(x),
      ylim = grDevices::extendrange(y)
    )
  } else {

    tryCatch(
      {
        list(
          xlim = grid::convertX(grobi$x, "native", valueOnly = TRUE),
          ylim = grid::convertY(grobi$y, "native", valueOnly = TRUE)
        )
      },
      error = function(e) {
        list(
          xlim = get_unit(grobi$x, "native", as.numeric = TRUE),
          ylim = get_unit(grobi$y, "native", as.numeric = TRUE)
        )
      }
    )
  }
}
