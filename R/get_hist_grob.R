get_hist_grob <- function(loon.grob, yshows, binId, binX, binHeight, binwidth, n, swapAxes, showStackedColors, showOutlines, 
                          color, colorFill, colorOutline) {
  
  colorOrder <- as.character(levels(as.factor(color)))
  
  histGrob <- gTree(
    children = do.call (
      grid::gList,
      lapply(seq(length(binHeight)),
             function(i){
               
               if(binHeight[i] != 0){
                 
                 if(!swapAxes) {
                   
                   x <- grid::unit(mean(c(binX[i], binX[i + 1])), "native")
                   y <- grid::unit(mean(c(0, binHeight[i])), "native")
                   
                   width <- grid::unit(binwidth, "native")
                   height <- grid::unit(binHeight[i], "native")
                 } else {
                   
                   y <- grid::unit(mean(c(binX[i], binX[i + 1])), "native")
                   x <- grid::unit(mean(c(0, binHeight[i])), "native")
                   
                   height <- grid::unit(binwidth, "native")
                   width <- grid::unit(binHeight[i], "native")
                 }
                 
                 if(showStackedColors) {
                   
                   colorBinHeight <- if(yshows == "frequency") table(color[binId[[i]]]) else table(color[binId[[i]]])/(n * binwidth)
                   # reorder color bin height
                   colorBinHeight <- colorBinHeight[
                     order(unname(sapply(names(colorBinHeight), 
                                         function(name) 
                                           which(colorOrder %in% name))))]
                   
                   cumsumColorBinHeight <- c(0, cumsum(colorBinHeight))
                   
                   gTree(
                     children =   do.call(
                       grid::gList, 
                       lapply(seq(length(colorBinHeight)), 
                              function(i){
                                if(!swapAxes) {
                                  y <- grid::unit(mean(c(cumsumColorBinHeight[i], 
                                                         cumsumColorBinHeight[i+1])), "native")
                                  height <- grid::unit(colorBinHeight[i], "native")
                                } else {
                                  x <- grid::unit(mean(c(cumsumColorBinHeight[i], 
                                                         cumsumColorBinHeight[i+1])), "native")
                                  width <- grid::unit(colorBinHeight[i], "native")
                                }
                                grid::rectGrob(
                                  x = x, y = y, width = width, height = height, 
                                  gp = grid::gpar(fill = names(colorBinHeight)[i], 
                                                  col = if(showOutlines) colorOutline else NA)) 
                              }
                       )
                     ),
                     name = paste0("bin", i)
                   )
                 } else {
                   grid::rectGrob(
                     x = x, 
                     y = y, 
                     width = width, 
                     height = height, 
                     gp = grid::gpar(fill = colorFill, 
                                     col = if(showOutlines) colorOutline else NA),
                     name = paste0("bin", i)
                   )
                 }
               } else grid::grob(name = paste0("bin", i))
             }
      )
    ),
    name = "histogram"
  )
  
  grid::setGrob(
    gTree = loon.grob,
    gPath = "histogram",
    newGrob = histGrob
  )
}
