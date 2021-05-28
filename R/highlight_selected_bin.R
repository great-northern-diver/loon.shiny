
highlight_selected_bin_grob <- function (loon.grob, yshows, active, selected,
                                         binId, binX, binHeight, binwidth,
                                         n, swapAxes, showStackedColors, showOutlines,
                                         color, colorFill, colorOutline, loonColor){
  
  brushId <- intersect(which(selected), which(active))
  if(length(brushId) > 0) {
    
    histGrob <- grid::getGrob(loon.grob, "histogram")
    which_bin_is_affected <- which(sapply(binId, function(bi) any(bi %in% brushId)))
    
    colorOrder <- as.character(levels(as.factor(color)))
    
    sel_color <- select_color()
    
    lapply(which_bin_is_affected,
           function(i) {
             
             binName <- paste0("bin", i)
             
             if(binHeight[i] != 0) {
               
               if(!swapAxes) {
                 
                 x <- unit(mean(c(binX[i], binX[i + 1])), "native")
                 y <- unit(mean(c(0, binHeight[i])), "native")
                 
                 width <- unit(binwidth, "native")
                 height <- unit(binHeight[i], "native")
               } else {
                 
                 y <- unit(mean(c(binX[i], binX[i + 1])), "native")
                 x <- unit(mean(c(0, binHeight[i])), "native")
                 
                 height <- unit(binwidth, "native")
                 width <- unit(binHeight[i], "native")
               }
               
               isSelected  <- selected[binId[[i]]]
               
               if(showStackedColors) {
                 
                 unselectedColorBinHeight <- table(color[binId[[i]]][which(isSelected == FALSE)])
                 # reorder color bin height
                 unselectedColorBinHeight <- unselectedColorBinHeight[order(unname(sapply(names(unselectedColorBinHeight), function(name) which(colorOrder %in% name))))]
                 
                 binSelected <- length(which(isSelected == TRUE))
                 colorBinHeight <- if(yshows == "frequency") {
                   c(binSelected, unselectedColorBinHeight)
                 } else{
                   c(binSelected, unselectedColorBinHeight) / (n * binwidth)
                 }
                 names(colorBinHeight) <- c(sel_color, names(unselectedColorBinHeight))
                 
                 cumsumColorBinHeight <- c(0, cumsum(colorBinHeight))
                 
                 histGrob <<- grid::setGrob(
                   gTree = histGrob,
                   gPath = binName,
                   newGrob = gTree(
                     children = do.call(gList, 
                                        lapply(1:length(colorBinHeight), 
                                               function(i){
                                                 if(!swapAxes){
                                                   y <- unit(mean(c(cumsumColorBinHeight[i], 
                                                                    cumsumColorBinHeight[i+1])), "native")
                                                   height <- unit(colorBinHeight[i], "native")
                                                 }else{
                                                   x <- unit(mean(c(cumsumColorBinHeight[i], 
                                                                    cumsumColorBinHeight[i+1])), "native")
                                                   width <- unit(colorBinHeight[i], "native")
                                                 }
                                                 grid::rectGrob(
                                                   x = x, y = y, 
                                                   width = width, height = height, 
                                                   gp = gpar(fill = names(colorBinHeight)[i], 
                                                             col = if(showOutlines) colorOutline else NA)) 
                                               }
                                        )
                     ),
                     name = binName
                   )
                 )
               } else {
                 
                 binGrob <- grid::rectGrob(
                   x = x, y = y, 
                   width = width, height = height, 
                   gp = gpar(fill = colorFill, 
                             col = if(showOutlines) colorOutline else NA)
                 )
                 
                 binSelected <- if(yshows == "frequency") {
                   
                   length(which(isSelected == TRUE))
                 } else {
                   
                   length(which(isSelected == TRUE)) / (n * binwidth)
                 }
                 
                 if(!swapAxes) {
                   
                   y <- unit(mean(c(0, binSelected)), "native")
                   height <- unit(binSelected, "native") 
                 } else {
                   
                   x <- unit(mean(c(0, binSelected)), "native")
                   width <- unit(binSelected, "native")
                 }
                 
                 histGrob <<- grid::setGrob(
                   gTree = histGrob,
                   gPath = binName,
                   newGrob = gTree(
                     children = gList(
                       binGrob,
                       grid::rectGrob(
                         x = x, y = y, 
                         width = width, height = height, 
                         gp = gpar(fill = sel_color, 
                                   col = if(showOutlines) colorOutline else NA)
                       )
                     ),
                     name = binName
                   )
                 )
               }
             } else grob(name = binName)
           }
    )
    
    grid::setGrob(
      gTree = loon.grob,
      gPath = "histogram",
      newGrob = histGrob
    )
  } else loon.grob
}
