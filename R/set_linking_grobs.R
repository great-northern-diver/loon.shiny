set_linking_grobs <- function(loon_grob, output_grob, linkedInfo, tabPanelName, loon_color, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("set_linking_grobs", obj)
}

set_linking_grobs.l_plot <- function(loon_grob, output_grob, linkedInfo, tabPanelName, loon_color, ...) {

  # pointsTree name
  scatterplot_grob <- getGrob(loon_grob, "scatterplot")
  pointsTree_name <- scatterplot_grob$childrenOrder

  if(pointsTree_name != "points: missing glyphs") {
    
    linkedKey <- linkedInfo$linkingKey[[tabPanelName]]
    linkedStates <- linkedInfo$linkingStates[[tabPanelName]]

    if(length(linkedStates) > 0 & length(linkedKey) > 0) {
      
        args <- list(...)
        roundings <- args$roundings

        new_loon_grob <- getGrob(loon_grob, pointsTree_name)
        new_output_grob <- getGrob(output_grob, pointsTree_name)
        
        color <- linkedInfo$color[linkedKey]
        size <- linkedInfo$size[linkedKey]
        pch <- linkedInfo$pch[linkedKey]
        selected <- linkedInfo$selected[linkedKey]
        active <- linkedInfo$active[linkedKey]

        lapply(1:length(linkedKey),
               function(i) {
                 
                 grobi <- new_loon_grob$children[[i]]
                 
                 if(str_detect(grobi$name, "primitive_glyph")) {
                   
                   # set pch, size and color
                   
                   if("glyph" %in% linkedStates & !is.null(pch)) {
                     grobi_pch <- pch[i]
                     if(!is.numeric(grobi_pch)) grobi_pch <- grobi$pch
                   } else grobi_pch <- grobi$pch
                   
                   if("color" %in% linkedStates & !is.null(color)) {
                     grobi_color <- color[i]
                   } else {
                     if(grobi_pch %in% 21:24) {
                       grobi_color <- grobi$gp$fill
                     } else {
                       grobi_color <- grobi$gp$col
                     }
                   }
                   
                   if("size" %in% linkedStates & !is.null(size)) {
                     grobi_size <- size[i]
                   } else grobi_size <- grobi$gp$cex
                   
                   grobi <- editGrob(
                     grob = grobi,
                     gp = if(grobi_pch %in% 21:24) {
                       gpar(
                         fill = grobi_color,
                         cex = grobi_size,
                         col = loon_color$foreground_color[1]
                       )
                     } else {
                       gpar(
                         col = grobi_color,
                         cex = grobi_size
                       )
                     },
                     pch = grobi_pch
                   )
                   
                   # set deactive and reactive
                   if("active" %in% linkedStates) {
                     grobi <- if(!active[i]) {
                       do.call(grob, getGrobArgs(grobi))
                     } else {
                       do.call(pointsGrob, getGrobArgs(grobi))
                     }
                   }
                   
                   new_loon_grob$children[[i]] <<- grobi
                   
                   new_output_grob$children[[i]] <<- if("selected" %in% linkedStates & selected[i]) {
                     
                     editGrob(
                       grob = grobi,
                       gp = if(grobi_pch %in% 21:24) {
                         gpar(
                           fill = loon_color$select_color[1],
                           cex = grobi_size,
                           col = loon_color$foreground_color[1]
                         )
                       } else {
                         gpar(
                           col = loon_color$select_color[1],
                           cex = grobi_size
                         )
                       }
                     )
                   } else grobi
                   
                   
                 } else if(str_detect(grobi$name, "serialaxes_glyph"))  {
                   
                   # reset boundary
                   boundary_grob <- getGrob(grobi, "boundary")
                   if(is.null(boundary_grob)) {
                     boundary_grob <- getGrob(grobi, "boundary: polylineGrob arguments")
                   }
                   
                   # axes serialaxes
                   axes_grob <- getGrob(grobi, "axes")
                   if(is.null(axes_grob)) {
                     axes_grob <- getGrob(grobi, "axes: polylineGrob arguments")
                   } 
                   
                   serialaxes_grob <- getGrob(grobi, "polyline")
                   if(is.null(serialaxes_grob)) {
                     serialaxes_grob <- getGrob(grobi, "polyline: showArea")
                     serialaxes_grob_name <-  "polyline: showArea"
                   } else {
                     serialaxes_grob_name <-  "polyline"
                   }
                   
                   # set size
                   if("size" %in% linkedStates & !is.null(size)) {
                     
                     rounding <- roundings[[i]][[1]]
                     
                     boundary_grob <- editGrob(
                       grob = boundary_grob,
                       x = get_unit(boundary_grob$x, as.numeric = FALSE) +
                         unit(rounding$boundary_grob_rounding$x * sqrt(size[i]/default_size()), "mm"),
                       y = get_unit(boundary_grob$y, as.numeric = FALSE) +
                         unit(rounding$boundary_grob_rounding$y * sqrt(size[i]/default_size()), "mm")
                     )
                     
                     axes_grob <- editGrob(
                       grob = axes_grob,
                       x = get_unit(axes_grob$x, as.numeric = FALSE) +
                         unit(rounding$axes_grob_rounding$x * sqrt(size[i]/default_size()), "mm"),
                       y = get_unit(axes_grob$y, as.numeric = FALSE) +
                         unit(rounding$axes_grob_rounding$y * sqrt(size[i]/default_size()), "mm")
                     )
                     
                     serialaxes_grob <- editGrob(
                       grob = serialaxes_grob,
                       x = get_unit(serialaxes_grob$x, as.numeric = FALSE) +
                         unit(rounding$serialaxes_grob_rounding$x * sqrt(size[i]/default_size()), "mm"),
                       y = get_unit(serialaxes_grob$y, as.numeric = FALSE) +
                         unit(rounding$serialaxes_grob_rounding$y * sqrt(size[i]/default_size()), "mm")
                     )
                   }
                   
                   grobi <- if(str_detect(grobi$name, "parallel")){
                     gTree(
                       children = gList(
                         boundary_grob,
                         axes_grob,
                         serialaxes_grob
                       ),
                       name =  grobi$name
                     )
                   } else {
                     gTree(
                       children = gList(
                         serialaxes_grob,
                         boundary_grob,
                         axes_grob
                       ),
                       name =  grobi$name
                     )
                   }
                   
                   # set color
                   if("color" %in% linkedStates & !is.null(color)) {
                     serialaxes_grob <- getGrob(grobi, serialaxes_grob_name)
                     if(serialaxes_grob_name == "polyline: showArea") {
                       serialaxes_grob$gp$fill <- color[i]
                     } else {
                       serialaxes_grob$gp$col <- color[i]
                     }
                     
                     grobi <- setGrob(
                       gTree = grobi,
                       gPath = serialaxes_grob_name,
                       newGrob = serialaxes_grob
                     )
                   }
                   
                   # set deactive and reactive
                   if("active" %in% linkedStates) {
                     if(!active[i]) {
                       grobi <- gTree(
                         children = gList(
                           do.call(grob, getGrobArgs(grobi$children[[1]])),
                           do.call(grob, getGrobArgs(grobi$children[[2]])),
                           do.call(grob, getGrobArgs(grobi$children[[3]]))
                         ),
                         name = grobi$name
                       )
                     } else {
                       gTree_names <- grobi$childrenOrder
                       
                       grobi <- if("polyline" %in% gTree_names) {
                         
                         setGrob(
                           gTree = grobi,
                           gPath = "polyline",
                           newGrob = do.call(linesGrob, getGrobArgs(getGrob(grobi, "polyline")))
                         )
                       } else if("polyline: showArea" %in% gTree_names) {
                         
                         setGrob(
                           gTree = grobi,
                           gPath = "polyline: showArea",
                           newGrob = do.call(polygonGrob, getGrobArgs(getGrob(grobi, "polyline: showArea")))
                         )
                       } else stop("serialaxes name does not match")
                       
                       if("boundary" %in% gTree_names) {
                         
                         grobi <- setGrob(
                           gTree = grobi,
                           gPath = "boundary",
                           newGrob = do.call(polylineGrob, getGrobArgs(getGrob(grobi, "boundary")))
                         )
                       }
                       
                       if("axes" %in% gTree_names) {
                         
                         grobi <- setGrob(
                           gTree = grobi,
                           gPath = "axes",
                           newGrob = do.call(polylineGrob, getGrobArgs(getGrob(grobi, "axes")))
                         )
                       }
                     }
                   }
                   
                   new_loon_grob$children[[i]] <<- grobi
                   new_output_grob$children[[i]] <<- if("selected" %in% linkedStates & selected[i]) {
                     
                     serialaxes_grob <- getGrob(grobi, serialaxes_grob_name)
                     
                     if(serialaxes_grob_name == "polyline: showArea") {
                       serialaxes_grob$gp$fill <- loon_color$select_color[1]
                     } else {
                       serialaxes_grob$gp$col <- loon_color$select_color[1]
                     }
                     
                     setGrob(
                       gTree = grobi,
                       gPath = serialaxes_grob_name,
                       newGrob = serialaxes_grob
                     )
                   } else grobi
                   
                 } else if(str_detect(grobi$name, "polygon_glyph")) {
                   
                   # set size
                   if("size" %in% linkedStates & !is.null(size)) {
                     rounding <- roundings[[i]][[1]]
                     
                     
                     grobi <- editGrob(
                       grob = grobi,
                       x = get_unit(grobi$x, as.numeric = FALSE) +
                         unit(rounding$x * sqrt(size[i]/default_size()), "mm"),
                       y = get_unit(grobi$y, as.numeric = FALSE) +
                         unit(rounding$y * sqrt(size[i]/default_size()), "mm")
                     )
                   }
                   
                   # set color
                   if("color" %in% linkedStates & !is.null(color)) {
                     grobi <- editGrob(
                       grob = grobi,
                       gp = gpar(
                         fill = color[i],
                         col = color[i],
                         fontsize = grobi$gp$lwd
                       )
                     )
                   }
                   
                   # set deactive and reactive
                   if("active" %in% linkedStates) {
                     grobi <- if(!active[i]) {
                       
                       do.call(grob, getGrobArgs(grobi))
                     } else {
                       
                       if(str_detect(grobi$name, "showArea")) {
                         
                         do.call(polygonGrob, getGrobArgs(grobi))
                       } else {
                         
                         do.call(polylineGrob, getGrobArgs(grobi))
                       }
                     }
                   }
                   
                   new_loon_grob$children[[i]] <<- grobi
                   new_output_grob$children[[i]] <<- if("selected" %in% linkedStates & selected[i]) {
                     
                     editGrob(
                       grob = grobi,
                       gp = gpar(
                         fill = loon_color$select_color[1],
                         col = loon_color$select_color[1],
                         fontsize = grobi$gp$lwd
                       )
                     )
                   } else grobi
                   
                 } else if(str_detect(grobi$name, "pointrange_glyph")) {
                   
                   point_grob <- getGrob(grobi, "point")
                   line_grob <- getGrob(grobi, "range")
                   
                   # set glyph
                   if("glyph" %in% linkedStates & !is.null(pch)) {
                     
                     point_grob_pch <- pch[i]
                     if(!is.numeric(point_grob_pch)) point_grob_pch <- point_grob$pch
                     point_grob$pch <- point_grob_pch
                     
                     grobi <- setGrob(
                       gTree = grobi,
                       gPath = "point",
                       newGrob = point_grob
                     )
                   } else point_grob_pch <- point_grob$pch
                   
                   # set color
                   if("color" %in% linkedStates & !is.null(color)) {
                     
                     if(point_grob_pch %in% 21:24) point_grob$gp$fill <- color[i] else point_grob$gp$col <- color[i]
                     
                     line_grob$gp$col <- color[i]
                     
                     grobi <- setGrob(
                       gTree = setGrob(
                         gTree = grobi,
                         gPath = "point",
                         newGrob = point_grob
                       ),
                       gPath = "range",
                       newGrob = line_grob
                     )
                   }
                   
                   # set size
                   if("size" %in% linkedStates & !is.null(size)) {
                     
                     grobi <- setGrob(
                       gTree = grobi,
                       gPath = "point",
                       newGrob = editGrob(
                         grob = getGrob(grobi, "point"),
                         gp = if(grobi$pch %in% 21:24) {
                           gpar(
                             fill = grobi$gp$fill,
                             cex = size[i],
                             col = grobi$gp$col
                           )
                         } else {
                           gpar(
                             col = grobi$gp$col,
                             cex = size[i]
                           )
                         }
                       )
                     )
                   }
                   
                   # set deactive and reactive
                   if("active" %in% linkedStates) {
                     grobi <- if(!active[i]) {
                       gTree(
                         children = gList(
                           # point
                           do.call(grob, getGrobArgs(getGrob(grobi, "point"))),
                           # range
                           do.call(grob, getGrobArgs(getGrob(grobi, "range")))
                         ),
                         name = grobi$name
                       )
                     } else {
                       gTree(
                         children = gList(
                           # point
                           do.call(pointsGrob, getGrobArgs(getGrob(grobi, "point"))),
                           # range
                           do.call(linesGrob, getGrobArgs(getGrob(grobi, "range")))
                         ),
                         name = grobi$name
                       )
                     }
                   }
                   
                   new_loon_grob$children[[i]] <<- grobi
                   new_output_grob$children[[i]] <<- if("selected" %in% linkedStates & selected[i]) {
                     
                     point_grob <- getGrob(grobi, "point")
                     line_grob <- getGrob(grobi, "range")
                     
                     point_grob$gp$col <- loon_color$select_color[1]
                     line_grob$gp$col <- loon_color$select_color[1]
                     
                     grobi <- setGrob(
                       gTree = setGrob(
                         gTree = grobi,
                         gPath = "point",
                         newGrob = point_grob
                       ),
                       gPath = "range",
                       newGrob = line_grob
                     )
                   } else grobi
                   
                 } else if(str_detect(grobi$name, "text_glyph"))  {
                   
                   # set color and size
                   if("color" %in% linkedStates & !is.null(color)) {
                     grobi_color <- color[i]
                   } else {
                     grobi_color <- grobi$gp$col
                   }
                   
                   if("size" %in% linkedStates & !is.null(size)) {
                     grobi_size <- size[i] * loon_default_size()[["adjusted_size"]]
                   } else {
                     grobi_size <- grobi$gp$fontsize
                   }
                   
                   grobi <- editGrob(
                     grob = grobi,
                     gp = gpar(
                       col = grobi_color,
                       fontsize = grobi_size
                     )
                   )
                   
                   if("active" %in% linkedStates) {
                     grobi <- if(!active[i]) {
                       do.call(grob, getGrobArgs(grobi))
                     } else {
                       do.call(textGrob, getGrobArgs(grobi))
                     }
                   }
                   
                   new_loon_grob$children[[i]] <<- grobi
                   new_output_grob$children[[i]] <<- if("selected" %in% linkedStates & selected[i]) {
                     
                     editGrob(
                       grob = grobi,
                       gp = gpar(
                         col = loon_color$select_color[1],
                         fontsize = size[i] * loon_default_size()[["adjusted_size"]]
                       )
                     )
                   } else grobi
                   
                 } else if(str_detect(grobi$name, "image_glyph")) {
                   
                   # set color
                   if("color" %in% linkedStates & !is.null(color)) {
                     grobi <- setGrob(
                       gTree = grobi,
                       gPath = "image_border",
                       newGrob = editGrob(
                         grob = getGrob(grobi, "image_border"),
                         gp = gpar(
                           fill = color[i],
                           col =  NA
                         )
                       )
                     )
                   }
                   
                   # set size
                   if("size" %in% linkedStates & !is.null(size)) {
                     rounding <- roundings[[i]][[1]]
                     
                     image_border_grob <- getGrob(grobi, "image_border")
                     
                     image_border_grob <- editGrob(
                       grob = image_border_grob,
                       width = get_unit(image_border_grob$width, unit = "mm", as.numeric = FALSE) +
                         unit(rounding$width * sqrt(size[i]/default_size()), "cm"),
                       height = get_unit(image_border_grob$height, unit = "mm", as.numeric = FALSE) +
                         unit(rounding$height * sqrt(size[i]/default_size()), "cm")
                     )
                     
                     image_grob <- editGrob(
                       grob = getGrob(grobi, "image"),
                       width = unit(rounding$width * sqrt(size[i]/default_size()), "cm"),
                       height = unit(rounding$height * sqrt(size[i]/default_size()), "cm")
                     )
                     
                     grobi <- gTree(
                       children = gList(
                         image_border_grob,
                         image_grob
                       ),
                       name =  grobi$name
                     )
                   }
                   
                   # set deactive and reactive
                   if("active" %in% linkedStates) {
                     grobi <- if(!active[i]) {
                       gTree(
                         children = gList(
                           do.call(grob, getGrobArgs(getGrob(grobi, "image_border"))),
                           do.call(grob, getGrobArgs(getGrob(grobi, "image")))
                         ),
                         name = grobi$name
                       )
                     } else {
                       
                       raster_args <- getGrobArgs(getGrob(grobi, "image"))
                       names(raster_args) <- c("image", names(raster_args)[-1])
                       
                       gTree(
                         children = gList(
                           do.call(rectGrob, getGrobArgs(getGrob(grobi, "image_border"))),
                           do.call(rasterGrob, raster_args)
                         ),
                         name = grobi$name
                       )
                     }
                   }
                   
                   new_loon_grob$children[[i]] <<- grobi
                   new_output_grob$children[[i]] <<- if("selected" %in% linkedStates & selected[i]) {
                     
                     setGrob(
                       gTree = grobi,
                       gPath = "image_border",
                       newGrob = editGrob(
                         grob = getGrob(grobi, "image_border"),
                         gp = gpar(
                           fill = loon_color$select_color[1],
                           col =  NA
                         )
                       )
                     )
                   } else grobi
                 }
               }
        )
        
        loon_grob <- setGrob(
          gTree = loon_grob,
          gPath = pointsTree_name,
          newGrob = new_loon_grob
        )
        output_grob <- setGrob(
          gTree = output_grob,
          gPath = pointsTree_name,
          newGrob = new_output_grob
        )
        output_grob <- reorder_grob(output_grob,
                                    index = which(selected),
                                    pointsTree_name = pointsTree_name)
      }
  }
  list(
    output_grob = output_grob,
    loon_grob = loon_grob
  )
}

set_linking_grobs.l_graph <- function(loon_grob, output_grob, linkedInfo, tabPanelName, loon_color, ...) {


  linkedKey <- linkedInfo$linkingKey[[tabPanelName]]
  linkedStates <- linkedInfo$linkingStates[[tabPanelName]]
  
  if(length(linkedStates) > 0 & length(linkedKey) > 0) {
    
    args <- list(...)
    graph_edges <- args$graph_edges
    swap_in_shiny <- args$swap_in_shiny
    swap_in_loon <- args$swap_in_loon
    loon_color <- args$loon_color
    
    swap <- ((swap_in_shiny & !swap_in_loon) | (!swap_in_shiny & swap_in_loon))

    color <- linkedInfo$color[linkedKey]
    size <- linkedInfo$size[linkedKey]
    pch <- linkedInfo$pch[linkedKey]
    selected <- linkedInfo$selected[linkedKey]
    active <- linkedInfo$active[linkedKey]

    new_loon_grob <- getGrob(loon_grob, "graph nodes")
    new_output_grob <- getGrob(output_grob, "graph nodes")
    
    lapply(1:length(linkedKey),
           function(i) {
             
             grobi <- new_loon_grob$children[[i]]
             
             if("glyph" %in% linkedStates & !is.null(pch)) {
               grobi_pch <- pch[i]
               if(!is.numeric(grobi_pch)) grobi_pch <- grobi$pch
             } else grobi_pch <- grobi$pch
             
             if("color" %in% linkedStates & !is.null(color)) {
               grobi_color <- color[i]
             } else {
               if(grobi_pch %in% 21:24) {
                 grobi_color <- grobi$gp$fill
               } else {
                 grobi_color <- grobi$gp$col
               }
             }
             
             if("size" %in% linkedStates & !is.null(size)) {
               grobi_size <- size[i]
             } else grobi_size <- grobi$gp$cex
             
             grobi <- editGrob(
               grob = grobi,
               gp = if(grobi_pch %in% 21:24) {
                 gpar(
                   fill = grobi_color,
                   cex = grobi_size,
                   col = loon_color$foreground_color[1]
                 )
               } else {
                 gpar(
                   col = grobi_color,
                   cex = grobi_size
                 )
               },
               pch = grobi_pch
             )
             
             new_loon_grob$children[[i]] <<- grobi
             
             new_output_grob$children[[i]] <<- if("selected" %in% linkedStates & selected[i]) {
               
               editGrob(
                 grob = grobi,
                 gp = if(grobi_pch %in% 21:24) {
                   gpar(
                     fill = loon_color$select_color[1],
                     cex = grobi_size,
                     col = loon_color$foreground_color[1]
                   )
                 } else {
                   gpar(
                     col = loon_color$select_color[1],
                     cex = grobi_size
                   )
                 }
               )
             } else grobi
           }
    )
    
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "graph nodes",
      newGrob = new_loon_grob
    )
    
    output_grob <- setGrob(
      gTree = output_grob,
      gPath = "graph nodes",
      newGrob = new_output_grob
    )
    
    if("active" %in% linkedStates) {
      
      # reactive
      output_grob <- set_reactive_grob(
        loon_grob = output_grob,
        index = which(active),
        graph_edges = graph_edges,
        swap = swap
      )
      
      loon_grob <- set_reactive_grob(
        loon_grob = loon_grob,
        index = which(active),
        graph_edges = graph_edges,
        swap = FALSE
      )
      
      # deactive
      output_grob <- set_deactive_grob(
        loon_grob = output_grob,
        index = which(!active)
      )
      
      loon_grob <- set_deactive_grob(
        loon_grob = loon_grob,
        index = which(!active)
      )
    }
    
    output_grob <- reorder_grob(output_grob,
                                index = which(selected))
  }
  
  list(
    output_grob = output_grob,
    loon_grob = loon_grob
  )
}

set_linking_grobs.l_hist <- function(loon_grob, output_grob, linkedInfo, tabPanelName, loon_color = NULL, ...) {
  
  linkedKey <- linkedInfo$linkingKey[[tabPanelName]]
  linkedStates <- linkedInfo$linkingStates[[tabPanelName]]
  
  if(length(linkedStates) > 0 & length(linkedKey) > 0) {
    
    args <- list(...)
    loonWidgets_info <- args$loonWidgets_info
    
    color <- if("color" %in% linkedStates) linkedInfo$color[linkedKey] else loonWidgets_info$color
    selected <- if("selected" %in% linkedStates) linkedInfo$selected[linkedKey] else loonWidgets_info$selected
    active <- if("active" %in% linkedStates) linkedInfo$active[linkedKey] else loonWidgets_info$active
    
    N <- length(loonWidgets_info$linkingKey)
    
    binInfo <- get_binInfo(data = loonWidgets_info$x, origin = loonWidgets_info$origin, active = active, 
                           binwidth = loonWidgets_info$binwidth, yshows = loonWidgets_info$yshows)
    binId <- binInfo$binId
    binX <- binInfo$binX
    binHeight <- binInfo$binHeight
    
    bin_xy <- get_bin_xy(binX = binX, binId = binId, binwidth = loonWidgets_info$binwidth,
                         yshows = loonWidgets_info$yshows, color = color, n = N)
    
    # build grob at the end ---------------------------------------------------------------
    output_grob <- get_hist_grob(loon_grob = output_grob, yshows = loonWidgets_info$yshows, 
                                 binId = binId, binX = binX, binHeight = binHeight, binwidth = loonWidgets_info$binwidth,
                                 n = N, swapAxes = loonWidgets_info$swap_in_shiny,
                                 showStackedColors = loonWidgets_info$showStackedColors, showOutlines = loonWidgets_info$showOutlines,
                                 color = color, colorFill = loonWidgets_info$colorFill, colorOutline = loonWidgets_info$colorOutline)
    
    # highlight selected bin
    output_grob <- highlight_selected_bin_grob(loon_grob = output_grob, yshows = loonWidgets_info$yshows, active = active, selected = selected,
                                               binId = binId, binX = binX, binHeight = binHeight, binwidth = loonWidgets_info$binwidth, n = N, 
                                               swapAxes = loonWidgets_info$swap_in_shiny, showStackedColors = loonWidgets_info$showStackedColors, 
                                               showOutlines = loonWidgets_info$showOutlines,
                                               color = color, colorFill = loonWidgets_info$colorFill, colorOutline = loonWidgets_info$colorOutline,
                                               loon_color = loonWidgets_info$loon_color)
    
    loonWidgets_info$color <- color
    loonWidgets_info$selected <- selected
    loonWidgets_info$active <- active
  }
  
  list(
    output_grob =   output_grob,
    loonWidgets_info = loonWidgets_info
  )
}

set_linking_grobs.l_serialaxes <- function(loon_grob, output_grob, linkedInfo, tabPanelName,  loon_color) {
  
  linkedKey <- linkedInfo$linkingKey[[tabPanelName]]
  linkedStates <- linkedInfo$linkingStates[[tabPanelName]]
  
  if(length(linkedStates) > 0 & length(linkedKey) > 0) {
    
    loon_grob_showArea <- get_showArea(loon_grob)
    output_grob_showArea <- get_showArea(output_grob)
    
    color <- linkedInfo$color[linkedKey]
    selected <- linkedInfo$selected[linkedKey]
    active <- linkedInfo$active[linkedKey]
    size <- linkedInfo$size[linkedKey]
    
    loon_grob_axes_gPath <- if(get_axesLayout(loon_grob) == "parallel") "parallelAxes" else "radialAxes"
    output_grob_axes_gPath <- if(get_axesLayout(output_grob) == "parallel") "parallelAxes" else "radialAxes"
    
    new_loon_grob <- getGrob(loon_grob, loon_grob_axes_gPath)
    new_output_grob <- getGrob(output_grob, output_grob_axes_gPath)

    lapply(1:length(linkedKey),
           function(i) {

             loon_grobi <- new_loon_grob$children[[i]]
             output_grobi <- new_output_grob$children[[i]]
             
             if("color" %in% linkedStates & !is.null(color)) {
               grobi_color <- color[i]
             } else {
               
               grobi_color <- if(loon_grob_showArea) {
                 loon_grobi$gp$fill
               } else loon_grobi$gp$col
             }
             
             grobi_size <- if("size" %in% linkedStates & !is.null(size)) {
               size[i]
             } else {
               loon_grobi$gp$lwd
             }
             
             loon_grobi <- editGrob(
               grob = loon_grobi,
               gp = if(loon_grob_showArea) {
                 gpar(fill = grobi_color, col = NA)
               } else {
                 gpar(col = grobi_color, lwd = grobi_size)
               } 
             )
             
             output_grobi <- editGrob(
               grob = output_grobi,
               gp = if(output_grob_showArea) {
                 gpar(fill = grobi_color, col = NA)
               } else {
                 gpar(col = grobi_color, lwd = grobi_size)
               }
             )

             if("active" %in% linkedStates) {
               
               loon_grobi <- if(!active[i]) {
                 do.call(grob, getGrobArgs(loon_grobi))
               } else {
                 if(loon_grob_showArea) {
                   do.call(polygonGrob, getGrobArgs(loon_grobi))
                 } else {
                   do.call(linesGrob, getGrobArgs(loon_grobi))
                 }
               }
               
               output_grobi <- if(!active[i]) {
                 do.call(grob, getGrobArgs(output_grobi))
               } else {
                 if(output_grob_showArea) {
                   do.call(polygonGrob, getGrobArgs(output_grobi))
                 } else {
                   do.call(linesGrob, getGrobArgs(output_grobi))
                 }
               }
             }
             
             new_loon_grob$children[[i]] <<- loon_grobi
             
             new_output_grob$children[[i]] <<- if("selected" %in% linkedStates & selected[i]) {
               
               editGrob(
                 grob = output_grobi,
                 gp = if(output_grob_showArea) {
                   gpar(fill = loon_color$select_color[1], col = NA)
                 } else {
                   gpar(col = loon_color$select_color[1], lwd = grobi_size)
                 }
               )
             } else output_grobi
           }
    )

    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = loon_grob_axes_gPath,
      newGrob = new_loon_grob
    )
    
    output_grob <- setGrob(
      gTree = output_grob,
      gPath = output_grob_axes_gPath,
      newGrob = new_output_grob
    )
    
    output_grob <- reorder_grob(output_grob,
                                index = which(selected),
                                axes_gPath = output_grob_axes_gPath)
  }
  
  list(
    output_grob = output_grob,
    loon_grob = loon_grob
  )
}
