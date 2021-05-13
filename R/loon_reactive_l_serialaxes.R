loon_reactive.l_serialaxes <- function(loon_grob, output_grob, linkingInfo, buttons, position, selectBy,
                                       linkingGroup, input, tabPanelName, output_info) {
  
  if(!is.null(output_grob) & input[["navBarPage"]] != tabPanelName) {
    
    loonWidgets_info <- output_info$loonWidgets_info
    
    if(linkingGroup != "none") {
      
      grobs <- set_linking_grobs(
        loon_grob = loon_grob,
        output_grob = output_grob,
        linkedInfo = linkingInfo[[linkingGroup]],
        tabPanelName = tabPanelName,
        loon_color = loonWidgets_info$loon_color
      )
      
      selected <- linkingInfo[[linkingGroup]]$selected
      brush_id <- which(selected)
      select_by_color <- linkingInfo[[linkingGroup]]$select_by_color
      
      output_grob <- grobs$output_grob
      loon_grob <- grobs$loon_grob
      
      loonWidgets_info <- update_loonWidgets_info(loonWidgets_info, 
                                                  linkedInfo = linkingInfo[[linkingGroup]],         
                                                  tabPanelName = tabPanelName)
      
    } else {
      
      brush_id <- output_info$brush_id
      select_by_color <- output_info$select_by_color
    }
  } else {
    
    input$plot_brush
    input$plot_click
    
    isFirst_draw <- is.null(output_grob)
    output_grob <- loon_grob
    loonWidgets_info <- output_info$loonWidgets_info
    
    axesLayout_in_shiny <- input[[paste0(tabPanelName, "axesLayout")]]
    axesLayout_in_loon <- loonWidgets_info$axesLayout
    
    plotShow <- input[[paste0(tabPanelName, "plot")]]
    showGuides <- "showGuides" %in% plotShow
    showAxes <- "showAxes" %in% plotShow
    showAxesLabels <- "showAxesLabels" %in% plotShow
    showLabels <- "showLabels" %in% plotShow
    showArea <- "showArea" %in% plotShow
    andrews <- "andrews" %in% plotShow
    
    title <- loonWidgets_info$title
    title_gPath <- if(!is.null(getGrob(output_grob, "title"))) {
      "title"
    } else {
      "title: textGrob arguments"
    }
    
    loon_default_serialaxes_args <- loon_default_serialaxes_args()
    
    if(showLabels & title != "") {
      title_grob <- textGrob(
        name = title_gPath,
        label = title,
        y = unit(1, "npc") - unit(.8, "lines"),
        gp = gpar(fontsize = loon_default_serialaxes_args$title_fontsize,
                  fontface="bold"),
        vjust = .5
      )
    } else {
      
      title_grob <- grob(name = title_gPath)
    }
    
    output_grob <- setGrob(
      gTree = output_grob,
      gPath = title_gPath,
      newGrob = title_grob
    )
    
    scaling <- input[[paste0(tabPanelName, "scaling")]]
    scaledActiveData <- switch(scaling, 
                               "variable" = loonWidgets_info$scaledActiveData_variable,
                               "observation" = loonWidgets_info$scaledActiveData_observation,
                               "data" = loonWidgets_info$scaledActiveData_data,
                               "none" = loonWidgets_info$scaledActiveData_none)
    
    N <- loonWidgets_info$N
    len.xaxis <- loonWidgets_info$len_seqName
    axesLabels <- loonWidgets_info$seqName
    andrewsSeriesLength <- loonWidgets_info$andrewsSeriesLength
    
    if(andrews) {
      axesLabels <- round(seq(-pi, pi, length.out = len.xaxis), 2)
      fourierTrans <- loonWidgets_info$fourierTrans
      
      scaledActiveData <- as.matrix(scaledActiveData) %*% fourierTrans$matrix
      
      dataRange <- range(scaledActiveData, na.rm = TRUE)
      d <- if(diff(dataRange) == 0) 1 else diff(dataRange)
      
      scaledActiveData <- (scaledActiveData - min(scaledActiveData, na.rm = TRUE))/d
    }
    
    # set layout axes, showAxes, showGuides, ...
    if(axesLayout_in_shiny == "parallel") {
      
      xaxis <- seq(0, 1, length.out =  len.xaxis)
      
      axes_gPath <- if(axesLayout_in_shiny == axesLayout_in_loon) "parallelAxes" else "radialAxes"
      yaxis <- grid.pretty(loonWidgets_info$ylim)
      len.yaxis <- length(yaxis)
      
      # set guides -----------------------------------------------------------
      
      guides_grob <- if(showGuides) {
        
        gTree(
          children = do.call(
            gList,
            lapply(seq(len.xaxis + len.yaxis + 1),
                   function(i) {
                     if(i == 1){
                       rectGrob(gp = gpar(col = NA, fill = loon_default_serialaxes_args$guides_background),
                                name = "bounding box")
                     } else if( i > 1 && i<= (1 + len.xaxis)){
                       condGrob(
                         test = showAxes,
                         grobFun = linesGrob,
                         name = paste("x axis", i - 1),
                         x = unit(rep(xaxis[i - 1],2 ), "native"),
                         y =  unit(c(0, 1), "native"),
                         gp = gpar(col =  loon_default_serialaxes_args$line_color1,
                                   lwd = loon_default_serialaxes_args$guideLine_width)
                       )
                     } else {
                       linesGrob(
                         x = unit(c(0, 1), "native"),
                         y =  unit(rep(yaxis[i - (1 + len.xaxis)],2 ), "native"),
                         gp = gpar(col =loon_default_serialaxes_args$line_color1,
                                   lwd = loon_default_serialaxes_args$guideLine_width),
                         name = paste("y axis", i - (1 + len.xaxis))
                       )
                     }
                   })),
          name = "guides"
        )
      } else {
        
        gTree(
          children =  do.call(
            gList,
            lapply(seq(len.xaxis),
                   function(i) {
                     condGrob(
                       test = showAxes,
                       grobFun = linesGrob,
                       name = paste("x axis", i),
                       x = unit(rep(xaxis[i],2 ), "native"),
                       y =  unit(c(0, 1), "native"),
                       gp = gpar(col =  loon_default_serialaxes_args$line_color2,
                                 lwd = loon_default_serialaxes_args$guideLine_width)
                     )
                   }
            )
          ),
          name = "guides"
        )
      }
      loonWidgets_info$showGuides <- showGuides
      output_grob <- setGrob(
        gTree = output_grob,
        gPath = "guides",
        newGrob = guides_grob
      )
      
      # set labels -----------------------------------------------------------
      labels_grob <- gTree(
        children = do.call(
          gList,
          lapply(seq(len.xaxis),
                 function(i) {
                   condGrob(
                     test = showAxesLabels,
                     grobFun = textGrob,
                     label = axesLabels[i],
                     name = paste("label", i),
                     x = unit(xaxis[i], "native"),
                     y = unit(0, "npc") + unit(1.2, "lines"),
                     gp = gpar(fontsize = loon_default_serialaxes_args$label_fontsize), vjust = 1
                   )
                 }
          )
        ),
        name = "labels"
      )
      
      loonWidgets_info$showLabels <- showLabels
      
      output_grob <- setGrob(
        gTree = output_grob,
        gPath = "labels",
        newGrob = labels_grob
      )
      
      if(andrews) {
        len.xaxis <- andrewsSeriesLength
        x.axis <- seq(0, 1, length.out = len.xaxis)
      } else {
        x.axis <- xaxis
      }
      # set axes -----------------------------------------------------------
      axes_grob <- gTree(
        children = gList(
          do.call(
            gList,
            lapply(seq_len(N),
                   function(i){
                     if (showArea) {
                       
                       xx <- unit(c(x.axis, rev(x.axis)), "native")
                       yy <- unit(c(scaledActiveData[i, ], rep(0, len.xaxis)), "native")
                       
                       loonWidgets_info$x[[i]] <<- xx
                       loonWidgets_info$y[[i]] <<- yy
                       
                       polygonGrob(
                         x = xx,
                         y = yy,
                         name = paste("polyline: showArea", i),
                         gp = gpar(fill = loonWidgets_info$color[i],
                                   col = NA)
                       )
                     } else {
                       
                       xx <- unit(x.axis, "native")
                       yy <- unit(scaledActiveData[i, ], "native")
                       
                       loonWidgets_info$x[[i]] <<- xx
                       loonWidgets_info$y[[i]] <<- yy
                       
                       linesGrob(
                         x = xx,
                         y = yy,
                         name = paste("polyline", i),
                         gp = gpar(
                           col = loonWidgets_info$color[i],
                           lwd = if(is.na(loonWidgets_info$size[i])) loon_default_serialaxes_args$linewidth_default else loonWidgets_info$size[i]
                         )
                       )
                     }
                   }
            )
          )
        ),
        name = axes_gPath
      )
      loonWidgets_info$showAxes <- showAxes
      
      output_grob <- setGrob(
        gTree = output_grob,
        gPath = axes_gPath,
        newGrob = axes_grob
      )
    } else if(axesLayout_in_shiny == "radial") {
      
      xpos <- unit(0.5, "native")
      ypos <- unit(0.5, "native")
      radius <- loon_default_serialaxes_args$radius
      angle <- seq(0, 2*pi, length.out = len.xaxis + 1)[1:len.xaxis]
      
      axes_gPath <- if(axesLayout_in_shiny == axesLayout_in_loon) "radialAxes" else "parallelAxes"
      # set guides ---------------------------------------------------------
      guides_grob <- if(showGuides) {
        
        gTree(
          children = gList(
            rectGrob(gp = gpar(col = NA, fill = loon_default_serialaxes_args$guides_background),
                     name = "bounding box"),
            polygonGrob(xpos + unit(radius * cos(seq(0, 2*pi, length=101)), "npc"),
                        ypos + unit(radius * sin(seq(0, 2*pi, length=101)), "npc"),
                        gp = gpar(fill = NA, col = l_getOption("guidelines"),
                                  lwd = loon_default_serialaxes_args$guideLine_width),
                        name = "bounding line" # TODO find line width
            ),
            condGrob(
              test = showAxes,
              grobFun = polylineGrob,
              name = "axes",
              x = xpos + unit(c(rep(0, len.xaxis) ,radius * cos(angle)), "npc"),
              y =  ypos + unit(c(rep(0, len.xaxis) ,radius * sin(angle)), "npc"),
              id = rep(1:len.xaxis, 2),
              gp = gpar(col = loon_default_serialaxes_args$line_color1,
                        lwd = loon_default_serialaxes_args$guideLine_width)   # TODO Again with width loon should use guide colours
            )
          ),
          name = "guides"
        )
      } else {
        
        gTree(
          children = gList(
            condGrob(
              test = showAxes,
              grobFun = polylineGrob,
              name = "axes",
              x = unit(c(rep(0, len.xaxis) ,radius * cos(angle)), "npc") + xpos,
              y = unit(c(rep(0, len.xaxis) ,radius * sin(angle)), "npc") + ypos,
              id = rep(1:len.xaxis, 2),
              gp = gpar(col = loon_default_serialaxes_args$line_color2,
                        lwd = loon_default_serialaxes_args$guideLine_width)
            )
          ), name = "guides"
        )
      }
      loonWidgets_info$showGuides <- showGuides
      output_grob <- setGrob(
        gTree = output_grob,
        gPath = "guides",
        newGrob = guides_grob
      )
      
      # set labels ---------------------------------------------------------
      labels_grob <- gTree(
        children = do.call(
          gList,
          lapply(1:(len.xaxis),
                 function(i) {
                   condGrob(
                     test = showAxesLabels,
                     grobFun = textGrob,
                     name = paste("label", i),
                     label = axesLabels[i],
                     x = unit((radius + loon_default_serialaxes_args$radius_offset) * cos(angle[i]), "npc") + xpos,
                     y = unit((radius + loon_default_serialaxes_args$radius_offset) * sin(angle[i]), "npc") + ypos,
                     gp = gpar(fontsize = loon_default_serialaxes_args$label_fontsize), vjust = 0.5
                   )
                 }
          )
        ),
        name = "labels"
      )
      loonWidgets_info$showLabels <- showLabels
      
      output_grob <- setGrob(
        gTree = output_grob,
        gPath = "labels",
        newGrob = labels_grob
      )
      
      # set axes ---------------------------------------------------------
      if(andrews) {
        angle <- seq(0, 2*pi, length.out = andrewsSeriesLength + 1)[1:andrewsSeriesLength]
      }
      axes_grob <- gTree(
        children = do.call(
          gList,
          lapply(seq_len(N),
                 function(i){
                   
                   radialxais <- radius * scaledActiveData[i, ] * cos(angle)
                   radialyais <- radius * scaledActiveData[i, ] * sin(angle)
                   
                   xx <- xpos + unit(c(radialxais, radialxais[1]), "npc")
                   yy <- ypos + unit(c(radialyais, radialyais[1]), "npc")
                   
                   loonWidgets_info$x[[i]] <<- xx
                   loonWidgets_info$y[[i]] <<- yy
                   
                   if(showArea){
                     
                     polygonGrob(
                       x = xx,
                       y = yy,
                       name = paste("polyline: showArea", i),
                       gp = gpar(fill = loonWidgets_info$color[i], col = NA)
                     )
                   } else {
                     
                     linesGrob(
                       x = xx,
                       y = yy,
                       name = paste("polyline", i),
                       gp = gpar(
                         col = loonWidgets_info$color[i],
                         lwd = if(is.na(loonWidgets_info$size[i])) loon_default_serialaxes_args$linewidth_default else loonWidgets_info$size[i]
                       )
                     )
                   }
                 }
          )
        ),
        name = axes_gPath
      )
      
      loonWidgets_info$showAxes <- showAxes
      
      output_grob <- setGrob(
        gTree = output_grob,
        gPath = axes_gPath,
        newGrob = axes_grob
      )
    } else NULL
    
    loon_color <- loonWidgets_info$loon_color
    linkingGroup <- input[[paste0(tabPanelName, "linkingGroup")]]
    default_serialaxes <- get_default_serialaxes(axesLayout_in_shiny)
    
    viewPort <- vpStack(
      plotViewport(margins = loon_default_serialaxes_args$margins, name = "plotViewport"),
      dataViewport(xscale = default_serialaxes$xscale,
                   yscale = default_serialaxes$yscale,
                   name = "dataViewport")
    )
    
    # sweeping or brushing
    brush_id <- if(isFirst_draw) {
      
      output_info$brush_id
    } else {
      
      if(is.null(input$plot_brush) & is.null(input$plot_click)) {
        
        output_info$brush_id
      } else {
        
        get_brush_id(
          loon_grob = output_grob,
          coord = list(
            x = loonWidgets_info$x,
            y = loonWidgets_info$y
          ),
          position = position,
          brush_info = input$plot_brush,
          vp = viewPort,
          axesLayout_in_shiny = axesLayout_in_shiny
        )
      }
    }
    
    sticky <- input[[paste0(tabPanelName, "sticky")]]
    select_by_color <- input[[paste0(tabPanelName, "select_by_color")]]
    
    if(sticky == "off") {
      
      if(!is.null(select_by_color)) {
        
        # when select_by_color is on, we can use brush to clear selection but keep brush id
        loonWidgets_info$lastSelection <- if(!is.null(input$plot_brush) | !is.null(input$plot_click)) brush_id else integer(0)
        brush_id <- which(loonWidgets_info$color %in% select_by_color)
      } else {
        
        if(!is.null(output_info$select_by_color)) brush_id <- loonWidgets_info$lastSelection
      }
    } else {
      
      # sticky is on
      if(!is.null(select_by_color)) {
        
        which_is_selected <- union(which(loonWidgets_info$color %in% select_by_color), which(loonWidgets_info$selected))
        
      } else {
        
        which_is_selected <- which(loonWidgets_info$selected)
      }
      
      if(is.null(input$plot_brush)) {
        brush_id <- which_is_selected
      } else {
        brush_id <- union(which_is_selected, brush_id)
      }
    }
    
    # select panel -------------------------------------
    input[[paste0(tabPanelName, "select_static_all")]]
    input[[paste0(tabPanelName, "select_static_none")]]
    input[[paste0(tabPanelName, "select_static_invert")]]
    
    if(buttons$static_button$all != 0) {
      
      brush_id <- seq(N)
    } else if(buttons$static_button$none != 0) {
      
      brush_id <- integer(0)
    } else if(buttons$static_button$invert != 0) {
      
      brush_id <- setdiff(seq(N), brush_id)
    } else NULL
    
    loonWidgets_info$selected <- rep(FALSE, N)
    loonWidgets_info$selected[brush_id] <- TRUE
    
    # highlight color
    output_grob <- set_color_grob(
      loon_grob = output_grob,
      index = brush_id,
      color = loon_color$select_color[1],
      axes_gPath = axes_gPath,
      loon_color = loonWidgets_info$loon_color
    )
    
    # adjust color -------------------------------
    colorList <- loonWidgets_info$colorList
    input[[paste0(tabPanelName, "color")]]
    lapply(colorList, function(col) input[[paste0(tabPanelName, col)]])
    modify_color <- isolate(input[[paste0(tabPanelName, "modify_color")]])
    
    if(buttons$color_button$modify != 0) {
      
      loon_grob <- set_color_grob(
        loon_grob = loon_grob,
        index = brush_id,
        color = modify_color,
        axes_gPath = axes_gPath,
        loon_color = loonWidgets_info$loon_color
      )
      
      loonWidgets_info$color[brush_id] <- modify_color
    }
    
    for(col in colorList) {
      
      if(buttons$color_button[[col]] != 0) {
        
        loon_grob <- set_color_grob(
          loon_grob = loon_grob,
          index = brush_id,
          color = col,
          axes_gPath = axes_gPath,
          loon_color = loonWidgets_info$loon_color
        )
        
        loonWidgets_info$color[brush_id] <- col
      }
    }
    
    # adjust deactive--------------------------------
    which_is_deactive <- which(!loonWidgets_info$active)
    output_grob <- set_deactive_grob(
      loon_grob = output_grob,
      index = which_is_deactive,
      axes_gPath = axes_gPath
    )
    
    loon_grob <- set_deactive_grob(
      loon_grob = loon_grob,
      index = which_is_deactive,
      axes_gPath = axes_gPath
    )
    
    input[[paste0(tabPanelName, "modify_deactive")]]
    if(buttons$active_button$deactive != 0) {
      
      loon_grob <- set_deactive_grob(
        loon_grob = loon_grob,
        index = brush_id,
        axes_gPath = axes_gPath
      )
      
      output_grob <- set_deactive_grob(
        loon_grob = output_grob,
        index = brush_id,
        axes_gPath = axes_gPath
      )
      
      loonWidgets_info$active[brush_id] <- FALSE
      which_is_deactive <- union(which_is_deactive, brush_id)
    }
    
    input[[paste0(tabPanelName, "modify_reactive")]]
    if (buttons$active_button$reactive != 0) {
      
      output_grob <- set_reactive_grob(
        loon_grob = output_grob,
        index = which_is_deactive,
        axes_gPath = axes_gPath,
        showArea = showArea
      )
      
      loon_grob <- set_reactive_grob(
        loon_grob = loon_grob,
        index = which_is_deactive,
        axes_gPath = axes_gPath,
        showArea = showArea
      )
      loonWidgets_info$active <- rep(TRUE, N)
    }
    
    # adjust size--------------------------------
    input[[paste0(tabPanelName, "abs_to_plus")]]
    input[[paste0(tabPanelName, "abs_to_minus")]]
    input[[paste0(tabPanelName, "rel_to_plus")]]
    input[[paste0(tabPanelName, "rel_to_minus")]]
    
    if(buttons$size_button$abs_to_plus != 0) {
      
      if(length(brush_id) > 0) {
        new_size <- min(loonWidgets_info$size[brush_id]) + default_step_size(line = TRUE)
        loonWidgets_info$size[brush_id] <- rep(new_size, length(brush_id))
        
        loon_grob <- set_size_grob(loon_grob = loon_grob,
                                   index = brush_id,
                                   new_size = loonWidgets_info$size,
                                   axes_gPath = axes_gPath,
                                   showArea = showArea)
        
        output_grob <- set_size_grob(loon_grob = output_grob,
                                     index = brush_id,
                                     new_size = loonWidgets_info$size,
                                     axes_gPath = axes_gPath,
                                     showArea = showArea)
      }
    }
    
    if(buttons$size_button$abs_to_minus != 0) {
      
      if(length(brush_id) > 0) {
        new_size <- min(loonWidgets_info$size[brush_id]) - default_step_size(line = TRUE)
        if(new_size <= 0) new_size <- minimumSize()
        loonWidgets_info$size[brush_id] <- rep(new_size, length(brush_id))
        
        loon_grob <- set_size_grob(loon_grob = loon_grob,
                                   index = brush_id,
                                   new_size = loonWidgets_info$size,
                                   axes_gPath = axes_gPath,
                                   showArea = showArea)
        
        output_grob <- set_size_grob(loon_grob = output_grob,
                                     index = brush_id,
                                     new_size = loonWidgets_info$size,
                                     axes_gPath = axes_gPath,
                                     showArea = showArea)
      }
    }
    
    if(buttons$size_button$rel_to_plus != 0) {
      
      if(length(brush_id) > 0) {
        
        loonWidgets_info$size[brush_id] <- loonWidgets_info$size[brush_id] + default_step_size(line = TRUE)
        
        loon_grob <- set_size_grob(loon_grob = loon_grob,
                                   index = brush_id,
                                   new_size = loonWidgets_info$size,
                                   axes_gPath = axes_gPath,
                                   showArea = showArea)
        
        output_grob <- set_size_grob(loon_grob = output_grob,
                                     index = brush_id,
                                     new_size = loonWidgets_info$size,
                                     axes_gPath = axes_gPath,
                                     showArea = showArea)
      }
    }
    
    if(buttons$size_button$rel_to_minus != 0) {
      
      if(length(brush_id) > 0) {
        
        new_size <- loonWidgets_info$size[brush_id] - default_step_size(line = TRUE)
        new_size[which(new_size <= 0)] <- minimumSize()
        loonWidgets_info$size[brush_id] <- new_size
        
        loon_grob <- set_size_grob(loon_grob = loon_grob,
                                   index = brush_id,
                                   new_size = loonWidgets_info$size,
                                   axes_gPath = axes_gPath,
                                   showArea = showArea)
        
        output_grob <- set_size_grob(loon_grob = output_grob,
                                     index = brush_id,
                                     new_size = loonWidgets_info$size,
                                     axes_gPath = axes_gPath,
                                     showArea = showArea)
      }
    }
    
    output_grob <- reorder_grob(output_grob,
                                number = N,
                                brush_id,
                                axes_gPath = axes_gPath)
    
    
    output_grob <- setGrob(
      gTree = output_grob,
      gPath = "l_serialaxes",
      newGrob = editGrob(
        grob = getGrob(output_grob, "l_serialaxes"),
        vp = viewPort
      )
    )
    
    # set linking info
    linkingInfo <- update_linkingInfo(loon_grob,
                                      tabPanelName = tabPanelName,
                                      linkingInfo = linkingInfo, 
                                      linkingGroup = linkingGroup, 
                                      selected = loonWidgets_info$selected,
                                      color = loonWidgets_info$color, 
                                      active = loonWidgets_info$active, 
                                      size = loonWidgets_info$size,
                                      select_by_color = select_by_color,
                                      linkedStates = input[[paste0(tabPanelName, "linkedStates")]])
  }
  
  list(
    output_grob = output_grob,
    loon_grob = loon_grob,
    output_info = list(
      brush_id = brush_id,
      select_by_color = select_by_color,
      linkingGroup = linkingGroup,
      linkingInfo = linkingInfo,
      loonWidgets_info = loonWidgets_info
    )
  )
}