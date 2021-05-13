# the way to build loon_reactive.l_hist is very different from others. The reason is that loonGrob.l_hist is 
# a couple of rect grobs; it would be hard to determine the linking index. Hence, we would use the widgets information
# to rebuild l_hist grob.
loon_reactive.l_hist <- function(loon_grob, output_grob, linkingInfo, buttons, position, selectBy,
                                 linkingGroup, input, tabPanelName, output_info) {
  
  input$plot_brush
  input$plot_click
  
  if(!is.null(output_grob) & input[["navBarPage"]] != tabPanelName) {
    
    loonWidgets_info <- output_info$loonWidgets_info
    
    if(linkingGroup != "none") {
      
      # set_linking_grobs is slightly different, it returns loonWidget_info and output_grob (instead of loon_grob)
      grobs_info <- set_linking_grobs(
        loon_grob = loon_grob,
        output_grob = output_grob,
        linkedInfo = linkingInfo[[linkingGroup]],
        tabPanelName = tabPanelName,
        loon_color = NULL,
        loonWidgets_info = loonWidgets_info
      )
      
      selected <- linkingInfo[[linkingGroup]]$selected
      brush_id <- which(selected)
      select_by_color <- linkingInfo[[linkingGroup]]$select_by_color
      
      loonWidgets_info <- grobs_info$loonWidgets_info
      output_grob <- grobs_info$output_grob
    } else {
      
      brush_id <- output_info$brush_id
      select_by_color <- output_info$select_by_color
    }
  } else {
    
    isFirst_draw <- is.null(output_grob)
    output_grob <- loon_grob
    brush_id <- output_info$brush_id
    
    loonWidgets_info <- output_info$loonWidgets_info
    
    # interactive ------------------------------------------------------
    plot_axes1 <- input[[paste0(tabPanelName, "plot_axes1")]]
    plot_axes2 <- input[[paste0(tabPanelName, "plot_axes2")]]
    plot_show <- input[[paste0(tabPanelName, "plot_show")]]
       
    # plot scale to
    scale_to_button <- list(
      plot = buttons$scale_to_button$plot,
      world = buttons$scale_to_button$world
    )
    
    swap_in_shiny <- "swap" %in% plot_axes1
    swap_in_loon <- loonWidgets_info$swap_in_loon
    loonWidgets_info$swap_in_shiny <-swap_in_shiny
    
    yshows <- input[[paste0(tabPanelName, "yshows")]]
    yshowsIsModified <- FALSE
    if(loonWidgets_info$yshows != yshows) {
      yshowsIsModified <- TRUE
      loonWidgets_info$yshows <- yshows
    }
    loonWidgets_info$yshowsIsModified <- yshowsIsModified
    
    origin <- input[[paste0(tabPanelName, "origin")]]
    originIsModified <- FALSE
    if(loonWidgets_info$origin != origin) {
      originIsModified <- TRUE
      loonWidgets_info$origin <- origin
    }
    loonWidgets_info$originIsModified <- originIsModified
    
    binwidth <- input[[paste0(tabPanelName, "binwidth")]]
    binwidthIsModified <- FALSE
    if(loonWidgets_info$binwidth != binwidth) {
      binwidthIsModified <- TRUE
      loonWidgets_info$binwidth <- binwidth
    }
    loonWidgets_info$binwidthIsModified <- binwidthIsModified

    
    showStackedColors <- "stackedColors" %in% plot_show
    loonWidgets_info$showStackedColors <- showStackedColors
    
    showOutlines <- "outlines" %in% plot_show
    loonWidgets_info$showOutlines <- showOutlines
    
    colorFill <- loonWidgets_info$colorFill # showStackedColors is FALSE (thistle)
    colorOutline <- loonWidgets_info$colorOutline
    color <- loonWidgets_info$color
    N <- length(loonWidgets_info$linkingKey)
    
    # set active
    input[[paste0(tabPanelName, "modify_reactive")]]
    if (buttons$active_button$reactive != 0) {
      loonWidgets_info$active <- rep(TRUE, N)
    }
    active <- loonWidgets_info$active
    
    binInfo <- get_binInfo(data = loonWidgets_info$x, origin = origin, active = active, 
                           binwidth = binwidth, yshows = yshows)
    binId <- binInfo$binId
    binX <- binInfo$binX
    binHeight <- binInfo$binHeight
    
    bin_xy <- get_bin_xy(binX = binX, binId = binId, binwidth = binwidth,
                         yshows = yshows, color = color, n = N)
    
    # ++++++++++++++++++++++++++++++++ set guides labels axis and scales ++++++++++++++++++++++++++++++++++++++++++++
    # build Cartesian coordinates
    input[[paste0(tabPanelName, "plot_scale_to_plot")]]
    input[[paste0(tabPanelName, "plot_scale_to_world")]]
    
    input[[paste0(tabPanelName, "xlim")]]
    input[[paste0(tabPanelName, "ylim")]]
    
    if(yshowsIsModified && yshows == "density") {
      scale_to_button$plot <- 1
    }
    
    if((binwidthIsModified || originIsModified) && yshows == "density") {
      scale_to_button$world <- 1
    }
    
    plotView_xlim <- grDevices::extendrange(c(bin_xy$xmin, bin_xy$xmax))
    plotView_ylim <- grDevices::extendrange(c(bin_xy$ymin, bin_xy$ymax))
    
    loonWidgets_info$plotView_xlim <- plotView_xlim
    loonWidgets_info$plotView_ylim <- plotView_ylim
    
    if(swap_in_loon) {
      
      worldView_xlim <- grDevices::extendrange(c(plotView_xlim, loonWidgets_info$worldView_ylim))
      worldView_ylim <- grDevices::extendrange(c(plotView_ylim, loonWidgets_info$worldView_xlim))
    } else {
      
      worldView_xlim <- grDevices::extendrange(c(plotView_xlim, loonWidgets_info$worldView_xlim))
      worldView_ylim <- grDevices::extendrange(c(plotView_ylim, loonWidgets_info$worldView_ylim))
    }
    
    # swap layers
    if(swap_in_loon != swap_in_shiny) {
      output_grob <- swap_layer_grob(output_grob, parent = "histogram")
    }
    
    if(swap_in_shiny) {
      
      xlabel <- loonWidgets_info$ylabel
      ylabel <- loonWidgets_info$xlabel
      
      if(scale_to_button$plot != 0) {
        
        ylim <- plotView_xlim
        xlim <- plotView_ylim
      } else if(scale_to_button$world != 0) {
        
        ylim <- worldView_xlim
        xlim <-  worldView_ylim
      } else {
        
        ylim <- input[[paste0(tabPanelName, "xlim")]]
        xlim <- input[[paste0(tabPanelName, "ylim")]]
      }
      
    } else {
      
      xlabel <- loonWidgets_info$xlabel
      ylabel <- loonWidgets_info$ylabel 
      
      if(scale_to_button$plot != 0) {
        
        xlim <- plotView_xlim
        ylim <- plotView_ylim
      } else if(scale_to_button$world != 0) {
        
        xlim <- worldView_xlim
        ylim <-  worldView_ylim
      } else {
        
        xlim <- input[[paste0(tabPanelName, "xlim")]]
        ylim <- input[[paste0(tabPanelName, "ylim")]]
      }
    }
    
    xaxis <- grid.pretty(xlim)
    yaxis <- grid.pretty(ylim)
    
    title <- loonWidgets_info$title
    
    # reset margins ----------------------------------------------------------
    loon_margins <- loonWidgets_info$loon_default_margins
    margins <- rep(0, 4)
    
    # set scales ----------------------------------------------------------
    if("scales" %in% plot_axes1) {
      
      output_grob <- set_scales_grob(loon_grob = output_grob,
                                     xaxis = xaxis,
                                     yaxis = yaxis)
      
      margins <- margins + loon_margins$scalesMargins
      
      loonWidgets_info$showScales <- TRUE
    } else {
      
      output_grob <- setGrob(
        gTree = output_grob,
        gPath = "axes",
        newGrob = nullGrob(name = "axes")
      )
      
      loonWidgets_info$showScales <- FALSE
    }
    
    # set labels -------------------------------------------------------------
    if("labels" %in% plot_axes2) {
      
      if(yshows == "density") {
        if(swap_in_shiny) {
          xlabel <- "Density"
        } else {
          ylabel <- "Density"
        }
      } else {
        # yshows is Frequency
        if(swap_in_shiny) {
          xlabel <- "Frequency"
        } else {
          ylabel <- "Frequency"
        }
      }
      
      output_grob <- set_labels_grob(
        loon_grob = output_grob,
        showScales = loonWidgets_info$showScales,
        xlabel = xlabel,
        ylabel = ylabel,
        title = title
      )
      
      if(is.null(xlabel)) loon_margins$labelMargins[1] <- loon_margins$minimumMargins[1]
      if(is.null(ylabel)) loon_margins$labelMargins[2] <- loon_margins$minimumMargins[2]
      if(is.null(title)) loon_margins$labelMargins[3] <- loon_margins$minimumMargins[3]
      margins <- margins + loon_margins$labelMargins
      
      loonWidgets_info$showLabels <- TRUE
    } else {
      
      output_grob <- setGrob(
        gTree = output_grob,
        gPath = "labels",
        newGrob = nullGrob(name = "labels")
      )
      
      loonWidgets_info$showLabels <- FALSE
    }
    
    if(loonWidgets_info$showLabels | loonWidgets_info$showScales) margins <- apply(cbind(margins, loon_margins$minimumMargins), 1, max)
    
    # set guides -------------------------------------------------------------
    if("guides" %in% plot_axes2) {
      
      output_grob <- set_guides_grob(loon_grob = output_grob,
                                     xaxis = xaxis,
                                     yaxis = yaxis,
                                     loon_color = loonWidgets_info$loon_color)
      
      loonWidgets_info$showGuides <- TRUE
    } else {
      
      output_grob <- setGrob(
        gTree = output_grob,
        gPath = "guides",
        newGrob = nullGrob(name = "guides")
      )
      
      loonWidgets_info$showGuides <- FALSE
    }
    
    # set viewport
    output_grob <- set_viewPort_grob(
      loon_grob = output_grob,
      margins = margins, 
      xlim = xlim,
      ylim = ylim
    )
    
    # reset boundary
    output_grob <- set_boundary_grob(loon_grob = output_grob,
                                     margins = margins, 
                                     loon_color = loonWidgets_info$loon_color)
    
    # +++++++++++++++++++++++++++++++++++++++++ set other aesthetic ++++++++++++++++++++++++++++++++++++++++
    linkingGroup <- loonWidgets_info$linkingGroup <- input[[paste0(tabPanelName, "linkingGroup")]]
    
    brush_id <- if(isFirst_draw) {
      
      output_info$brush_id
    } else {
      
      if(is.null(input$plot_brush) & is.null(input$plot_click)) {
        output_info$brush_id
      } else {
        
        get_brush_id(
          loon_grob = output_grob,
          coord = bin_xy,
          swap_in_shiny = swap_in_shiny,
          position = position,
          brush_info = input$plot_brush,
          vp = vpStack(
            plotViewport(margins = margins, name = "plotViewport"),
            dataViewport(xscale = xlim,
                         yscale = ylim,
                         name = "dataViewport")
          ),
          click_info = input$plot_click
        )
      }
    }
    
    # dynamic select -----------------------------------------------
    select_dynamic <- input[[paste0(tabPanelName, "select_dynamic")]]
    sticky <- input[[paste0(tabPanelName, "sticky")]]
    # select by color ------------------------------------
    select_by_color <- input[[paste0(tabPanelName, "select_by_color")]]
    
    if(sticky == "off") {
      
      if(!is.null(select_by_color)) {
        
        # when select_by_color is on, we can use brush to clear selection but keep brush id
        loonWidgets_info$lastSelection <- if(!is.null(input$plot_brush) | !is.null(input$plot_click)) brush_id else integer(0)
        brush_id <- which(color %in% select_by_color)
        
      } else {
        
        if(!is.null(output_info$select_by_color)) brush_id <- loonWidgets_info$lastSelection
      }
      
      if("deselect" == select_dynamic) {
        if(!is.null(input$plot_brush) | !is.null(input$plot_click)) brush_id <- integer(0)
      }
      
    } else {
      
      # sticky is on
      if(!is.null(select_by_color)) {
        
        which_is_selected <- union(which(color %in% select_by_color), which(loonWidgets_info$selected))
        
      } else {
        
        which_is_selected <- which(loonWidgets_info$selected)
      }
      
      if("invert" == select_dynamic) {
        
        if(is.null(input$plot_brush)) {
          brush_id <- which_is_selected
        } else {
          brush_id <- union(setdiff(which_is_selected, brush_id), setdiff(brush_id, which_is_selected))
        }
      } else if("deselect" == select_dynamic) {
        
        if(is.null(input$plot_brush)) {
          brush_id <- which_is_selected
        } else {
          brush_id <- setdiff(which_is_selected, brush_id)
        }
        
      } else {
        
        if(is.null(input$plot_brush)) {
          brush_id <- which_is_selected
        } else {
          brush_id <- union(which_is_selected, brush_id)
        }
      }
    }
    
    # static select -----------------------------------------------
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
    
    # modify color ------------------------------------------------
    colorList <- loonWidgets_info$colorList
    input[[paste0(tabPanelName, "color")]]
    lapply(colorList, function(col) input[[paste0(tabPanelName, col)]])
    modify_color <- isolate(input[[paste0(tabPanelName, "modify_color")]])
    
    if(buttons$color_button$modify != 0) {
      
      color[brush_id] <- modify_color
      loonWidgets_info$color <- color
    }
    
    for(col in colorList) {
      
      if(buttons$color_button[[col]] != 0) {
        
        color[brush_id] <- col
        loonWidgets_info$color <- color
      }
    }
    
    # set deactive --------------------------------------------
    input[[paste0(tabPanelName, "modify_deactive")]]
    if(buttons$active_button$deactive != 0) {
      
      if(length(brush_id) > 0) {
        
        active[brush_id] <- FALSE
        loonWidgets_info$active <- active
        
        binInfo <- get_binInfo(data = loonWidgets_info$x, origin = origin, active = active, 
                               binwidth = binwidth, yshows = yshows)
        binId <- binInfo$binId
        binX <- binInfo$binX
        binHeight <- binInfo$binHeight
        
        bin_xy <- get_bin_xy(binX = binX, binId = binId, binwidth = binwidth,
                             yshows = yshows, color = color, n = length(which(active)))
        
        plotView_xlim <- grDevices::extendrange(c(bin_xy$xmin, bin_xy$xmax))
        plotView_ylim <- grDevices::extendrange(c(bin_xy$ymin, bin_xy$ymax))
        
        loonWidgets_info$plotView_xlim <- plotView_xlim
        loonWidgets_info$plotView_ylim <- plotView_ylim
        
      }
    }
    
    which_is_deactive <- which(!active)
    
    # build grob at the end ---------------------------------------------------------------
    output_grob <- get_hist_grob(loon_grob = output_grob, yshows = yshows,
                                 binId = binId, binX = binX, binHeight = binHeight, binwidth = binwidth,
                                 n = N, swapAxes = swap_in_shiny,
                                 showStackedColors = showStackedColors, showOutlines = showOutlines,
                                 color = color, colorFill = colorFill, colorOutline = colorOutline)
    
    # highlight selected bin
    output_grob <- highlight_selected_bin_grob(loon_grob = output_grob, yshows = yshows, active = active, selected = loonWidgets_info$selected,
                                               binId = binId, binX = binX, binHeight = binHeight, binwidth = binwidth, n = N, 
                                               swapAxes = swap_in_shiny, showStackedColors = showStackedColors, showOutlines = showOutlines,
                                               color = color, colorFill = colorFill, colorOutline = colorOutline,
                                               loon_color = loonWidgets_info$loon_color)
    
    # set linking info
    linkingInfo <- update_linkingInfo(loon_grob,
                                      tabPanelName = tabPanelName,
                                      linkingInfo = linkingInfo, 
                                      linkingGroup = linkingGroup, 
                                      selected = loonWidgets_info$selected,
                                      color = loonWidgets_info$color, 
                                      active = loonWidgets_info$active, 
                                      select_by_color = select_by_color,
                                      linkedStates = input[[paste0(tabPanelName, "linkedStates")]])
    
    ## up, down, visible, invisible, ... layer
    layer_button <- list(
      up = buttons$layer_button$up,
      down = buttons$layer_button$down,
      visible = buttons$layer_button$visible,
      invisible = buttons$layer_button$invisible,
      plus = buttons$layer_button$plus,
      minus = buttons$layer_button$minus,
      scale_to = buttons$layer_button$scale_to,
      set = buttons$layer_button$set
    )
    
    # layers
    input[[paste0(tabPanelName, "layer_up")]]
    input[[paste0(tabPanelName, "layer_down")]]
    input[[paste0(tabPanelName, "layer_visible")]]
    input[[paste0(tabPanelName, "layer_invisible")]]
    input[[paste0(tabPanelName, "layer_plus")]]
    input[[paste0(tabPanelName, "layer_minus")]]
    input[[paste0(tabPanelName, "layer_scale_to")]]
    input[[paste0(tabPanelName, "layer_set")]]
    
    current_layer <- input[[paste0(tabPanelName, "layer")]]
    new_layer_label <- isolate(input[[paste0(tabPanelName, "layer_changed_label")]])
    
    if(layer_button$set != 0) {
      
      if(new_layer_label == "") {
        message("no valid label")
      } else {
        layers <- loonWidgets_info$layers
        layers_name <- names(layers)
        
        which_layer_is_edited <- which(layers_name == current_layer)
        
        layers_name[which_layer_is_edited] <- new_layer_label
        names(layers) <- layers_name
        loonWidgets_info$layers <- layers
        
        current_layer <- layers[which_layer_is_edited]
      }
    } else {
      
      layers <- loonWidgets_info$layers
      layers_name <- names(layers)
      
      current_layer <- layers[which(layers_name == current_layer)]
    }
    
    if(layer_button$up != 0) {
      
      loon_grob <- move_layer_up_grob(loon_grob = loon_grob,
                                      current_layer = current_layer,
                                      parent = "l_hist_layers")
      
      output_grob <- move_layer_up_grob(loon_grob = output_grob,
                                        current_layer = current_layer,
                                        parent = "l_hist_layers")
      
    } else if (layer_button$down != 0) {
      
      loon_grob <- move_layer_down_grob(loon_grob = loon_grob,
                                        current_layer = current_layer,
                                        parent = "l_hist_layers")
      
      output_grob <- move_layer_down_grob(loon_grob = output_grob,
                                          current_layer = current_layer,
                                          parent = "l_hist_layers")
      
    } else if (layer_button$visible != 0) {
      
      loon_grob <- move_layer_visible_grob(loon_grob = loon_grob,
                                           current_layer = current_layer)
      
      output_grob <- move_layer_visible_grob(loon_grob = output_grob,
                                             current_layer = current_layer)
      
    } else if (layer_button$invisible != 0) {
      
      loon_grob <- move_layer_invisible_grob(loon_grob = loon_grob,
                                             current_layer = current_layer)
      
      output_grob <- move_layer_invisible_grob(loon_grob = output_grob,
                                               current_layer = current_layer)
      
    } else if (layer_button$plus != 0) {
      message("adding layers has not been inplemented so far")
    } else if (layer_button$minus != 0) {
      
      loon_grob <- setGrob(
        gTree = loon_grob,
        gPath = current_layer,
        newGrob = nullGrob(name = current_layer)
      )
      
      output_grob <- setGrob(
        gTree = loon_grob,
        gPath = current_layer,
        newGrob = nullGrob(name = current_layer)
      )
      
    } else if (layer_button$scale_to != 0) {
      
      if(current_layer == "histogram") {
        
        loonWidgets_info$xlim <- loonWidgets_info$plotView_xlim
        loonWidgets_info$ylim <- loonWidgets_info$plotView_ylim
      } else {
        
        layer_lim <- get_layer_worldView(loon_grob, layer = current_layer)
        
        if(length(layer_lim$xlim) != 0 & length(layer_lim$ylim) != 0) {
          
          if(swap_in_loon) {
            
            loonWidgets_info$xlim <- layer_lim$ylim
            loonWidgets_info$ylim <- layer_lim$xlim
            
          } else {
            
            loonWidgets_info$xlim <- layer_lim$xlim
            loonWidgets_info$ylim <- layer_lim$ylim
            
          }
        } else message("group layer cannot be scaled to")
      }
    } else NULL
    
    layer_button <- list(
      up = buttons$layer_button$up,
      down = buttons$layer_button$down,
      visible = buttons$layer_button$visible,
      invisible = buttons$layer_button$invisible,
      plus = buttons$layer_button$plus,
      minus = buttons$layer_button$minus,
      scale_to = buttons$layer_button$scale_to,
      set = buttons$layer_button$set
    )
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