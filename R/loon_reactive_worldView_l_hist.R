loon_reactive_worldView.l_hist <- function(loon_grob, buttons, input, tabPanelName, output_info) {
  
  input$plot_click
  input$plot_brush
  
  if(input[["navBarPage"]] == tabPanelName) {
    
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
    yshows <- input[[paste0(tabPanelName, "yshows")]]
    origin <- input[[paste0(tabPanelName, "origin")]]
    binwidth <- input[[paste0(tabPanelName, "binwidth")]]
    showStackedColors <- "stackedColors" %in% plot_show
    showOutlines <- "outlines" %in% plot_show
    colorFill <- loonWidgets_info$colorFill # showStackedColors is FALSE (thistle)
    colorOutline <- loonWidgets_info$colorOutline
    color <- loonWidgets_info$color
    n <- length(loonWidgets_info$x)
    
    # set active
    input[[paste0(tabPanelName, "modify_reactive")]]
    if (buttons$active_button$reactive != 0) {
      loonWidgets_info$active <- rep(TRUE, n)
    }
    active <- loonWidgets_info$active
    
    binInfo <- get_binInfo(data = loonWidgets_info$x, origin = origin, active = active, 
                           binwidth = binwidth, yshows = yshows)
    binId <- binInfo$binId
    binX <- binInfo$binX
    binHeight <- binInfo$binHeight
    
    bin_xy <- get_bin_xy(binX = binX, binId = binId, binwidth = binwidth, 
                         yshows = yshows, color = color, n = n)
    
    # ++++++++++++++++++++++++++++++++ set guides labels axis and scales ++++++++++++++++++++++++++++++++++++++++++++
    # build Cartesian coordinates
    input[[paste0(tabPanelName, "plot_scale_to_plot")]]
    input[[paste0(tabPanelName, "plot_scale_to_world")]]

    input[[paste0(tabPanelName, "xlim")]]
    input[[paste0(tabPanelName, "ylim")]]
    
    plotView_xlim <- grDevices::extendrange(c(bin_xy$xmin, bin_xy$xmax), f = 0.05)
    plotView_ylim <- grDevices::extendrange(c(bin_xy$ymin, bin_xy$ymax), f = 0.05)
    
    if(swap_in_loon) {
      
      worldView_xlim <- grDevices::extendrange(c(bin_xy$xmin, bin_xy$xmax, loonWidgets_info$worldView_ylim), f = 0.05)
      worldView_ylim <- grDevices::extendrange(c(bin_xy$ymin, bin_xy$ymax, loonWidgets_info$worldView_xlim), f = 0.05)
    } else {
      
      worldView_xlim <- grDevices::extendrange(c(bin_xy$xmin, bin_xy$xmax, loonWidgets_info$worldView_xlim), f = 0.05)
      worldView_ylim <- grDevices::extendrange(c(bin_xy$ymin, bin_xy$ymax, loonWidgets_info$worldView_ylim), f = 0.05)
    }
    
    loonWidgets_info$worldView_xlim <- worldView_xlim
    loonWidgets_info$worldView_ylim <- worldView_ylim
    
    # swap layers
    if(loonWidgets_info$swap_in_loon != swap_in_shiny) loon_grob <- swap_layer_grob(loon_grob, parent = "histogram")
    
    if(swap_in_shiny) {
      
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

    # set viewport
    loon_grob <- set_viewPort_grob(
      loon_grob = loon_grob,
      margins = rep(1, 4), 
      xlim = if(swap_in_shiny) worldView_ylim  else worldView_xlim,
      ylim = if(swap_in_shiny) worldView_xlim  else worldView_ylim
    )
    # +++++++++++++++++++++++++++++++++++++++++ set other aesthetic ++++++++++++++++++++++++++++++++++++++++
    # dynamic select -----------------------------------------------
    select_dynamic <- input[[paste0(tabPanelName, "select_dynamic")]]
    sticky <- input[[paste0(tabPanelName, "sticky")]]
    
    if(sticky == "off") {
      
      if("deselect" == select_dynamic) {
        if(!is.null(input$plot_brush)) brush_id <- integer(0)
      }
    } else {
      
      loonWidgets_info$selected[brush_id] <- TRUE
      brush_id <- which(loonWidgets_info$selected)
    }
    
    # select by color ------------------------------------
    select_by_color <- input[[paste0(tabPanelName, "select_by_color")]]
    if(!is.null(select_by_color)) {
      
      brush_id <- which(color %in% select_by_color)
      
    } else {
      
      if(!is.null(output_info$select_by_color)) brush_id <- numeric(0)
    }
    
    # static select -----------------------------------------------
    input[[paste0(tabPanelName, "select_static_all")]]
    input[[paste0(tabPanelName, "select_static_none")]]
    input[[paste0(tabPanelName, "select_static_invert")]]
    
    N <- length(loonWidgets_info$linkingKey)
    
    if(buttons$static_button$all != 0) {
      
      brush_id <- seq(N)
    } else if(buttons$static_button$none != 0) {
      
      brush_id <- integer(0)
    } else if(buttons$static_button$invert != 0) {
      
      brush_id <- setdiff(seq(N), brush_id)
    } else NULL
    
    loonWidgets_info$selected <- rep(FALSE, n)
    loonWidgets_info$selected[brush_id] <- TRUE
    
    # modify color ------------------------------------------------
    input[[paste0(tabPanelName, "color")]]
    modify_color <- isolate(input[[paste0(tabPanelName, "modify_color")]])
    if(buttons$color_button$modify != 0) {
      
      color[brush_id] <- modify_color
      loonWidgets_info$color <- color
    }
    
    # set deactive --------------------------------------------
    input[[paste0(tabPanelName, "modify_deactive")]]
    if(buttons$active_button$deactive != 0) {
      
      if(length(brush_id) > 0) {
        
        active[brush_id] <- FALSE
        binInfo <- get_binInfo(data = loonWidgets_info$x, origin = origin, active = active, 
                               binwidth = binwidth, yshows = yshows)
        binId <- binInfo$binId
        binX <- binInfo$binX
        binHeight <- binInfo$binHeight
        
        bin_xy <- get_bin_xy(binX = binX, binId = binId, binwidth = binwidth,
                             yshows = yshows, color = color, n = n)
      }
    }
    
    which_is_deactive <- which(!active)
    
    # build grob at the end ---------------------------------------------------------------
    loon_grob <- get_hist_grob(loon_grob = loon_grob, yshows = yshows,
                               binId = binId, binX = binX, binHeight = binHeight, binwidth = binwidth,
                               n = n, swapAxes = swap_in_shiny,
                               showStackedColors = showStackedColors, showOutlines = showOutlines,
                               color = color, colorFill = colorFill, colorOutline = colorOutline)
    
    # highlight selected bin
    loon_grob <- highlight_selected_bin_grob(loon_grob = loon_grob, yshows = yshows, active = active, 
                                             selected = loonWidgets_info$selected,
                                             binId = binId, binX = binX, binHeight = binHeight, 
                                             binwidth = binwidth, n = n, 
                                             swapAxes = swap_in_shiny, showStackedColors = showStackedColors, 
                                             showOutlines = showOutlines,
                                             color = color, colorFill = colorFill, colorOutline = colorOutline,
                                             loon_color = loonWidgets_info$loon_color)
    
    
    
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
      
    } else if (layer_button$down != 0) {
      
      loon_grob <- move_layer_down_grob(loon_grob = loon_grob,
                                        current_layer = current_layer,
                                        parent = "l_hist_layers")
      
    } else if (layer_button$visible != 0) {
      
      loon_grob <- move_layer_visible_grob(loon_grob = loon_grob,
                                           current_layer = current_layer)
      
    } else if (layer_button$invisible != 0) {
      
      loon_grob <- move_layer_invisible_grob(loon_grob = loon_grob,
                                             current_layer = current_layer)
      
    } else if (layer_button$plus != 0) {
      message("adding layers has not been inplemented so far")
    } else if (layer_button$minus != 0) {
      
      loon_grob <- setGrob(
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
    
    # remove scales
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "axes",
      newGrob = nullGrob(name = "axes")
    )
    
    # remove labels
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "labels",
      newGrob = nullGrob(name = "labels")
    )
    
    # remove guides
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "guides",
      newGrob = nullGrob(name = "guides")
    )
    
    # bounding box color to grey
    if(is.null(getGrob(loon_grob, "boundary rectangle"))) {
      bound_gPath <- "boundary rectangle: rectGrob arguments"
      bound_grob <- do.call(rectGrob, getGrobArgs(getGrob(loon_grob, gPath = bound_gPath)))
    } else {
      bound_gPath <- "boundary rectangle"
      bound_grob <- getGrob(loon_grob, gPath = bound_gPath)
    }
    
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = bound_gPath,
      newGrob = editGrob(
        grob = bound_grob,
        gp = gpar(
          fill = NA, 
          col = "grey90",
          lwd = 1
        )
      )
    )
    
    # remove clip
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "clipping region",
      newGrob = nullGrob(name = "clipping region")
    )
    
    loon_color <- loonWidgets_info$loon_color
    
    loon_grob <- addGrob(
      gTree = loon_grob,
      gPath = "loon plot",
      child = rectGrob(
        x = unit(mean(xlim), "native"),
        y = unit(mean(ylim), "native"),
        width = unit(diff(xlim), "native"),
        height = unit(diff(ylim), "native"),
        gp = gpar(
          fill = NA,
          col = loon_color$foreground_color[1],
          lwd = 3
        ),
        name = "world view"
      )
    )
  }
  
  loon_grob
}