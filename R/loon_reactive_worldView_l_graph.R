loon_reactive_worldView.l_graph <- function(loon_grob, buttons, input, tabPanelName, output_info) {

  path <- file.path(find.package(package = 'loon.shiny'), "images")
  
  input$plot_click
  input$plot_brush

  if(input[["navBarPage"]] == tabPanelName) {
    
    brush_id <- output_info$brush_id
    loonWidgets_info <- output_info$loonWidgets_info
    
    # interactive ------------------------------------------------------
    plot_axes1 <- input[[paste0(tabPanelName, "plot_axes1")]]
    plot_axes2 <- input[[paste0(tabPanelName, "plot_axes2")]]
    
    # plot scale to
    scale_to_button <- list(
      select = buttons$scale_to_button$select,
      plot = buttons$scale_to_button$plot,
      world = buttons$scale_to_button$world
    )
    
    # swap, showScales, showLabels and showGuides -------------------------------------
    swap_in_loon <- loonWidgets_info$swap_in_loon
    swap_in_shiny <- "swap" %in% plot_axes1
    swap <- ((swap_in_shiny & !swap_in_loon) | (!swap_in_shiny & swap_in_loon))
    
    N <- length(loonWidgets_info$active)
    
    input[[paste0(tabPanelName, "xlim")]]
    input[[paste0(tabPanelName, "ylim")]]
    
    #labels <- get_labels(loon_grob)
    labels <- loonWidgets_info$labels
    if(swap) {
      
      if(scale_to_button$select != 0) {
        
        if(length(brush_id) == 0) {
          message("no points selected")
          
          loonWidgets_info$ylim <- input[[paste0(tabPanelName, "xlim")]]
          loonWidgets_info$xlim <- input[[paste0(tabPanelName, "ylim")]]
        } else {
          
          loonWidgets_info$ylim <- c(
            min(loonWidgets_info$x[brush_id]) - loonWidgets_info$step_x/2,
            max(loonWidgets_info$x[brush_id]) + loonWidgets_info$step_x/2
          )
          loonWidgets_info$xlim <- c(
            min(loonWidgets_info$y[brush_id]) - loonWidgets_info$step_y/2,
            max(loonWidgets_info$y[brush_id]) + loonWidgets_info$step_y/2
          )
          
        } 
      } else if(scale_to_button$plot != 0) {
        
        loonWidgets_info$ylim <- loonWidgets_info$plotView_xlim
        loonWidgets_info$xlim <- loonWidgets_info$plotView_ylim
      } else if(scale_to_button$world != 0) {
        
        loonWidgets_info$ylim <- loonWidgets_info$worldView_xlim
        loonWidgets_info$xlim <- loonWidgets_info$worldView_ylim
      } else {
        
        loonWidgets_info$ylim <- input[[paste0(tabPanelName, "xlim")]]
        loonWidgets_info$xlim <- input[[paste0(tabPanelName, "ylim")]]
      }
      # swap output grob
      loon_grob <- swapCoords_grob(loon_grob, 
                                     x = loonWidgets_info$y, 
                                     y = loonWidgets_info$x, 
                                     reactive = FALSE)
      # swap layer
      loon_grob <- swap_layer_grob(loon_grob, parent = "graph")
    } else {
      
      if(scale_to_button$select != 0) {
        if(length(brush_id) == 0) {
          message("no points selected")
          loonWidgets_info$xlim <- input[[paste0(tabPanelName, "xlim")]]
          loonWidgets_info$ylim <- input[[paste0(tabPanelName, "ylim")]]
        } else {
          
          loonWidgets_info$xlim <- c(
            min(loonWidgets_info$x[brush_id]) - loonWidgets_info$step_x/2,
            max(loonWidgets_info$x[brush_id]) + loonWidgets_info$step_x/2
          )
          loonWidgets_info$ylim <- c(
            min(loonWidgets_info$y[brush_id]) - loonWidgets_info$step_y/2,
            max(loonWidgets_info$y[brush_id]) + loonWidgets_info$step_y/2
          )
        }
      } else if(scale_to_button$plot != 0) {
        
        loonWidgets_info$xlim <- loonWidgets_info$plotView_xlim
        loonWidgets_info$ylim <- loonWidgets_info$plotView_ylim
      } else if(scale_to_button$world != 0) {
        
        loonWidgets_info$xlim <- loonWidgets_info$worldView_xlim
        loonWidgets_info$ylim <- loonWidgets_info$worldView_ylim
      } else {        
        loonWidgets_info$xlim <- input[[paste0(tabPanelName, "xlim")]]
        loonWidgets_info$ylim <- input[[paste0(tabPanelName, "ylim")]]
      }
    }

    loon_color <- loonWidgets_info$loon_color
    linkingGroup <- input[[paste0(tabPanelName, "linkingGroup")]]
    
    # select dynamic
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
    
    select_by_color <- input[[paste0(tabPanelName, "select_by_color")]]
    if(!is.null(select_by_color)) {
      
      brush_id <- if(sticky == "on") {
        union(which(loonWidgets_info$color %in% select_by_color), which(loonWidgets_info$selected))
      } else {
        which(loonWidgets_info$color %in% select_by_color)
      }
    } else {
      
      if(!is.null(output_info$select_by_color)) brush_id <- numeric(0)
    }
    
    # select panel -------------------------------------
    input[[paste0(tabPanelName, "select_static_all")]]
    input[[paste0(tabPanelName, "select_static_none")]]
    input[[paste0(tabPanelName, "select_static_invert")]]
    
    if(buttons$static_button$all != 0) {
      
      brush_id <- seq(N)
    } else if(buttons$static_button$none != 0) {
      
      brush_id <- numeric(0)
    } else if(buttons$static_button$invert != 0) {
      
      brush_id <- setdiff(seq(N), brush_id)
    } else NULL
    
    loonWidgets_info$selected <- rep(FALSE, N)
    loonWidgets_info$selected[brush_id] <- TRUE
    
    # adjust glyph size--------------------------------
    loon_grob <- set_glyph_grob(
      loon_grob = loon_grob,
      index = seq(N),
      new_pch = point_default_pch(),
      tmp = FALSE,
      color = loonWidgets_info$color,
      size = rep(default_size(), N),
      pch = loonWidgets_info$pch,
      loon_color = loonWidgets_info$loon_color
    )
    
    # highlight color
    loon_grob <- set_color_grob(
      loon_grob = loon_grob,
      index = brush_id,
      color = loon_color$select_color[1],
      size = rep(default_size(), N),
      pch = rep(point_default_pch(), N),
      loon_color = loonWidgets_info$loon_color
    )
    
    # adjust active--------------------------------
    input[[paste0(tabPanelName, "modify_deactive")]]
    if(buttons$active_button$deactive != 0) {
      
      loon_grob <- set_deactive_grob(
        loon_grob = loon_grob,
        index = brush_id
      )
      
      loon_grob <- set_deactive_grob(
        loon_grob = loon_grob,
        index = brush_id
      )
      
      loonWidgets_info$active[brush_id] <- FALSE
      which_is_deactive <- brush_id
      
    } else which_is_deactive <- which(!loonWidgets_info$active)
    
    input[[paste0(tabPanelName, "modify_reactive")]]
    if (buttons$active_button$reactive != 0) {
      
      loon_grob <- set_reactive_grob(
        loon_grob = loon_grob,
        index = which_is_deactive,
        graph_edges = loonWidgets_info$graph_edges
      )
      
      which_is_deactive <- numeric(0)
      loonWidgets_info$active <- rep(TRUE, N)
    } else NULL
    
    showOrbit <- input[[paste0(tabPanelName, "show_nodes_label")]]
    if(showOrbit) {
      
      loon_grob <- set_node_labels_grob(loon_grob, which_is_deactive)
      
      loonWidgets_info$showOrbit <- TRUE
    } else {
      
      loon_grob <- setGrob(
        gTree = loon_grob,
        gPath = "graph labels",
        newGrob = nullGrob(name = "graph labels")
      )
      
      loonWidgets_info$showOrbit <- FALSE
    }
    
    # modify move
    move_button <- list(
      halign = buttons$move_button$halign,
      valign = buttons$move_button$valign,
      hdist = buttons$move_button$hdist,
      vdist = buttons$move_button$vdist,
      grid = buttons$move_button$grid,
      jitter = buttons$move_button$jitter,
      reset = buttons$move_button$reset
    )
    
    input[[paste0(tabPanelName, "modify_move_jitter")]]
    
    if(length(brush_id) > 0) {
      
      if(move_button$halign != 0) {
        
        # to determine if the default widget is swapped
        halign_y <- if(swap) mean(loonWidgets_info$x[brush_id]) else mean(loonWidgets_info$y[brush_id])
        
        loon_grob <- move_halign_grob(loon_grob = loon_grob,
                                      index = brush_id,
                                      swap = swap,
                                      halign_y = halign_y,
                                      temporary = TRUE)
        
        if(swap) loonWidgets_info$x[brush_id] <- halign_y else loonWidgets_info$y[brush_id] <- halign_y
        
      } else if(move_button$valign != 0) {

        valign_x <- if(swap) mean(loonWidgets_info$y[brush_id]) else mean(loonWidgets_info$x[brush_id])
        
        loon_grob <- move_valign_grob(loon_grob = loon_grob,
                                      index = brush_id,
                                      swap = swap,
                                      valign_x = valign_x,
                                      temporary = TRUE)
        
        if(swap) loonWidgets_info$y[brush_id] <- valign_x else loonWidgets_info$x[brush_id] <- valign_x
        
      } else if(move_button$hdist != 0) {
        
        hdist_y <- if(swap) {
          
          seq(
            from = min(loonWidgets_info$x[brush_id]),
            to = max(loonWidgets_info$x[brush_id]),
            length.out = length(brush_id)
          )
        } else {
          
          seq(
            from = min(loonWidgets_info$y[brush_id]),
            to = max(loonWidgets_info$y[brush_id]),
            length.out = length(brush_id)
          )
        } 
        
        loon_grob <- move_hdist_grob(loon_grob = loon_grob,
                                       index = brush_id,
                                       swap = swap,
                                       hdist_y = hdist_y,
                                       temporary = TRUE)
        
        if(swap) loonWidgets_info$x[brush_id] <- hdist_y else loonWidgets_info$y[brush_id] <- hdist_y
        
      } else if(move_button$vdist != 0) {
        
        vdist_x <- if(swap) {
          
          seq(
            from = min(loonWidgets_info$y[brush_id]),
            to = max(loonWidgets_info$y[brush_id]),
            length.out = length(brush_id)
          )
        } else {
          
          seq(
            from = min(loonWidgets_info$x[brush_id]),
            to = max(loonWidgets_info$x[brush_id]),
            length.out = length(brush_id)
          ) 
        }
        
        loon_grob <- move_vdist_grob(loon_grob = loon_grob,
                                       index = brush_id,
                                       swap = swap,
                                       vdist_x = vdist_x,
                                       temporary = TRUE)
        
        if(swap) loonWidgets_info$y[brush_id] <- vdist_x else loonWidgets_info$x[brush_id] <- vdist_x
        
      } else if (move_button$jitter != 0) {
        
        jitter_xy <- jitter_coord(
          x = if(swap) loonWidgets_info$y else loonWidgets_info$x,
          y = if(swap) loonWidgets_info$x else loonWidgets_info$y,
          index = brush_id
        )
        
        loon_grob <- move_jitter_grob(loon_grob = loon_grob,
                                        index = brush_id,
                                        swap = swap,
                                        jitter_xy = jitter_xy,
                                        temporary = TRUE)
        
        if(swap) {
          
          loonWidgets_info$y[brush_id] <- jitter_xy$x
          loonWidgets_info$x[brush_id] <- jitter_xy$y 
        } else {
          
          loonWidgets_info$x[brush_id] <- jitter_xy$x
          loonWidgets_info$y[brush_id] <- jitter_xy$y
        }
      } else if(move_button$grid != 0) {
        
        square_xy <- square_coord(
          x = if(swap) loonWidgets_info$y else loonWidgets_info$x,
          y = if(swap) loonWidgets_info$x else loonWidgets_info$y,
          index = brush_id
        )
        
        loon_grob <- move_grid_grob(loon_grob = loon_grob,
                                    index = brush_id,
                                    swap = swap,
                                    square_xy = square_xy,
                                    temporary = TRUE)
        
        if(swap) {
          
          loonWidgets_info$y[brush_id] <- square_xy$x
          loonWidgets_info$x[brush_id] <- square_xy$y
        } else {
          
          loonWidgets_info$x[brush_id] <- square_xy$x
          loonWidgets_info$y[brush_id] <- square_xy$y
        }
      } else if(move_button$reset != 0) {
        
        loon_grob <- move_reset_grob(loon_grob = loon_grob,
                                     index = brush_id,
                                     swap = swap,
                                     xy_original = loonWidgets_info$xy_original,
                                     temporary = TRUE)
        
        loonWidgets_info$x <- loonWidgets_info$x_original
        loonWidgets_info$y <- loonWidgets_info$y_original
        
      } else NULL # none of move buttons is active
    }
    
    # reorder selected points
    loon_grob <- reorder_grob(loon_grob,
                              number = N,
                              brush_id)
    
    
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
                                      parent = "l_graph_layers")
      
    } else if (layer_button$down != 0) {
      
      loon_grob <- move_layer_down_grob(loon_grob = loon_grob,
                                        current_layer = current_layer,
                                        parent = "l_graph_layers")
      
    } else if (layer_button$visible != 0) {
      
      loon_grob <- move_layer_visible_grob(loon_grob = loon_grob,
                                           current_layer = current_layer,
                                           graph_edges = loonWidgets_info$graph_edges,
                                           N = N)
      
    } else if (layer_button$invisible != 0) {
      
      loon_grob <- move_layer_invisible_grob(loon_grob = loon_grob,
                                             current_layer = current_layer,
                                             N = N)
      
    } else if (layer_button$plus != 0) {
      message("adding layers has not been inplemented so far")
    } else if (layer_button$minus != 0) {
      
      loon_grob <- setGrob(
        gTree = loon_grob,
        gPath = current_layer,
        newGrob = nullGrob(name = current_layer)
      )
    } else if (layer_button$scale_to != 0) {
      
      if(current_layer == "graph") {
        
        if(swap) {
          
          loonWidgets_info$ylim <- loonWidgets_info$plotView_xlim
          loonWidgets_info$xlim <- loonWidgets_info$plotView_ylim
        } else {
          
          loonWidgets_info$xlim <- loonWidgets_info$plotView_xlim
          loonWidgets_info$ylim <- loonWidgets_info$plotView_ylim
        }
        
      } else {
        
        layer_lim <- get_layer_worldView(loon_grob, layer = current_layer)
        xlim <- layer_lim$xlim
        ylim <- layer_lim$ylim
        
        if(length(xlim) != 0 & length(ylim) != 0) {
          
          if(swap) {
            
            loonWidgets_info$ylim <-xlim
            loonWidgets_info$xlim <- ylim
          } else {
            
            loonWidgets_info$xlim <- xlim
            loonWidgets_info$ylim <- ylim
          }
        } else message("group layer cannot be scaled to")
      }
    } else NULL
    
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

    # remove axes
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "axes",
      newGrob = nullGrob(name = "axes")
    )

    # remove clip
    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "clipping region",
      newGrob = nullGrob(name = "clipping region")
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

    loon_grob <- setGrob(
      gTree = loon_grob,
      gPath = "loon plot",
      newGrob = editGrob(
        grob = getGrob(loon_grob, "loon plot"),
        vp = vpStack(
          plotViewport(margins = rep(1,4), name = "plotViewport"),
          dataViewport(xscale = if(swap) loonWidgets_info$worldView_ylim else loonWidgets_info$worldView_xlim, 
                       yscale = if(swap) loonWidgets_info$worldView_xlim else loonWidgets_info$worldView_ylim, 
                       name = "dataViewport")
        )
      )
    )
    
    loon_grob <- addGrob(
      gTree = loon_grob,
      gPath = "loon plot",
      child = rectGrob(
        x = unit(mean(loonWidgets_info$xlim), "native"),
        y = unit(mean(loonWidgets_info$ylim), "native"),
        width = unit(diff(loonWidgets_info$xlim), "native"),
        height = unit(diff(loonWidgets_info$ylim), "native"),
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
