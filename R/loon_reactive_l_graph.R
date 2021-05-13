loon_reactive.l_graph <- function(loon_grob, output_grob, linkingInfo, buttons, position, selectBy,
                                  linkingGroup, input, tabPanelName, output_info) {
  
  input$plot_brush
  input$plot_click
  
  if(!is.null(output_grob) & input[["navBarPage"]] != tabPanelName) {
    
    loonWidgets_info <- output_info$loonWidgets_info
    
    if(linkingGroup != "none") {
      
      grobs <- set_linking_grobs(
        loon_grob = loon_grob,
        output_grob = output_grob,
        linkedInfo = linkingInfo[[linkingGroup]],
        tabPanelName = tabPanelName,
        loon_color = loonWidgets_info$loon_color,
        graph_edges = loonWidgets_info$graph_edges,
        swap_in_loon = loonWidgets_info$swap_in_loon,
        swap_in_shiny = loonWidgets_info$swap_in_shiny
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
    
    isFirst_draw <- is.null(output_grob)
    output_grob <- loon_grob
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
    swap_in_shiny <- loonWidgets_info$swap_in_shiny <- "swap" %in% plot_axes1
    swap <- ((swap_in_shiny & !swap_in_loon) | (!swap_in_shiny & swap_in_loon))
    
    N <- length(loonWidgets_info$linkingKey)
    
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
          swap_in_shiny = swap_in_shiny,
          swap_in_loon = swap_in_loon,
          position = position,
          brush_info = input$plot_brush,
          vp = get_viewPort(loon_grob = output_grob),
          click_info = input$plot_click
        )
      }
    }
    
    #labels <- get_labels(output_grob)
    labels <- loonWidgets_info$labels
    title <- labels$title
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
      
      # swap label
      ylabel <- labels$xlabel
      xlabel <- labels$ylabel
      
      # swap output grob
      output_grob <- swapCoords_grob(output_grob, 
                                     x = loonWidgets_info$y, 
                                     y = loonWidgets_info$x, 
                                     reactive = FALSE)
      # swap layer
      output_grob <- swap_layer_grob(output_grob, parent = "graph")
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
      
      xlabel <- labels$xlabel
      ylabel <- labels$ylabel
    }
    
    xaxis <- grid.pretty(loonWidgets_info$xlim)
    yaxis <- grid.pretty(loonWidgets_info$ylim)
    
    # reset margins
    loon_margins <- loonWidgets_info$loon_default_margins
    margins <- rep(0, 4)
    
    if("scales" %in% plot_axes2) {
      
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
    
    if("labels" %in% plot_axes1) {
      
      output_grob <- set_labels_grob(
        loon_grob = output_grob,
        showScales = loonWidgets_info$showScales,
        xlabel = xlabel,
        ylabel = ylabel,
        title = title
      )
      
      if(is.null(xlabel)) loon_margins$labelMargins[1] <- loon_margins$minimumMargins[1]
      if(is.null(ylabel)) loon_margins$labelMargins[2] <- loon_margins$minimumMargins[2]
      if(title == "") loon_margins$labelMargins[3] <- loon_margins$minimumMargins[3]
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
    
    if(loonWidgets_info$showLabels | loonWidgets_info$showScales) {
      margins <- apply(cbind(margins, loon_margins$minimumMargins), 1, max)
    }
    
    brush_id <- if(!isFirst_draw) {
      # sweeping or brushing
      if(is.null(input$plot_brush) & is.null(input$plot_click)) {
        
        output_info$brush_id
      } else {
        
        get_brush_id(
          loon_grob = output_grob,
          coord = list(
            x = loonWidgets_info$x,
            y = loonWidgets_info$y
          ),
          swap_in_shiny = swap_in_shiny,
          swap_in_loon = swap_in_loon,
          position = position,
          brush_info = input$plot_brush,
          vp = vpStack(
            plotViewport(margins = margins, name = "plotViewport"),
            dataViewport(xscale = if(swap) loonWidgets_info$ylim else loonWidgets_info$xlim,
                         yscale = if(swap) loonWidgets_info$xlim else loonWidgets_info$ylim,
                         name = "dataViewport")
          ),
          click_info = input$plot_click
        )
      }
    }
    
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
    loon_color <- loonWidgets_info$loon_color
    linkingGroup <- input[[paste0(tabPanelName, "linkingGroup")]]
    
    # select dynamic
    select_dynamic <- input[[paste0(tabPanelName, "select_dynamic")]]
    sticky <- input[[paste0(tabPanelName, "sticky")]]
    select_by_color <- input[[paste0(tabPanelName, "select_by_color")]]
    
    if(sticky == "off") {
      
      if(!is.null(select_by_color)) {
        
        # when select_by_color is on, we can use brush to clear selection but keep brush id
        loonWidgets_info$lastSelection <- if(!is.null(input$plot_brush) | !is.null(input$plot_click)) brush_id else integer(0)
        brush_id <-  which(loonWidgets_info$color %in% select_by_color)
      } else {
        
        if(!is.null(output_info$select_by_color)) brush_id <- loonWidgets_info$lastSelection
      }
      
      if("deselect" == select_dynamic) {
        if(!is.null(input$plot_brush) | !is.null(input$plot_click)) brush_id <- integer(0)
      }
      
    } else {
      
      if(!is.null(select_by_color)) {
        
        which_is_selected <- union(which(loonWidgets_info$color %in% select_by_color), which(loonWidgets_info$selected))
        
      } else {
        
        which_is_selected <- which(loonWidgets_info$selected)
      }
      
      if("invert" == select_dynamic) {
        
        if(is.null(input$plot_brush) | !is.null(input$plot_click)) {
          brush_id <- which_is_selected
        } else {
          brush_id <- union(setdiff(which_is_selected, brush_id), setdiff(brush_id, which_is_selected))
        }
      } else if("deselect" == select_dynamic) {
        
        if(is.null(input$plot_brush) | !is.null(input$plot_click)) {
          brush_id <- which_is_selected
        } else {
          brush_id <- setdiff(which_is_selected, brush_id)
        }
        
      } else {
        
        if(is.null(input$plot_brush) | !is.null(input$plot_click)) {
          brush_id <- which_is_selected
        } else {
          brush_id <- union(which_is_selected, brush_id)
        }
      }
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
    
    # highlight color
    output_grob <- set_color_grob(
      loon_grob = output_grob,
      index = brush_id,
      color = loon_color$select_color[1],
      size = loonWidgets_info$size,
      pch = loonWidgets_info$pch,
      loon_color = loonWidgets_info$loon_color
    )
    
    colorList <- loonWidgets_info$colorList
    input[[paste0(tabPanelName, "color")]]
    lapply(colorList, function(col) input[[paste0(tabPanelName, col)]])
    modify_color <- isolate(input[[paste0(tabPanelName, "modify_color")]])
    
    if(buttons$color_button$modify != 0) {
      
      loon_grob <- set_color_grob(
        loon_grob = loon_grob,
        index = brush_id,
        color = modify_color,
        size = loonWidgets_info$size,
        pch = loonWidgets_info$pch,
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
          size = loonWidgets_info$size,
          pch = loonWidgets_info$pch,
          loon_color = loonWidgets_info$loon_color
        )
        
        loonWidgets_info$color[brush_id] <- col
      }
    }
    
    # adjust deactive--------------------------------
    which_is_deactive <- which(!loonWidgets_info$active)
    output_grob <- set_deactive_grob(
      loon_grob = output_grob,
      index = which_is_deactive
    )
    
    loon_grob <- set_deactive_grob(
      loon_grob = loon_grob,
      index = which_is_deactive
    )
    
    input[[paste0(tabPanelName, "modify_deactive")]]
    if(buttons$active_button$deactive != 0) {
      
      output_grob <- set_deactive_grob(
        loon_grob = output_grob,
        index = brush_id
      )
      
      loon_grob <- set_deactive_grob(
        loon_grob = loon_grob,
        index = brush_id
      )
      
      loonWidgets_info$active[brush_id] <- FALSE
      which_is_deactive <- union(which_is_deactive, brush_id)
    }
    
    input[[paste0(tabPanelName, "modify_reactive")]]
    if (buttons$active_button$reactive != 0) {
      
      output_grob <- set_reactive_grob(
        loon_grob = output_grob,
        index = which_is_deactive,
        graph_edges = loonWidgets_info$graph_edges,
        swap = swap
      )
      
      loon_grob <- set_reactive_grob(
        loon_grob = loon_grob,
        index = which_is_deactive,
        graph_edges = loonWidgets_info$graph_edges,
        swap = FALSE
      )
      
      which_is_deactive <- numeric(0)
      loonWidgets_info$active <- rep(TRUE, N)
      
    } else NULL
    
    showOrbit <- input[[paste0(tabPanelName, "show_nodes_label")]]
    if(showOrbit) {
      
      output_grob <- set_node_labels_grob(output_grob, which_is_deactive)
      
      loonWidgets_info$showOrbit <- TRUE
    } else {
      
      output_grob <- setGrob(
        gTree = output_grob,
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
    
    if(move_button$halign != 0) {
      
      # to determine if the default widget is swapped
      halign_y <- if(swap) mean(loonWidgets_info$x[brush_id]) else mean(loonWidgets_info$y[brush_id])
      
      output_grob <- move_halign_grob(loon_grob = output_grob,
                                      index = brush_id,
                                      swap = swap,
                                      halign_y = halign_y,
                                      temporary = TRUE)
      
      loon_grob <- move_halign_grob(loon_grob = loon_grob,
                                    index = brush_id,
                                    swap = swap,
                                    halign_y = halign_y,
                                    temporary = FALSE)
      
      if(swap) loonWidgets_info$x[brush_id] <- halign_y else loonWidgets_info$y[brush_id] <- halign_y
      
    } else if(move_button$valign != 0) {
      
      valign_x <- if(swap) mean(loonWidgets_info$y[brush_id]) else mean(loonWidgets_info$x[brush_id])
      
      output_grob <- move_valign_grob(loon_grob = output_grob,
                                      index = brush_id,
                                      swap = swap,
                                      valign_x = valign_x,
                                      temporary = TRUE)
      
      loon_grob <- move_valign_grob(loon_grob = loon_grob,
                                    index = brush_id,
                                    swap = swap,
                                    valign_x = valign_x,
                                    temporary = FALSE)
      
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
      
      output_grob <- move_hdist_grob(loon_grob = output_grob,
                                     index = brush_id,
                                     swap = swap,
                                     hdist_y = hdist_y,
                                     temporary = TRUE)
      
      loon_grob <- move_hdist_grob(loon_grob = loon_grob,
                                   index = brush_id,
                                   swap = swap,
                                   hdist_y = hdist_y,
                                   temporary = FALSE)
      
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
      
      output_grob <- move_vdist_grob(loon_grob = output_grob,
                                     index = brush_id,
                                     swap = swap,
                                     vdist_x = vdist_x,
                                     temporary = TRUE)
      
      loon_grob <- move_vdist_grob(loon_grob = loon_grob,
                                   index = brush_id,
                                   swap = swap,
                                   vdist_x = vdist_x,
                                   temporary = FALSE)
      
      if(swap) loonWidgets_info$y[brush_id] <- vdist_x else loonWidgets_info$x[brush_id] <- vdist_x
      
    } else if (move_button$jitter != 0) {
      
      jitter_xy <- jitter_coord(
        x = if(swap) loonWidgets_info$y else loonWidgets_info$x,
        y = if(swap) loonWidgets_info$x else loonWidgets_info$y,
        index = brush_id
      )
      
      output_grob <- move_jitter_grob(loon_grob = output_grob,
                                      index = brush_id,
                                      swap = swap,
                                      jitter_xy = jitter_xy,
                                      temporary = TRUE)
      
      loon_grob <- move_jitter_grob(loon_grob = loon_grob,
                                    index = brush_id,
                                    swap = swap,
                                    jitter_xy = jitter_xy,
                                    temporary = FALSE)
      
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
      
      output_grob <- move_grid_grob(loon_grob = output_grob,
                                    index = brush_id,
                                    swap = swap,
                                    square_xy = square_xy,
                                    temporary = TRUE)
      
      loon_grob <- move_grid_grob(loon_grob = loon_grob,
                                  index = brush_id,
                                  swap = swap,
                                  square_xy = square_xy,
                                  temporary = FALSE)
      
      if(swap) {
        
        loonWidgets_info$y[brush_id] <- square_xy$x
        loonWidgets_info$x[brush_id] <- square_xy$y
      } else {
        
        loonWidgets_info$x[brush_id] <- square_xy$x
        loonWidgets_info$y[brush_id] <- square_xy$y
      }
    } else if(move_button$reset != 0) {
      
      output_grob <- move_reset_grob(loon_grob = output_grob,
                                     index = seq(N),
                                     swap = swap,
                                     xy_original = loonWidgets_info$xy_original,
                                     temporary = TRUE)
      
      loon_grob <- move_reset_grob(loon_grob = loon_grob,
                                   index = seq(N),
                                   swap = swap,
                                   xy_original = loonWidgets_info$xy_original,
                                   temporary = FALSE)
      
      loonWidgets_info$x <- loonWidgets_info$x_original
      loonWidgets_info$y <- loonWidgets_info$y_original
      
    } else NULL # none of move buttons is active
    
    
    # adjust glyph size--------------------------------
    glyph_button <- list(
      circle = buttons$glyph_button$circle,
      ccircle = buttons$glyph_button$ccircle,
      ocircle = buttons$glyph_button$ocircle,
      square = buttons$glyph_button$square,
      csquare = buttons$glyph_button$csquare,
      osquare = buttons$glyph_button$osquare,
      triangle = buttons$glyph_button$triangle,
      ctriangle = buttons$glyph_button$ctriangle,
      otriangle = buttons$glyph_button$otriangle
    )
    which_glyph_is_active <- which(glyph_button != 0)
    
    if(length(which_glyph_is_active) > 0)  {
      
      new_glyph <- names(glyph_button[which_glyph_is_active])
      new_pch <- glyph_to_pch(new_glyph)
      
      loon_grob <- set_glyph_grob(
        loon_grob = loon_grob,
        index = brush_id,
        new_pch = new_pch,
        tmp = FALSE,
        color = loonWidgets_info$color,
        size = loonWidgets_info$size,
        pch = loonWidgets_info$pch,
        loon_color = loonWidgets_info$loon_color
      )
      
      output_grob <- set_glyph_grob(
        loon_grob = output_grob,
        index = brush_id,
        new_pch = new_pch,
        tmp = TRUE,
        color = loonWidgets_info$color,
        size = loonWidgets_info$size,
        pch = loonWidgets_info$pch,
        loon_color = loonWidgets_info$loon_color
      )
      
      loonWidgets_info$glyph[brush_id] <- new_glyph
      loonWidgets_info$pch[brush_id] <- new_pch
      loonWidgets_info$glyph_name[brush_id] <- paste0("primitive_glyph ", loonWidgets_info$index[brush_id])
    }
    
    # adjust size--------------------------------
    input[[paste0(tabPanelName, "abs_to_plus")]]
    input[[paste0(tabPanelName, "abs_to_minus")]]
    input[[paste0(tabPanelName, "rel_to_plus")]]
    input[[paste0(tabPanelName, "rel_to_minus")]]
    
    if(buttons$size_button$abs_to_plus != 0) {
      
      if(length(brush_id) > 0) {
        new_size <- min(loonWidgets_info$size[brush_id]) + default_step_size()
        loonWidgets_info$size[brush_id] <- rep(new_size, length(brush_id))
        
        loon_grob <- set_size_grob(loon_grob = loon_grob,
                                   index = brush_id,
                                   new_size = loonWidgets_info$size,
                                   pch = loonWidgets_info$pch)
        
        output_grob <- set_size_grob(loon_grob = output_grob,
                                     index = brush_id,
                                     new_size = loonWidgets_info$size,
                                     pch = loonWidgets_info$pch)
      }
    }
    
    if(buttons$size_button$abs_to_minus != 0) {
      
      if(length(brush_id) > 0) {
        new_size <- min(loonWidgets_info$size[brush_id]) - default_step_size()
        if(new_size <= 0) new_size <- minimumSize()
        loonWidgets_info$size[brush_id] <- rep(new_size, length(brush_id))
        
        loon_grob <- set_size_grob(loon_grob = loon_grob,
                                   index = brush_id,
                                   new_size = loonWidgets_info$size,
                                   pch = loonWidgets_info$pch)
        
        output_grob <- set_size_grob(loon_grob = output_grob,
                                     index = brush_id,
                                     new_size = loonWidgets_info$size,
                                     pch = loonWidgets_info$pch)
      }
    }
    
    if(buttons$size_button$rel_to_plus != 0) {
      
      if(length(brush_id) > 0) {
        
        loonWidgets_info$size[brush_id] <- loonWidgets_info$size[brush_id] + default_step_size()
        
        loon_grob <- set_size_grob(loon_grob = loon_grob,
                                   index = brush_id,
                                   new_size = loonWidgets_info$size,
                                   pch = loonWidgets_info$pch)
        
        output_grob <- set_size_grob(loon_grob = output_grob,
                                     index = brush_id,
                                     new_size = loonWidgets_info$size,
                                     pch = loonWidgets_info$pch)
      }
    }
    
    if(buttons$size_button$rel_to_minus != 0) {
      
      if(length(brush_id) > 0) {
        
        new_size <- loonWidgets_info$size[brush_id] - default_step_size()
        new_size[which(new_size <= 0)] <- minimumSize()
        loonWidgets_info$size[brush_id] <- new_size
        
        loon_grob <- set_size_grob(loon_grob = loon_grob,
                                   index = brush_id,
                                   new_size = loonWidgets_info$size,
                                   pch = loonWidgets_info$pch)
        
        output_grob <- set_size_grob(loon_grob = output_grob,
                                     index = brush_id,
                                     new_size = loonWidgets_info$size,
                                     pch = loonWidgets_info$pch)
      }
    }
    
    # reorder selected points
    output_grob <- reorder_grob(output_grob,
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
      
      output_grob <- move_layer_up_grob(loon_grob = output_grob,
                                        current_layer = current_layer,
                                        parent = "l_graph_layers")
      
    } else if (layer_button$down != 0) {
      
      loon_grob <- move_layer_down_grob(loon_grob = loon_grob,
                                        current_layer = current_layer,
                                        parent = "l_graph_layers")
      
      output_grob <- move_layer_down_grob(loon_grob = output_grob,
                                          current_layer = current_layer,
                                          parent = "l_graph_layers")
      
    } else if (layer_button$visible != 0) {
      
      loon_grob <- move_layer_visible_grob(loon_grob = loon_grob,
                                           current_layer = current_layer,
                                           graph_edges = loonWidgets_info$graph_edges,
                                           N = N)
      
      output_grob <- move_layer_visible_grob(loon_grob = output_grob,
                                             current_layer = current_layer,
                                             graph_edges = loonWidgets_info$graph_edges,
                                             N = N)
      
    } else if (layer_button$invisible != 0) {
      
      loon_grob <- move_layer_invisible_grob(loon_grob = loon_grob,
                                             current_layer = current_layer,
                                             N = N)
      
      output_grob <- move_layer_invisible_grob(loon_grob = output_grob,
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
      
      output_grob <- setGrob(
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
    
    # reset vp
    output_grob <- set_viewPort_grob(
      loon_grob = output_grob,
      margins = margins, 
      xlim = loonWidgets_info$xlim,
      ylim = loonWidgets_info$ylim
    )
    
    # reset boundary
    output_grob <- set_boundary_grob(loon_grob = output_grob,
                                     margins = margins, 
                                     loon_color = loonWidgets_info$loon_color)
    
    
    # set linking info
    linkingInfo <- update_linkingInfo(loon_grob,
                                      tabPanelName = tabPanelName,
                                      linkingInfo = linkingInfo, 
                                      linkingGroup = linkingGroup, 
                                      selected = loonWidgets_info$selected,
                                      color = loonWidgets_info$color, 
                                      size = loonWidgets_info$size, 
                                      pch = loonWidgets_info$pch, 
                                      active = loonWidgets_info$active, 
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
