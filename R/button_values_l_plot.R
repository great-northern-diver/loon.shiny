button_values <- function(loon_grob, tabPanelName, input, colorList) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("button_values", obj)
}

button_values.default <- function(loon_grob, tabPanelName, input, colorList) NULL

button_values.l_plot <- function(loon_grob, tabPanelName, input, colorList) {
  
  # static
  static_button <- shiny::reactiveValues(all = 0,
                                         none = 0,
                                         invert = 0)
  # active
  active_button <- shiny::reactiveValues(deactive = 0,
                                         reactive = 0)
  # color
  color_names <- c(colorList, "modify")
  color_button <- do.call(
    shiny::reactiveValues,
    stats::setNames(
      lapply(1:(1+length(colorList)), function(i) 0),
      color_names
    )
  )
  
  # glyph
  glyph_button <- shiny::reactiveValues(circle = 0, square = 0, triangle = 0,
                                        ocircle = 0, osquare = 0, otriangle = 0,
                                        ccircle = 0, csquare = 0, ctriangle = 0)
  
  glyph_set_button <- shiny::reactiveValues(glyph_set = 0)
  
  # move
  move_button <- shiny::reactiveValues(halign = 0, valign = 0, hdist = 0,
                                       vdist = 0, grid = 0, jitter = 0, reset = 0)
  
  # size
  size_button <- shiny::reactiveValues(abs_to_plus = 0, abs_to_minus = 0, 
                                       rel_to_plus = 0, rel_to_minus = 0)
  
  # scale
  scale_to_button <- shiny::reactiveValues(select = 0,
                                           plot = 0,
                                           world = 0)
  
  # layers
  layer_button <- shiny::reactiveValues(up = 0,
                                        down = 0,
                                        visible = 0,
                                        invisible = 0,
                                        plus = 0,
                                        minus = 0,
                                        scale_to = 0,
                                        set = 0)
  
  observeEvent(input[[paste0(tabPanelName, "glyph_set")]], {
    glyph_set_button$glyph_set <- 1
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_move_halign")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 1
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_move_valign")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 1
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0     
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_move_hdist")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 1
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_move_vdist")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 1
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_move_grid")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 1
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_move_jitter")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 1
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_move_reset")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 1
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "plot_scale_to_select")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 1
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "plot_scale_to_plot")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 1
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "plot_scale_to_world")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 1
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  
  observeEvent(input[[paste0(tabPanelName, "select_static_all")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 1
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "select_static_none")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 1
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "select_static_invert")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 1
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_deactive")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 1
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_reactive")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 1
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_glyph_circle")]], {
    glyph_set_button$glyph_set <- 0
    glyph_button$circle <- 1
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_glyph_square")]], {
    glyph_set_button$glyph_set <- 0
    glyph_button$circle <- 0
    glyph_button$square <- 1
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_glyph_triangle")]], {
    glyph_set_button$glyph_set <- 0
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 1
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_glyph_ccircle")]], {
    glyph_set_button$glyph_set <- 0
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 1
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_glyph_csquare")]], {
    glyph_set_button$glyph_set <- 0
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 1
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_glyph_ctriangle")]], {
    glyph_set_button$glyph_set <- 0
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 1
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_glyph_ocircle")]], {
    glyph_set_button$glyph_set <- 0
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 1
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_glyph_osquare")]], {
    glyph_set_button$glyph_set <- 0
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 1
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_glyph_otriangle")]], {
    glyph_set_button$glyph_set <- 0
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 1
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_color")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    
    for(col_n in color_names) color_button[[col_n]] <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "layer_up")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    
    layer_button$up <- 1
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
    
  })
  
  observeEvent(input[[paste0(tabPanelName, "layer_down")]], {    
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    
    layer_button$up <- 0
    layer_button$down <- 1
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
    
  })
  
  observeEvent(input[[paste0(tabPanelName, "layer_visible")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 1
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "layer_invisible")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 1
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "layer_plus")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 1
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "layer_minus")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 1
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "layer_scale_to")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 1
    layer_button$set <- 0
  })
  
  
  observeEvent(input[[paste0(tabPanelName, "layer_set")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 1
  })
  
  observeEvent(input[[paste0(tabPanelName, "layer_changed_label")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input$plot_click, {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input$plot_brush, {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "sticky")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "xlim")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "abs_to_plus")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 1
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "abs_to_minus")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 1 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "rel_to_plus")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 1
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "rel_to_minus")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 1
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "layer")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "select_by_color")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "plot_axes1")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "plot_axes2")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "select_dynamic")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  lapply(color_names, 
         function(col_n_){
           
           tab_name <- if(col_n_ == "modify") {
             "color"
           } else {
             col_n_
           }
           
           observeEvent(input[[paste0(tabPanelName, tab_name)]], {
             glyph_set_button$glyph_set <- 0
             # glyph
             glyph_button$circle <- 0
             glyph_button$square <- 0
             glyph_button$triangle <- 0
             glyph_button$ccircle <- 0
             glyph_button$csquare <- 0
             glyph_button$ctriangle <- 0
             glyph_button$ocircle <- 0
             glyph_button$osquare <- 0
             glyph_button$otriangle <- 0
             # active
             active_button$deactive <- 0
             active_button$reactive <- 0
             
             # color
             for(col_n in color_names) {
               color_button[[col_n]] <- 0
             }
             color_button[[col_n_]] <- 1
             
             # size
             size_button$abs_to_plus <- 0
             size_button$abs_to_minus <- 0
             size_button$rel_to_plus <- 0
             size_button$rel_to_minus <- 0
             # static
             static_button$all <- 0
             static_button$none <- 0
             static_button$invert <- 0
             # scale to
             scale_to_button$select <- 0
             scale_to_button$plot <- 0
             scale_to_button$world <- 0
             # move
             move_button$halign <- 0
             move_button$valign <- 0
             move_button$hdist <- 0
             move_button$vdist <- 0
             move_button$grid <- 0
             move_button$jitter <- 0
             move_button$reset <- 0
             # layers
             layer_button$up <- 0
             layer_button$down <- 0
             layer_button$visible <- 0
             layer_button$invisible <- 0
             layer_button$plus <- 0
             layer_button$minus <- 0
             layer_button$scale_to <- 0
             layer_button$set <- 0
           })
         }
  )
  
  observeEvent(input[[paste0(tabPanelName, "ylim")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "glyph_setting")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "linkinGroup")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "linkingStates")]], {
    glyph_set_button$glyph_set <- 0
    # glyph
    glyph_button$circle <- 0
    glyph_button$square <- 0
    glyph_button$triangle <- 0
    glyph_button$ccircle <- 0
    glyph_button$csquare <- 0
    glyph_button$ctriangle <- 0
    glyph_button$ocircle <- 0
    glyph_button$osquare <- 0
    glyph_button$otriangle <- 0
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0 
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$select <- 0
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    # move
    move_button$halign <- 0
    move_button$valign <- 0
    move_button$hdist <- 0
    move_button$vdist <- 0
    move_button$grid <- 0
    move_button$jitter <- 0
    move_button$reset <- 0
    # layers
    layer_button$up <- 0
    layer_button$down <- 0
    layer_button$visible <- 0
    layer_button$invisible <- 0
    layer_button$plus <- 0
    layer_button$minus <- 0    
    layer_button$scale_to <- 0
    layer_button$set <- 0
  })
  
  list(
    color_button = color_button,
    glyph_button = glyph_button,
    active_button = active_button,
    size_button = size_button,
    static_button = static_button,
    scale_to_button = scale_to_button,
    move_button = move_button,
    layer_button = layer_button,
    glyph_set_button = glyph_set_button
  )
}

