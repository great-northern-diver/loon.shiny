button_values.l_hist <- function(loon_grob, tabPanelName, input, colorList) {
  
  # static
  static_button <- reactiveValues(all = 0,
                                  none = 0,
                                  invert = 0)
  # active
  active_button <- reactiveValues(deactive = 0,
                                  reactive = 0)
  # color
  color_names <- c(colorList, "modify")
  color_button <- do.call(
    reactiveValues,
    setNames(
      lapply(1:(1+length(colorList)), function(i) 0),
      color_names
    )
  )
  
  # scale
  scale_to_button <- reactiveValues(plot = 0,
                                    world = 0)
  
  # layers
  layer_button <- reactiveValues(up = 0,
                                 down = 0,
                                 visible = 0,
                                 invisible = 0,
                                 plus = 0,
                                 minus = 0,
                                 scale_to = 0,
                                 set = 0)
  
  observeEvent(input[[paste0(tabPanelName, "plot_scale_to_plot")]], {
    
    scale_to_button$plot <- 1
    scale_to_button$world <- 0
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
        for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
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
    scale_to_button$plot <- 0
    scale_to_button$world <- 1
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    
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
    static_button$all <- 1
    static_button$none <- 0
    static_button$invert <- 0
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    static_button$all <- 0
    static_button$none <- 1
    static_button$invert <- 0
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 1
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0

    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    active_button$deactive <- 1
    active_button$reactive <- 0

    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    active_button$deactive <- 0
    active_button$reactive <- 1

    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
             # active
             active_button$deactive <- 0
             active_button$reactive <- 0
             # color
             # color
             for(col_n in color_names) {
               color_button[[col_n]] <- 0
             }
             color_button[[col_n_]] <- 1
             # static
             static_button$all <- 0
             static_button$none <- 0
             static_button$invert <- 0
             # scale to
             scale_to_button$plot <- 0
             scale_to_button$world <- 0
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
  
  observeEvent(input[[paste0(tabPanelName, "modify_color")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    
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
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    
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
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    
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
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    
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
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    
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
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    
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
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    
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
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    
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
    
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    
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
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
    
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
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
  
  observeEvent(input[[paste0(tabPanelName, "ylim")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
  
  observeEvent(input[[paste0(tabPanelName, "plot_show")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
  
  observeEvent(input[[paste0(tabPanelName, "yshows")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
  
  observeEvent(input[[paste0(tabPanelName, "linkingGroup")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
  
  observeEvent(input[[paste0(tabPanelName, "linkedStates")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
  
  observeEvent(input[[paste0(tabPanelName, "binwidth")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
  
  observeEvent(input[[paste0(tabPanelName, "origin")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
     for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # scale to
    scale_to_button$plot <- 0
    scale_to_button$world <- 0
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
    active_button = active_button,
    static_button = static_button,
    scale_to_button = scale_to_button,
    layer_button = layer_button
  )
}
