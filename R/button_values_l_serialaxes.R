button_values.l_serialaxes <- function(loon_grob, tabPanelName, input, colorList) {
  
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
  # size
  size_button <- reactiveValues(abs_to_plus = 0, abs_to_minus = 0, 
                                rel_to_plus = 0, rel_to_minus = 0)
  
  observeEvent(input[[paste0(tabPanelName, "select_static_all")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 1
    static_button$none <- 0
    static_button$invert <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "select_static_none")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 1
    static_button$invert <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "select_static_invert")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 1
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_deactive")]], {
    # active
    active_button$deactive <- 1
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "modify_reactive")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 1
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
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
             for(col_n in color_names) {
               color_button[[col_n]] <- 0
             }
             color_button[[col_n_]] <- 1
             # static
             static_button$all <- 0
             static_button$none <- 0
             static_button$invert <- 0
             # size
             size_button$abs_to_plus <- 0
             size_button$abs_to_minus <- 0
             size_button$rel_to_plus <- 0
             size_button$rel_to_minus <- 0
           })
         }
  )
  
  observeEvent(input[[paste0(tabPanelName, "abs_to_plus")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # size
    size_button$abs_to_plus <- 1
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "abs_to_minus")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 1
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "rel_to_plus")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 1
    size_button$rel_to_minus <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "rel_to_minus")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 1
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
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
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
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
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
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
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
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "plot")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "axesLayout")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
  })
  
  observeEvent(input[[paste0(tabPanelName, "scaling")]], {
    # active
    active_button$deactive <- 0
    active_button$reactive <- 0
    # color
    for(col_n in color_names) color_button[[col_n]] <- 0
    # static
    static_button$all <- 0
    static_button$none <- 0
    static_button$invert <- 0
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
  })
  
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
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
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
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
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
    # size
    size_button$abs_to_plus <- 0
    size_button$abs_to_minus <- 0
    size_button$rel_to_plus <- 0
    size_button$rel_to_minus <- 0
  })
  
  list(
    color_button = color_button,
    size_button = size_button,
    static_button = static_button,
    active_button = active_button
  )
}

