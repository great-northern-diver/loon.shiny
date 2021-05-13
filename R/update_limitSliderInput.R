update_limitSliderInput <- function(loon_grob, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("update_limitSliderInput", obj)
}

update_limitSliderInput.default <- function(loon_grob, loonWidgets_info, input, session, buttons, tabPanelName,
                                            brush_id) {
  
  swap_in_loon <- loonWidgets_info$swap_in_loon
  swap_in_shiny <- "swap" %in% input[[paste0(tabPanelName, "plot_axes1")]]
  swap <- ((swap_in_shiny & !swap_in_loon) | (!swap_in_shiny & swap_in_loon))
  
  worldView_xlim <- loonWidgets_info$worldView_xlim 
  worldView_ylim <- loonWidgets_info$worldView_ylim 
  
  plotView_xlim <- loonWidgets_info$plotView_xlim
  plotView_ylim <- loonWidgets_info$plotView_ylim
  
  minX <- min(worldView_xlim) - diff(worldView_xlim)
  maxX <- max(worldView_xlim) + diff(worldView_xlim)
  
  minY <- min(worldView_ylim) - diff(worldView_ylim)
  maxY <- max(worldView_ylim) + diff(worldView_ylim)
  
  if(buttons$scale_to_button$plot != 0) {

    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swap) "ylim" else "xlim",
      step = log_ceiling(plotView_xlim),
      value = plotView_xlim,
      min = minX,
      max = maxX
    )
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swap) "xlim" else "ylim",
      step = log_ceiling(plotView_ylim),
      value = plotView_ylim,
      min = minY,
      max = maxY
    )
    
  } else if(buttons$scale_to_button$world != 0) {
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swap) "ylim" else "xlim",
      step = log_ceiling(worldView_xlim),
      value = worldView_xlim,
      min = minX,
      max = maxX
    )
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swap) "xlim" else "ylim",
      step = log_ceiling(worldView_ylim),
      value = worldView_ylim,
      min = minY,
      max = maxY
    )
    
  } else if(buttons$scale_to_button$select != 0) {
    
    if(length(brush_id) > 0) {
      shiny::updateSliderInput(
        session,
        inputId = paste0(tabPanelName, "xlim"),
        label = if(swap) "ylim" else "xlim",
        step = loonWidgets_info$step_x,
        value = grDevices::extendrange(
          c(
            min(loonWidgets_info$x[brush_id]) - loonWidgets_info$step_x/2,
            max(loonWidgets_info$x[brush_id]) + loonWidgets_info$step_x/2
          )
        ),
        min = minX,
        max = maxX
      )
      
      shiny::updateSliderInput(
        session,
        inputId = paste0(tabPanelName, "ylim"),
        label = if(swap) "xlim" else "ylim",
        step = loonWidgets_info$step_y,
        value =  grDevices::extendrange(
          c(
            min(loonWidgets_info$y[brush_id]) - loonWidgets_info$step_y/2,
            max(loonWidgets_info$y[brush_id]) + loonWidgets_info$step_y/2
          )
        ),
        min = minY,
        max = maxY
      )
    }
  } else if (buttons$layer_button$scale_to != 0) {
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swap) "ylim" else "xlim",
      step = loonWidgets_info$step_x,
      value = loonWidgets_info$xlim,
      min = minX,
      max = maxX
    )
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swap) "xlim" else "ylim",
      step = loonWidgets_info$step_y,
      value = loonWidgets_info$ylim,
      min = minY,
      max = maxY
    )
    
  } else {
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swap) "xlim" else "ylim",
      min = minY,
      max = maxY
    )
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swap) "ylim" else "xlim",
      min = minX,
      max = maxX
    )
  }
}

update_limitSliderInput.l_hist <- function(loon_grob, loonWidgets_info, input, session, buttons, tabPanelName) {
  
  swap_in_loon <- loonWidgets_info$swap_in_loon
  swap_in_shiny <- "swap" %in% input[[paste0(tabPanelName, "plot_axes1")]]
  
  # plot view
  plotView_xlim <- loonWidgets_info$plotView_xlim
  plotView_ylim <- loonWidgets_info$plotView_ylim
  
  yshows <- input[[paste0(tabPanelName, "yshows")]]
  origin <- input[[paste0(tabPanelName, "origin")]]
  binwidth <- input[[paste0(tabPanelName, "binwidth")]]
  
  scale_to_button <- list(
    plot = buttons$scale_to_button$plot,
    world = buttons$scale_to_button$world
  )
  
  if(loonWidgets_info$yshowsIsModified && yshows == "density") {
    scale_to_button$plot <- 1
  }
  
  if((loonWidgets_info$binwidthIsModified || loonWidgets_info$originIsModified) && yshows == "density") {
    scale_to_button$world <- 1
  }
  
  if(swap_in_loon) {
    
    worldView_xlim <- range(c(plotView_xlim, loonWidgets_info$worldView_ylim))
    worldView_ylim <- range(c(plotView_ylim, loonWidgets_info$worldView_xlim))
  } else {
    
    worldView_xlim <- range(c(plotView_xlim, loonWidgets_info$worldView_xlim))
    worldView_ylim <- range(c(plotView_ylim, loonWidgets_info$worldView_ylim))
  }
  
  minX <- min(worldView_xlim) - diff(worldView_xlim)
  maxX <- max(worldView_xlim) + diff(worldView_xlim)
  
  minY <- min(worldView_ylim) - diff(worldView_ylim)
  maxY <- max(worldView_ylim) + diff(worldView_ylim)
  
  # update sliderInput
  if(scale_to_button$plot != 0) {
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swap_in_shiny) "ylim" else "xlim",
      min = minX,
      max = maxX,
      value = plotView_xlim,
      step = log_ceiling(plotView_xlim)
    )
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swap_in_shiny) "xlim" else "ylim",
      min = minY,
      max = maxY,
      value = plotView_ylim,
      step = log_ceiling(plotView_ylim)
    )
    
  } else if (scale_to_button$world != 0) {
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swap_in_shiny) "ylim" else "xlim",
      min = minX,
      max = maxX,
      value = worldView_xlim,
      step = log_ceiling(worldView_xlim)
    )
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swap_in_shiny) "xlim" else "ylim",
      min = minY,
      max = maxY,
      value = worldView_ylim,
      step = log_ceiling(worldView_ylim)
    )
    
  }  else if (buttons$layer_button$scale_to != 0) {
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swap_in_shiny) "ylim" else "xlim",
      min = minX,
      max = maxX,
      step = log_ceiling(worldView_xlim),
      value = loonWidgets_info$xlim
    )
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swap_in_shiny) "xlim" else "ylim",
      min = minY,
      max = maxY,
      step = log_ceiling(worldView_ylim),
      value = loonWidgets_info$ylim
    )
    
  } else {
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "xlim"),
      label = if(swap_in_shiny) "ylim" else "xlim",
      min = minX,
      max = maxX
    )
    
    shiny::updateSliderInput(
      session,
      inputId = paste0(tabPanelName, "ylim"),
      label = if(swap_in_shiny) "xlim" else "ylim",
      min = minY,
      max = maxY
    )
  }
}