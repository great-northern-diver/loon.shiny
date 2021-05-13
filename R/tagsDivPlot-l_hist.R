tagsDivPlot.l_hist <- function(loon_grob, tabPanelName, 
                               loonWidgets_info, 
                               linkingGroup, linkingGroups) {
  
  viewPort <- get_viewPort(loon_grob)
  
  if(loonWidgets_info$swap_in_loon) {
    
    xlim <- round(viewPort[[2]]$yscale, 2)
    ylim <- round(viewPort[[2]]$xscale, 2)
    
  } else {
    
    xlim <- round(viewPort[[2]]$xscale, 2)
    ylim <- round(viewPort[[2]]$yscale, 2)
  }
  
  worldView_xlim <- round(range(c(loonWidgets_info$worldView_xlim, xlim)),2)
  worldView_ylim <- round(range(c(loonWidgets_info$worldView_ylim, ylim)),2)
  
  step_x <- log_ceiling(xlim)
  step_y <- log_ceiling(ylim)
  
  step_origin_binwidth <- step_x
  max_binwidth <- xlim[2]
  
  min_origin <- xlim[1] - diff(xlim)
  max_origin <- xlim[2] + diff(xlim)
  
  tags$div(
    id = paste0(tabPanelName, 'Plot'),  
    class="collapse",
    h6(""),
    do.call(
      sliderInput,
      list(
        inputId = paste0(tabPanelName, "xlim"),
        label = "xlim",
        min = worldView_xlim[1] - diff(worldView_xlim),
        max = worldView_xlim[2] + diff(worldView_xlim),
        step = step_x,
        value = xlim
      )
    ),
    do.call(
      sliderInput,
      list(
        inputId = paste0(tabPanelName, "ylim"),
        label = "ylim",
        min = worldView_ylim[1] - diff(worldView_ylim),
        max = worldView_ylim[2] + diff(worldView_ylim),
        step = step_y,
        value = ylim
      )
    ),
    fixedRow(
      column(
        3,
        h6("axes:")
      ),
      column(
        9,
        verticalLayout(
          do.call(
            checkboxGroupInput,
            list(paste0(tabPanelName, "plot_axes1"),
                 label = NULL,
                 choices = c("swap", "scales"),
                 selected = c(if(loonWidgets_info$swap_in_shiny) "swap",
                              if(loonWidgets_info$showScales) "scales"),
                 inline = TRUE)
            
          ),
          do.call(
            checkboxGroupInput,
            list(paste0(tabPanelName, "plot_axes2"),
                 label = NULL,
                 choices = c("guides", "labels"),
                 selected = c(if(loonWidgets_info$showGuides) "guides",
                              if(loonWidgets_info$showLabels) "labels"),
                 inline = TRUE)
          )
        )
      )
    ),
    fixedRow(
      column(
        3,
        h6("show:")
      ),
      column(
        9,
        do.call(
          checkboxGroupInput,
          list(paste0(tabPanelName, "plot_show"),
               label = NULL,
               choices = c("stackedColors", "outlines"),
               selected = c(if(loonWidgets_info$showStackedColors) "stackedColors",
                            if(loonWidgets_info$showOutlines) "outlines"),
               inline = TRUE
          )
        )
      )
    ),
    fixedRow(
      column(
        3,
        h6("yshows:")
      ),
      column(
        9,
        do.call(
          radioButtons,
          list(paste0(tabPanelName, "yshows"),
               label = NULL,
               choices = c("frequency", "density"),
               selected = loonWidgets_info$yshows,
               inline = TRUE)
        )
      )
    ),
    fixedRow(
      column(
        3,
        h6("scale:")
      ),
      column(
        3,
        actionButton(
          inputId = paste0(tabPanelName, "plot_scale_to_plot"),
          label = "plot",
          width = "150%",
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        3,
        actionButton(
          inputId = paste0(tabPanelName, "plot_scale_to_world"),
          label = "world",
          width = "150%",
          style='font-size:80%; background-color: white'
        )
      )
    ),
    do.call(
      sliderInput,
      list(
        inputId = paste0(tabPanelName, "binwidth"),
        label = "bin width",
        min = step_origin_binwidth,
        max = max_binwidth,
        step = step_origin_binwidth,
        value = loonWidgets_info$binwidth
      )
    ),
    do.call(
      sliderInput,
      list(
        inputId = paste0(tabPanelName, "origin"),
        label = "origin",
        min = min_origin,
        max = max_origin,
        step = step_origin_binwidth,
        value = loonWidgets_info$origin
      )
    )
  )
}