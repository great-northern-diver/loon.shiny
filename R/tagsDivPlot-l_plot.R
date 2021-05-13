tagsDivPlot <- function(loon_grob, tabPanelName, 
                        loonWidgets_info, linkingGroup, linkingGroups) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("tagsDivPlot", obj)
}

tagsDivPlot.l_plot <- function(loon_grob, tabPanelName, 
                               loonWidgets_info, linkingGroup, linkingGroups) {
  
  
  xlim <- loonWidgets_info$xlim
  ylim <- loonWidgets_info$ylim

  step_x <- loonWidgets_info$step_x
  step_y <- loonWidgets_info$step_y
  
  worldView_xlim <- loonWidgets_info$worldView_xlim
  worldView_ylim <- loonWidgets_info$worldView_ylim
  
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
        2,
        h6("axes:")
      ),
      column(
        9,
        verticalLayout(
          do.call(
            checkboxGroupInput,
            list(paste0(tabPanelName, "plot_axes1"),
                 label = NULL,
                 choices = c("swap", "labels"),
                 selected = c(if(loonWidgets_info$swap_in_shiny) "swap",
                              if(loonWidgets_info$showLabels) "labels"),
                 inline = TRUE)
            
          ),
          do.call(
            checkboxGroupInput,
            list(paste0(tabPanelName, "plot_axes2"),
                 label = NULL,
                 choices = c("scales", "guides"),
                 selected = c(if(loonWidgets_info$showScales) "scales",
                              if(loonWidgets_info$showGuides) "guides"),
                 inline = TRUE)
          )
        )
      )
    ),
    fixedRow(
      column(
        2,
        h6("scale to:")
      ),
      column(
        3,
        actionButton(
          inputId = paste0(tabPanelName, "plot_scale_to_select"),
          label = "selected",
          width = "150%",
          style='font-size:80%; background-color: white'
        )
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
    )
  )
}