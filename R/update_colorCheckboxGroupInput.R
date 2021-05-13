update_colorCheckboxGroupInput <- function(session, linkingGroup, linkingGroups, tabPanelName, tabPanelNames, select_by_color, input, 
                                           loonGrob_color, buttons, colorList) {
  
  input$plot_click
  input$plot_brush
  
  if(linkingGroup != "none") {
    
    which_is_linked <- which(linkingGroups %in% linkingGroup)
    
    # check if all linked states share the same `select_by_color`
    # avoiding unterminated loop

    check <- function(select_by_color, input, tabPanelNames, which_is_linked) {

      linked_tabPanels <- tabPanelNames[which_is_linked]
      if(is.null(select_by_color)) select_by_color <- "none selection"

      match_selection <- sapply(linked_tabPanels,
                                function(linked_tabPanel) {
                                  linked_tabPanel_select_by_color <- input[[paste0(linked_tabPanel, "select_by_color")]]
                                  if(is.null(linked_tabPanel_select_by_color)) {
                                    "none selection"
                                  } else linked_tabPanel_select_by_color

                                  if(length(linked_tabPanel_select_by_color) != length(select_by_color)) {
                                    FALSE
                                  } else {
                                    all(linked_tabPanel_select_by_color %in% select_by_color)
                                  }
                                }
      )

      all(match_selection)
    }

    is_shareSameStates <- check(select_by_color, input, tabPanelNames, which_is_linked)

    if(!is_shareSameStates) {
      
      hexColor <- unique(c(loonGrob_color, select_by_color))
      colorNames <- hex2colorName(hexColor)

      lapply(which_is_linked,
             function(i){
               shiny::updateCheckboxGroupInput(
                 session,
                 inputId = paste0(tabPanelNames[i], "select_by_color"),
                 choiceNames = lapply(seq(length(colorNames)), 
                                      function(i) {
                                        shiny::strong(tags$span(colorNames[i], 
                                                                style = paste0("color: ", hexColor[i], ";")))
                                      }),
                 choiceValues = unique(c(loonGrob_color, select_by_color)),
                 selected = if(is.null(select_by_color) | !is.null(input$plot_click) | !is.null(input$plot_brush)) character(0) else select_by_color
               )
             }
      )
    }
    
    if(buttons$color_button$modify != 0) {

      hexColor <- unique(c(loonGrob_color, select_by_color, input[[paste0(tabPanelName, "modify_color")]]))
      colorNames <- hex2colorName(hexColor)

      lapply(which_is_linked,
             function(i){
               shiny::updateCheckboxGroupInput(
                 session,
                 inputId = paste0(tabPanelNames[i], "select_by_color"),
                 choiceNames = lapply(seq(length(colorNames)), 
                                      function(i) {
                                        shiny::strong(tags$span(colorNames[i], 
                                                                style = paste0("color: ", hexColor[i], ";")))
                                      }),
                 choiceValues = unique(c(loonGrob_color, select_by_color, input[[paste0(tabPanelName, "modify_color")]])),
                 selected = if(is.null(select_by_color) | !is.null(input$plot_click) | !is.null(input$plot_brush)) character(0) else select_by_color
               )
             }
      )
    } else NULL

    for(color in colorList) {
      
      if(buttons$color_button[[color]] != 0) {
        
        hexColor <- unique(c(loonGrob_color, color))
        colorNames <- hex2colorName(hexColor)
        
        shiny::updateCheckboxGroupInput(
          session,
          inputId = paste0(tabPanelName, "select_by_color"),
          choiceNames = lapply(seq(length(colorNames)), 
                               function(i) {
                                 shiny::strong(tags$span(colorNames[i], 
                                                         style = paste0("color: ", hexColor[i], ";")))
                               }),
          choiceValues = unique(c(loonGrob_color, color))
        )
      } else NULL
    }
  } else {
    
    if(buttons$color_button$modify != 0) {
      
      hexColor <- unique(c(loonGrob_color, input[[paste0(tabPanelName, "modify_color")]]))
      colorNames <- hex2colorName(hexColor)
      
      shiny::updateCheckboxGroupInput(
        session,
        inputId = paste0(tabPanelName, "select_by_color"),
        choiceNames = lapply(seq(length(colorNames)), 
                             function(i) {
                               shiny::strong(tags$span(colorNames[i], 
                                                       style = paste0("color: ", hexColor[i], ";")))
                             }),
        choiceValues = unique(c(loonGrob_color, input[[paste0(tabPanelName, "modify_color")]]))
      )
    } else NULL
    
    for(color in colorList) {
      
      if(buttons$color_button[[color]] != 0) {
        
        hexColor <- unique(c(loonGrob_color, color))
        colorNames <- hex2colorName(hexColor)
        
        shiny::updateCheckboxGroupInput(
          session,
          inputId = paste0(tabPanelName, "select_by_color"),
          choiceNames = lapply(seq(length(colorNames)), 
                               function(i) {
                                 shiny::strong(tags$span(colorNames[i], 
                                                         style = paste0("color: ", hexColor[i], ";")))
                               }),
          choiceValues = unique(c(loonGrob_color, color))
        )
      } else NULL
    }
  }
  
  if(!is.null(input$plot_click) || !is.null(input$plot_brush)) {
    shiny::updateCheckboxGroupInput(
      session,
      inputId = paste0(tabPanelName, "select_by_color"),
      selected = character(0)
    )
  }
}