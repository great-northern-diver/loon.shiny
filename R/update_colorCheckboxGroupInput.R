update_colorCheckboxGroupInput <- function(session, linkingGroup, linkingGroups, tabPanelName, tabPanelNames, selectByColor, input,
                                           loonGrob_color, buttons, colorList) {

  plotClick <- input$plotClick
  plotBrush <- input$plotBrush

  colorListButtons <- setNames(
    lapply(colorList, function(col) input[[paste0(tabPanelName, col)]]),
    colorList
  )

  if(linkingGroup != "none") {

    which_is_linked <- which(linkingGroups %in% linkingGroup)

    # check if all linked states share the same `selectByColor`
    # avoiding unterminated loop

    check <- function(selectByColor, input, tabPanelNames, which_is_linked) {

      linked_tabPanels <- tabPanelNames[which_is_linked]
      if(is.null(selectByColor)) selectByColor <- "none selection"

      match_selection <- sapply(linked_tabPanels,
                                function(linked_tabPanel) {
                                  linked_tabPanel_selectByColor <- input[[paste0(linked_tabPanel, "selectByColor")]]
                                  if(is.null(linked_tabPanel_selectByColor)) {
                                    "none selection"
                                  } else linked_tabPanel_selectByColor

                                  if(length(linked_tabPanel_selectByColor) != length(selectByColor)) {
                                    FALSE
                                  } else {
                                    all(linked_tabPanel_selectByColor %in% selectByColor)
                                  }
                                }
      )

      all(match_selection)
    }

    is_shareSameStates <- check(selectByColor, input, tabPanelNames, which_is_linked)

    if(!is_shareSameStates) {

      hexColor <- unique(c(loonGrob_color, selectByColor))
      colorNames <- l_colorName(hexColor, error = FALSE)

      lapply(which_is_linked,
             function(i){
               shiny::updateCheckboxGroupInput(
                 session,
                 inputId = paste0(tabPanelNames[i], "selectByColor"),
                 choiceNames = lapply(seq(length(colorNames)),
                                      function(i) {
                                        shiny::strong(tags$span(colorNames[i],
                                                                style = paste0("color: ", hexColor[i], ";")))
                                      }),
                 choiceValues = unique(c(loonGrob_color, selectByColor)),
                 selected = if(is.null(selectByColor) || !is.null(plotClick) | !is.null(plotBrush)) character(0) else selectByColor
               )
             }
      )
    }

    colorApply <- input[[paste0(tabPanelName, "colorApply")]]
    if(colorApply > buttons["colorApply"]) {

      hexColor <- unique(c(loonGrob_color, selectByColor, input[[paste0(tabPanelName, "colorPicker")]]))
      colorNames <- l_colorName(hexColor, error = FALSE)

      lapply(which_is_linked,
             function(i){
               shiny::updateCheckboxGroupInput(
                 session,
                 inputId = paste0(tabPanelNames[i], "selectByColor"),
                 choiceNames = lapply(seq(length(colorNames)),
                                      function(i) {
                                        shiny::strong(tags$span(colorNames[i],
                                                                style = paste0("color: ", hexColor[i], ";")))
                                      }),
                 choiceValues = unique(c(loonGrob_color, selectByColor, input[[paste0(tabPanelName, "colorPicker")]])),
                 selected = if(is.null(selectByColor) || !is.null(plotClick) || !is.null(plotBrush)) character(0) else selectByColor
               )
             }
      )
    } else NULL

    for(color in colorList) {

      if(colorListButtons[[color]] > buttons[color]) {

        hexColor <- unique(c(loonGrob_color, color))
        colorNames <- l_colorName(hexColor, error = FALSE)

        shiny::updateCheckboxGroupInput(
          session,
          inputId = paste0(tabPanelName, "selectByColor"),
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

    colorApply <- input[[paste0(tabPanelName, "colorApply")]]
    if(colorApply > buttons["colorApply"]) {

      hexColor <- unique(c(loonGrob_color, input[[paste0(tabPanelName, "colorPicker")]]))
      colorNames <- l_colorName(hexColor, error = FALSE)

      shiny::updateCheckboxGroupInput(
        session,
        inputId = paste0(tabPanelName, "selectByColor"),
        choiceNames = lapply(seq(length(colorNames)),
                             function(i) {
                               shiny::strong(tags$span(colorNames[i],
                                                       style = paste0("color: ", hexColor[i], ";")))
                             }),
        choiceValues = unique(c(loonGrob_color, input[[paste0(tabPanelName, "colorPicker")]]))
      )
    } else NULL

    for(color in colorList) {

      if(colorListButtons[[color]] > buttons[color]) {

        hexColor <- unique(c(loonGrob_color, color))
        colorNames <- l_colorName(hexColor, error = FALSE)

        shiny::updateCheckboxGroupInput(
          session,
          inputId = paste0(tabPanelName, "selectByColor"),
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

  if(!is.null(plotClick) || !is.null(plotBrush)) {
    shiny::updateCheckboxGroupInput(
      session,
      inputId = paste0(tabPanelName, "selectByColor"),
      selected = character(0)
    )
  }
}
