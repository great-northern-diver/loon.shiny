tagsDivSelect <- function(loon_grob, tabPanelName, 
                          loonWidgets_info) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("tagsDivSelect", obj)
}

tagsDivSelect.default <- function(loon_grob, tabPanelName, 
                                  loonWidgets_info) {
  
  hexColor <- unique(loonWidgets_info$color)
  colorNames <- hex2colorName(hexColor)
  
  shiny::tags$div(
    id = paste0(tabPanelName, 'Select'),  
    class="collapse",
    h6(""),
    shiny::fixedRow(
      shiny::column(
        2,
        shiny::h6("static:")
      ),
      shiny::column(
        3,
        shiny::actionButton(
          paste0(tabPanelName, "select_static_all"),
          label = "all ",
          width = "130%",
          style='font-size:80%; background-color: white'
        )
      ),
      shiny::column(
        3,
        shiny::actionButton(
          paste0(tabPanelName, "select_static_none"),
          label = "none",
          width = "130%",
          style='font-size:80%; background-color: white'
        )
      ),
      shiny::column(
        3,
        shiny::actionButton(
          paste0(tabPanelName, "select_static_invert"),
          label = "invert",
          width = "130%",
          style='font-size:80%; background-color: white'
        )
      )
    ),
    shiny::fixedRow(
      shiny::column(
        4,
        shiny::h6("dynamic:")
      ),
      shiny::column(
        8,
        do.call(
          shiny::radioButtons,
          list(paste0(tabPanelName, "select_dynamic"),
               label = NULL,
               choices = c("select", "deselect", "invert"),
               selected = "select")
        )
      )
    ),
    shiny::fixedRow(
      shiny::column(
        4,
        shiny::h6("sticky:")
      ),
      shiny::column(
        8,
        do.call(
          shiny::radioButtons,
          list(paste0(tabPanelName, "sticky"),
               label = NULL,
               choices = c("on", "off"),
               selected = "off",
               inline = TRUE)
        )
      )
    ),
    shiny::fixedRow(
      shiny::column(
        4,
        shiny::h6("by color:")
      ),
      shiny::column(
        8,
        if(all(!is.na(unique(loonWidgets_info$color)))) {
          do.call(
            shiny::checkboxGroupInput,
            list(paste0(tabPanelName, "select_by_color"),
                 label = NULL,
                 choiceNames = lapply(seq(length(colorNames)), 
                                      function(i) {
                                        shiny::strong(tags$span(colorNames[i], 
                                                                style = paste0("color: ", hexColor[i], ";")))
                                      }),
                 choiceValues = unique(loonWidgets_info$color),
                 selected = NULL)
          )
        } else {
          do.call(
            shiny::helpText,
            list(inputId = paste0(tabPanelName, "select_by_color"),
                 "NULL selected color"
            )
          )
        }
      )
    )
  )
}