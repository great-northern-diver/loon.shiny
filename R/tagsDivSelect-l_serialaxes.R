tagsDivSelect.l_serialaxes <- function(loon_grob, tabPanelName, 
                                       loonWidgets_info) {
  
  hexColor <- unique(loonWidgets_info$color)
  colorNames <- hex2colorName(hexColor)
  
  tags$div(
    id = paste0(tabPanelName, 'Select'),  
    class="collapse",
    fixedRow(
      column(
        2,
        h6("select:")
      ),
      column(
        3,
        actionButton(
          paste0(tabPanelName, "select_static_all"),
          label = "all ",
          width = '130%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        3,
        actionButton(
          paste0(tabPanelName, "select_static_none"),
          label = "none",
          width = '130%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        3,
        actionButton(
          paste0(tabPanelName, "select_static_invert"),
          label = "invert",
          width = '130%',
          style='font-size:80%; background-color: white'
        )
      )
    ),
    fixedRow(
      column(
        4,
        h6("sticky:")
      ),
      column(
        8,
        do.call(
          radioButtons,
          list(paste0(tabPanelName, "sticky"),
               label = NULL,
               choices = c("on", "off"),
               selected = "off",
               inline = TRUE)
        )
      )
    ),
    fixedRow(
      column(
        4,
        h6("by color:")
      ),
      column(
        8,
        do.call(
          checkboxGroupInput,
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
      )
    )
  )
}