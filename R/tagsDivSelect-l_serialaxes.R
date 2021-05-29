tagsDivSelect.l_serialaxes <- function(loon.grob, tabPanelName,
                                       loonWidgetsInfo, displayedPanel) {

  hexColor <- unique(loonWidgetsInfo$color)
  colorNames <- l_colorName(hexColor, error = FALSE)

  do.call(
    tags$div,
    remove_null(
      list(
        id = paste0(tabPanelName, 'Select'),
        class=if(any(grepl("select", displayedPanel, ignore.case = TRUE))) {NULL} else {"collapse"},
        h6(""),
        fixedRow(
          column(
            2,
            h6("select:")
          ),
          column(
            3,
            actionButton(
              paste0(tabPanelName, "selectStaticAll"),
              label = "all ",
              width = '130%',
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            3,
            actionButton(
              paste0(tabPanelName, "selectStaticNone"),
              label = "none",
              width = '130%',
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            3,
            actionButton(
              paste0(tabPanelName, "selectStaticInvert"),
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
              list(paste0(tabPanelName, "selectByColor"),
                   label = NULL,
                   choiceNames = lapply(seq(length(colorNames)),
                                        function(i) {
                                          shiny::strong(tags$span(colorNames[i],
                                                                  style = paste0("color: ", hexColor[i], ";")))
                                        }),
                   choiceValues = unique(loonWidgetsInfo$color),
                   selected = NULL)
            )
          )
        )
      ),
      as_list = FALSE)
  )
}
