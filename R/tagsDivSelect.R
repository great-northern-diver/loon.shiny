tagsDivSelect <- function(loon.grob, tabPanelName,
                          loonWidgetsInfo, displayedPanel) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("tagsDivSelect", obj)
}

tagsDivSelect.default <- function(loon.grob, tabPanelName,
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
        shiny::fixedRow(
          shiny::column(
            2,
            shiny::h6("static:")
          ),
          shiny::column(
            3,
            shiny::actionButton(
              paste0(tabPanelName, "selectStaticAll"),
              label = "all ",
              width = "130%",
              style='font-size:80%; background-color: white'
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              paste0(tabPanelName, "selectStaticNone"),
              label = "none",
              width = "130%",
              style='font-size:80%; background-color: white'
            )
          ),
          shiny::column(
            3,
            shiny::actionButton(
              paste0(tabPanelName, "selectStaticInvert"),
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
              list(paste0(tabPanelName, "selectDynamic"),
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
            if(all(!is.na(unique(loonWidgetsInfo$color)))) {
              do.call(
                shiny::checkboxGroupInput,
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
            } else {
              do.call(
                shiny::helpText,
                list(inputId = paste0(tabPanelName, "selectByColor"),
                     "NULL selected color"
                )
              )
            }
          )
        )
      ),
      as_list = FALSE)
  )
}
