tagsDivModify.l_serialaxes <- function(loon.grob, tabPanelName, colorList,
                                       loonWidgetsInfo, displayedPanel) {

  do.call(
    tags$div,
    remove_null(
      list(
        id = paste0(tabPanelName, 'Modify'),
        class=if(any(grepl("modify", displayedPanel, ignore.case = TRUE))) {NULL} else {"collapse"},
        h6(""),
        fixedRow(
          column(
            3,
            h6("activate:")
          ),
          column(
            3,
            actionButton(
              paste0(tabPanelName, "modifyDeactive"),
              label = "deactivate",
              width = '130%',
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            3,
            actionButton(
              paste0(tabPanelName, "modifyReactive"),
              label = "reactivate",
              width = '130%',
              style='font-size:80%; background-color: white'
            )
          )
        ),
        h6("color:"),
        do.call(
          fixedRow,
          if(length(colorList) >= 1) {
            lapply(colorList,
                   function(col) {
                     column(
                       1,
                       actionButton(paste0(tabPanelName, col),
                                    label = "",
                                    style= paste(c(paste0('background-color: ', col), "height:25px"), collapse = "; ")
                       )
                     )
                   }
            )
          } else {
            list(
              column(
                8,
                helpText("No color button"),
                offset = 2
              )
            )
          }
        ),
        h6(""),
        fixedRow(
          column(
            2,
            h6("customise:")
          ),
          column(
            5,
            colourpicker::colourInput(
              paste0(tabPanelName, "colorPicker"),
              label = NULL,
              value = "#00BBDD",
              showColour = "background"
            ),
            offset = 1
          ),
          column(
            3,
            actionButton(paste0(tabPanelName, "colorApply"),
                         label = "apply",
                         width = "150%",
                         style='font-size:80%; background-color: white')
          )
        ),
        fixedRow(
          column(
            2,
            h6("linewidth:")
          ),
          column(
            4,
            h6("common")
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "absToPlus"),
              icon("plus"),
              width = "150%",
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "absToMinus"),
              icon("minus"),
              width = "150%",
              style='font-size:80%; background-color: white'
            )
          )
        ),
        fixedRow(
          column(
            4,
            h6("relative"),
            offset = 2
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "relToPlus"),
              icon("plus"),
              width = "150%",
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "relToMinus"),
              icon("minus"),
              width = "150%",
              style='font-size:80%; background-color: white'
            )
          )
        ),
        h6(""),
        fixedRow(
          column(
            2,
            h6("alpha:")
          ),
          column(
            7,
            do.call(
              sliderInput,
              list(
                inputId = paste0(tabPanelName, "alpha"),
                label = NULL,
                min = 0,
                max = 1,
                step = 0.1,
                value = 1
              )
            )
          ),
          column(
            2,
            actionButton(paste0(tabPanelName, "alphaApply"),
                         label = "apply",
                         width = "200%",
                         style='font-size:80%; background-color: white')
          )
        ),
        h6("")
      ),
      as_list = FALSE)
  )
}
