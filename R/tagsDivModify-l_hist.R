tagsDivModify.l_hist <- function(loon.grob, tabPanelName, colorList,
                                 loonWidgetsInfo, displayedPanel) {

  do.call(
    tags$div,
    remove_null(
      list(
        id = paste0(tabPanelName, 'Modify'),
        class=if(any(grepl("modify", displayedPanel, ignore.case = TRUE))) {NULL} else {"collapse"},
        h6(""),
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
                helpText("No color button")
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
            h6("activate:")
          ),
          column(
            3,
            actionButton(
              paste0(tabPanelName, "modifyDeactive"),
              label = "deactivate",
              width = "150%",
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            3,
            actionButton(
              paste0(tabPanelName, "modifyReactive"),
              label = "reactivate",
              width = "150%",
              style='font-size:80%; background-color: white'
            )
          )
        ),
        h6("")
      ),
      as_list = FALSE)
  )
}
