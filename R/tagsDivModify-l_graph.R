tagsDivModify.l_graph <- function(loon.grob, tabPanelName, colorList,
                                  loonWidgetsInfo, displayedPanel) {

  path <- file.path(find.package(package = 'loon.shiny'), "images")

  # It is mainly for the test
  # In `devtools::test`, it is looking for the Github source
  # not the library source. Hence, the path is different
  tryCatch(
    suppressWarnings(
      base64enc::dataURI(file=paste0(path, "/LoonIcon.png"), mime="image/png")
    ),
    error = function(e) {
      path <<- file.path(find.package(package = 'loon.shiny'), "inst/images")
    }
  )

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
          } else list(
            column(
              8,
              helpText("No color button")
            )
          )
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
            2,
            actionButton(paste0(tabPanelName, "colorApply"),
                         label = "apply",
                         width = "200%",
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
        fixedRow(
          column(
            2,
            h6("move:")
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyMoveHalign"),
              label = tags$img(src = base64enc::dataURI(file = paste0(path, "/align_h.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyMoveValign"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/align_v.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyMoveHdist"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/distribute_h.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyMoveVdist"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/distribute_v.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            )
          )
        ),
        fixedRow(
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyMoveGrid"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/distribute_grid.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            ),
            offset = 2
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyMoveJitter"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/jitter.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyMoveReset"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/reset.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            )
          )
        ),
        fixedRow(
          column(
            2,
            h6("glyph:")
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyGlyphCircle"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/circle.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyGlyphSquare"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/square.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyGlyphTriangle"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/triangle.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            )
          )
        ),
        fixedRow(
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyGlyphOcircle"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/ocircle.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            ),
            offset = 2
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyGlyphOsquare"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/osquare.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyGlyphOtriangle"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/otriangle.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            )
          )
        ),
        fixedRow(
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyGlyphCcircle"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/ccircle.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            ),
            offset = 2
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyGlyphCsquare"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/csquare.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            2,
            actionButton(
              paste0(tabPanelName, "modifyGlyphCtriangle"),
              label = tags$img(src = base64enc::dataURI(file=paste0(path, "/ctriangle.png"), mime="image/png"),
                               height = "10px",
                               weight = "10px"
              ),
              width = '150%',
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            3,
            checkboxInput(
              inputId = paste0(tabPanelName, "show_nodes_label"),
              label =  tags$img(src = base64enc::dataURI(file=paste0(path, "/orbit.png"), mime="image/png"),
                                height = "10px",
                                weight = "10px"
              ),
              value = loonWidgetsInfo$showOrbit,
              width = '300%'
            )
          )
        ),
        fixedRow(
          column(
            2,
            h6("size:")
          ),
          column(
            4,
            h6("common:")
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
            h6("relative:"),
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
