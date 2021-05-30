tagsDivGlyph <- function(loon.grob, tabPanelName,
                         loonWidgetsInfo, displayedPanel) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("tagsDivGlyph", obj)
}

tagsDivGlyph.default <- function(loon.grob, tabPanelName,
                                 loonWidgetsInfo, displayedPanel) {

  glyphNames <- loonWidgetsInfo$glyphNames

  do.call(
    tags$div,
    remove_null(
      list(
        id = paste0(tabPanelName, 'Glyph'),
        class = if(any(grepl(pattern = "glyph", displayedPanel, ignore.case = TRUE))) {NULL} else {"collapse"},
        h6(""),
        fixedRow(
          column(
            4,
            h6("glyph set:")
          ),
          column(
            8,
            if(all(!is.na(glyphNames))) {
              if(any(grepl(glyphNames, pattern = "serialaxes"))) {
                do.call(
                  checkboxGroupInput,
                  list(
                    paste0(tabPanelName, "nonePrimitiveGlyphSettings"),
                    label = "Serialaxes",
                    choices = c("showEnclosing", "showAxes", "showArea"),
                    selected = c(
                      if(loonWidgetsInfo$nonePrimitiveGlyphSettings$showEnclosing) "showEnclosing",
                      if(loonWidgetsInfo$nonePrimitiveGlyphSettings$showAxes) "showAxes",
                      if(loonWidgetsInfo$nonePrimitiveGlyphSettings$showArea) "showArea"
                    )
                  )
                )
              } else if(any(grepl(glyphNames, pattern = "pointrange"))) {
                do.call(
                  checkboxGroupInput,
                  list(paste0(tabPanelName, "nonePrimitiveGlyphSettings"),
                       label = "Pointrange",
                       choices = "showArea",
                       selected = if(loonWidgetsInfo$nonePrimitiveGlyphSettings$showArea) "showArea")
                )
              } else if(any(grepl(glyphNames, pattern = "polygon"))) {

                do.call(
                  checkboxGroupInput,
                  list(paste0(tabPanelName, "nonePrimitiveGlyphSettings"),
                       label = "Polygon",
                       choices = "showArea",
                       selected = if(loonWidgetsInfo$nonePrimitiveGlyphSettings$showArea) "showArea")
                )

              } else {
                do.call(
                  helpText,
                  list(inputId = paste0(tabPanelName, "nonePrimitiveGlyphSettings"),
                       "None glyph setting"
                  )
                )
              }
            } else {
              do.call(
                helpText,
                list(inputId = paste0(tabPanelName, "nonePrimitiveGlyphSettings"),
                     "None glyph setting"
                )
              )
            }
          )
        )
      ), as_list = FALSE
    )
  )

}
