tagsDivGlyph <- function(loon_grob, tabPanelName, 
                          loonWidgets_info) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("tagsDivGlyph", obj)
}

tagsDivGlyph.default <- function(loon_grob, tabPanelName, 
                                 loonWidgets_info) {
  
  path <- file.path(find.package(package = 'loon.shiny'), "images")
  glyph_name <- loonWidgets_info$glyph_name
  
  tags$div(
    id = paste0(tabPanelName, 'Glyph'),  
    class="collapse",
    h6(""),
    fixedRow(
      column(
        4,
        h6("glyph set:")
      ),
      column(
        8,
        if(all(!is.na(glyph_name))) {
          if(any(str_detect(glyph_name, "serialaxes"))) {
            do.call(
              checkboxGroupInput,
              list(
                paste0(tabPanelName, "glyph_setting"),
                label = "Serialaxes",
                choices = c("showEnclosing", "showAxes", "showArea"),
                selected = c(
                  if(loonWidgets_info$glyph_setting$showEnclosing) "showEnclosing",
                  if(loonWidgets_info$glyph_setting$showAxes) "showAxes",
                  if(loonWidgets_info$glyph_setting$showArea) "showArea"
                )
              )
            )
          } else if(any(str_detect(glyph_name, "pointrange"))) {
            do.call(
              checkboxGroupInput,
              list(paste0(tabPanelName, "glyph_setting"),
                   label = "Pointrange",
                   choices = "showArea",
                   selected = if(loonWidgets_info$glyph_setting$showArea) "showArea")
            )
          } else {
            do.call(
              helpText,
              list(inputId = paste0(tabPanelName, "glyph_setting"),
                   "None glyph setting"
              )
            )
          }
        } else {
          do.call(
            helpText,
            list(inputId = paste0(tabPanelName, "glyph_setting"),
                 "None glyph setting"
            )
          )
        }
      )
    )
  )
}