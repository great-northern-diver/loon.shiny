tagsDivModify.l_graph <- function(loon_grob, tabPanelName, 
                                 loonWidgets_info) {
  
  path <- file.path(find.package(package = 'loon.shiny'), "images")
  
  colorList <- loonWidgets_info$colorList
  tags$div(
    id = paste0(tabPanelName, 'Modify'),  
    class="collapse",
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
    fixedRow(
      column(
        6,
        colourpicker::colourInput(
          paste0(tabPanelName, "modify_color"),
          label = NULL,
          value = "#00BBDD",
          showColour = "background"
        )
      ),
      column(
        3,
        actionButton(paste0(tabPanelName, "color"),
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
          paste0(tabPanelName, "modify_deactive"),
          label = "deactivate",
          width = "150%",
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        3,
        actionButton(
          paste0(tabPanelName, "modify_reactive"),
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
          paste0(tabPanelName, "modify_move_halign"),
          label = tags$img(src = base64enc::dataURI(file = paste0(path, "/align_h.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "modify_move_valign"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/align_v.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "modify_move_hdist"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/distribute_h.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "modify_move_vdist"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/distribute_v.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
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
          paste0(tabPanelName, "modify_move_grid"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/distribute_grid.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        ),
        offset = 2
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "modify_move_jitter"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/jitter.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "modify_move_reset"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/reset.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
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
          paste0(tabPanelName, "modify_glyph_circle"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/circle.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "modify_glyph_square"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/square.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "modify_glyph_triangle"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/triangle.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "modify_glyph_ocircle"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/ocircle.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
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
          paste0(tabPanelName, "modify_glyph_osquare"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/osquare.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        ),
        offset = 2
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "modify_glyph_otriangle"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/otriangle.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "modify_glyph_ccircle"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/ccircle.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "modify_glyph_csquare"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/csquare.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
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
          paste0(tabPanelName, "modify_glyph_ctriangle"),
          label = tags$img(src = base64enc::dataURI(file=paste0(path, "/ctriangle.png"), mime="image/png"),
                           height = "20px",
                           weight = "20px"
          ),
          width = '150%',
          style='font-size:80%; background-color: white'
        ),
        offset = 2
      ),
      column(
        3,
        checkboxInput(
          inputId = paste0(tabPanelName, "show_nodes_label"),
          label =  tags$img(src = base64enc::dataURI(file=paste0(path, "/orbit.png"), mime="image/png"),
                            height = "20px",
                            weight = "20px"
          ),
          value = loonWidgets_info$showOrbit,
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
        h6("abs to:")
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "abs_to_plus"),
          icon("plus"),
          width = "150%",
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "abs_to_minus"),
          icon("minus"),
          width = "150%",
          style='font-size:80%; background-color: white'
        )
      )
    ),
    fixedRow(
      column(
        4,
        h6("rel to:"),
        offset = 2
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "rel_to_plus"),
          icon("plus"),
          width = "150%",
          style='font-size:80%; background-color: white'
        )
      ),
      column(
        2,
        actionButton(
          paste0(tabPanelName, "rel_to_minus"),
          icon("minus"),
          width = "150%",
          style='font-size:80%; background-color: white'
        )
      )
    ),
    h6("")
  )
}