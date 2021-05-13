tagsDivLink <- function(loon_grob, tabPanelName, 
                        loonWidgets_info, linkingGroup, linkingGroups) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("tagsDivLink", obj)
}

tagsDivLink.l_plot <- function(loon_grob, tabPanelName, loonWidgets_info, 
                               linkingGroup, linkingGroups) {
  
  tags$div(
    id = paste0(tabPanelName, 'Linking'),  
    class="collapse",
    fixedRow(
      column(
        4,
        h6("linkingGroup:")
      ),
      column(
        8,
        selectInput(
          inputId = paste0(tabPanelName, "linkingGroup"),
          label = NULL,
          choices = unique(c("none", linkingGroups)),
          selected = linkingGroup
        )
      )
    ),
    fixedRow(
      column(
        4,
        h6("linkingStates:")
      ),
      column(
        8,
        checkboxGroupInput(inputId = paste0(tabPanelName, "linkedStates"),
                           label = NULL,
                           choices = c("active", "selected", "color", "size", "glyph"),
                           selected = loonWidgets_info$linkingStates
        )
      )
    )
  )
}

tagsDivLink.l_hist <- function(loon_grob, tabPanelName, loonWidgets_info, 
                               linkingGroup, linkingGroups) {
  
  tags$div(
    id = paste0(tabPanelName, 'Linking'),  
    class="collapse",
    fixedRow(
      column(
        4,
        h6("linkingGroup:")
      ),
      column(
        8,
        selectInput(
          inputId = paste0(tabPanelName, "linkingGroup"),
          label = NULL,
          choices = unique(c("none", linkingGroups)),
          selected = linkingGroup
        )
      )
    ),
    fixedRow(
      column(
        4,
        h6("linkingStates:")
      ),
      column(
        8,
        checkboxGroupInput(inputId = paste0(tabPanelName, "linkedStates"),
                           label = NULL,
                           choices = c("active", "selected", "color"),
                           selected = loonWidgets_info$linkingStates
        )
      )
    )
  )
}

tagsDivLink.l_serialaxes <- function(loon_grob, tabPanelName, loonWidgets_info, 
                                     linkingGroup, linkingGroups) {
  
  tags$div(
    id = paste0(tabPanelName, 'Linking'),  
    class="collapse",
    h6(""),
    fixedRow(
      column(
        4,
        h6("linkingGroup:")
      ),
      column(
        8,
        selectInput(
          inputId = paste0(tabPanelName, "linkingGroup"),
          label = NULL,
          choices = unique(c("none", linkingGroups)),
          selected = linkingGroup
        )
      )
    ),
    fixedRow(
      column(
        4,
        h6("linkingStates:")
      ),
      column(
        8,
        checkboxGroupInput(inputId = paste0(tabPanelName, "linkedStates"),
                           label = NULL,
                           choices = c("active", "selected", "color", "size"),
                           selected = loonWidgets_info$linkingStates
        )
      )
    )
  )
}

tagsDivLink.l_graph<- function(loon_grob, tabPanelName, 
                               loonWidgets_info, linkingGroup, linkingGroups) { 
  tagsDivLink.l_plot(loon_grob, tabPanelName, 
                     loonWidgets_info, linkingGroup, linkingGroups)
}