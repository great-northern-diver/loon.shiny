tagsDivLink <- function(loon.grob, tabPanelName,
                        loonWidgetsInfo, linkingGroup,
                        linkingGroups, displayedPanel) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("tagsDivLink", obj)
}

tagsDivLink.l_plot <- function(loon.grob, tabPanelName, loonWidgetsInfo,
                               linkingGroup, linkingGroups, displayedPanel) {

  do.call(
    tags$div,
    remove_null(
      list(
        id = paste0(tabPanelName, 'Linking'),
        class=if(any(grepl("linking", displayedPanel, ignore.case = TRUE))) NULL else "collapse",
        h6(""),
        fixedRow(
          column(
            3,
            h6("linkingGroup:")
          ),
          column(
            4,
            selectInput(
              inputId = paste0(tabPanelName, "linkingGroup"),
              label = NULL,
              choices = unique(c("none", linkingGroups)),
              selected = linkingGroup
            )
          ),
          column(
            2,
            actionButton(paste0(tabPanelName, "pull"),
                         label = "pull",
                         width = "150%",
                         style='font-size:60%; background-color: white')
          ),
          column(
            2,
            actionButton(paste0(tabPanelName, "push"),
                         label = "push",
                         width = "150%",
                         style='font-size:60%; background-color: white')
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
                               selected = loonWidgetsInfo$linkingStates
            )
          )
        )
      ), as_list = FALSE
    )
  )
}

tagsDivLink.l_hist <- function(loon.grob, tabPanelName, loonWidgetsInfo,
                               linkingGroup, linkingGroups,
                               displayedPanel) {


  do.call(
    tags$div,
    remove_null(
      list(
        id = paste0(tabPanelName, 'Linking'),
        class = if(any(grepl("linking", displayedPanel, ignore.case = TRUE))) NULL else "collapse",
        h6(""),
        fixedRow(
          column(
            3,
            h6("linkingGroup:")
          ),
          column(
            4,
            selectInput(
              inputId = paste0(tabPanelName, "linkingGroup"),
              label = NULL,
              choices = unique(c("none", linkingGroups)),
              selected = linkingGroup
            )
          ),
          column(
            2,
            actionButton(paste0(tabPanelName, "pull"),
                         label = "pull",
                         width = "150%",
                         style='font-size:60%; background-color: white')
          ),
          column(
            2,
            actionButton(paste0(tabPanelName, "push"),
                         label = "push",
                         width = "150%",
                         style='font-size:60%; background-color: white')
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
                               selected = loonWidgetsInfo$linkingStates
            )
          )
        )
      ), as_list = FALSE
    )
  )
}

tagsDivLink.l_serialaxes <- function(loon.grob, tabPanelName, loonWidgetsInfo,
                                     linkingGroup, linkingGroups, displayedPanel) {


  do.call(
    tags$div,
    remove_null(
      list(
        id = paste0(tabPanelName, 'Linking'),
        class = if(any(grepl("linking", displayedPanel, ignore.case = TRUE))) NULL else "collapse",
        h6(""),
        fixedRow(
          column(
            3,
            h6("linkingGroup:")
          ),
          column(
            4,
            selectInput(
              inputId = paste0(tabPanelName, "linkingGroup"),
              label = NULL,
              choices = unique(c("none", linkingGroups)),
              selected = linkingGroup
            )
          ),
          column(
            2,
            actionButton(paste0(tabPanelName, "pull"),
                         label = "pull",
                         width = "150%",
                         style='font-size:60%; background-color: white')
          ),
          column(
            2,
            actionButton(paste0(tabPanelName, "push"),
                         label = "push",
                         width = "150%",
                         style='font-size:60%; background-color: white')
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
                               selected = loonWidgetsInfo$linkingStates
            )
          )
        )
      ), as_list = FALSE
    )
  )
}

tagsDivLink.l_graph<- function(loon.grob, tabPanelName,
                               loonWidgetsInfo, linkingGroup, linkingGroups, displayedPanel) {
  tagsDivLink.l_plot(loon.grob, tabPanelName,
                     loonWidgetsInfo, linkingGroup, linkingGroups, displayedPanel)
}
