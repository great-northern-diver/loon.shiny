tagsDivPlot <- function(loon.grob, tabPanelName,
                        loonWidgetsInfo, linkingGroup,
                        displayedPanel) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("tagsDivPlot", obj)
}

tagsDivPlot.l_plot <- function(loon.grob, tabPanelName,
                               loonWidgetsInfo, linkingGroup,
                               displayedPanel) {

  xlim <- loonWidgetsInfo$xlim
  ylim <- loonWidgetsInfo$ylim

  stepX <- loonWidgetsInfo$stepX
  stepY <- loonWidgetsInfo$stepY

  worldViewXlim <- loonWidgetsInfo$worldViewXlim
  worldViewYlim <- loonWidgetsInfo$worldViewYlim

  do.call(
    tags$div,
    remove_null(
      list(
        id = paste0(tabPanelName, 'Plot'),
        class = if(any(grepl("plot", displayedPanel, ignore.case = TRUE))) {NULL} else {"collapse"},
        h6(""),
        do.call(
          sliderInput,
          list(
            inputId = paste0(tabPanelName, "xlim"),
            label = "xlim",
            min = worldViewXlim[1] - diff(worldViewXlim),
            max = worldViewXlim[2] + diff(worldViewXlim),
            step = stepX,
            value = xlim,
            round = -2
          )
        ),
        do.call(
          sliderInput,
          list(
            inputId = paste0(tabPanelName, "ylim"),
            label = "ylim",
            min = worldViewYlim[1] - diff(worldViewYlim),
            max = worldViewYlim[2] + diff(worldViewYlim),
            step = stepY,
            value = ylim,
            round = -2
          )
        ),
        fixedRow(
          column(
            2,
            h6("axes:")
          ),
          column(
            9,
            verticalLayout(
              do.call(
                checkboxGroupInput,
                list(paste0(tabPanelName, "plotAxes1"),
                     label = NULL,
                     choices = c("swap", "labels"),
                     selected = c(if(loonWidgetsInfo$swapInShiny) "swap",
                                  if(loonWidgetsInfo$showLabels) "labels"),
                     inline = TRUE)

              ),
              do.call(
                checkboxGroupInput,
                list(paste0(tabPanelName, "plotAxes2"),
                     label = NULL,
                     choices = c("scales", "guides"),
                     selected = c(if(loonWidgetsInfo$showScales) "scales",
                                  if(loonWidgetsInfo$showGuides) "guides"),
                     inline = TRUE)
              )
            )
          )
        ),
        fixedRow(
          column(
            2,
            h6("glyph:")
          ),
          column(
            4,
            do.call(
              checkboxGroupInput,
              list(paste0(tabPanelName, "itemLabels"),
                   label = NULL,
                   choices = c("itemLabels"),
                   selected = if(loonWidgetsInfo$showItemLabels) "itemLabels",
                   inline = TRUE)

            )
          )
        ),
        fixedRow(
          column(
            2,
            h6("scale:")
          ),
          column(
            3,
            actionButton(
              inputId = paste0(tabPanelName, "scaleToSelect"),
              label = "selected",
              width = "150%",
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            3,
            actionButton(
              inputId = paste0(tabPanelName, "scaleToPlot"),
              label = "plot",
              width = "150%",
              style='font-size:80%; background-color: white'
            )
          ),
          column(
            3,
            actionButton(
              inputId = paste0(tabPanelName, "scaleToWorld"),
              label = "world",
              width = "150%",
              style='font-size:80%; background-color: white'
            )
          )
        )
      ),
      as_list = FALSE)
  )
}
