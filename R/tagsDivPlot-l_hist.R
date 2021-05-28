tagsDivPlot.l_hist <- function(loon.grob, tabPanelName,
                               loonWidgetsInfo,
                               linkingGroup, displayedPanel) {

  viewPort <- get_viewPort(loon.grob)

  if(loonWidgetsInfo$swapInLoon) {

    xlim <- viewPort[[2]]$yscale
    ylim <- viewPort[[2]]$xscale

  } else {

    xlim <- viewPort[[2]]$xscale
    ylim <- viewPort[[2]]$yscale
  }

  worldViewXlim <- range(c(loonWidgetsInfo$layerXlim, xlim))
  worldViewYlim <- range(c(loonWidgetsInfo$layerYlim, ylim))

  stepX <- log_ceiling(diff(xlim))
  stepY <- log_ceiling(diff(ylim))

  step_origin_binwidth <- stepX
  max_binwidth <- diff(xlim)

  min_origin <- xlim[1] - diff(xlim)
  max_origin <- xlim[2] + diff(xlim)

  do.call(
    tags$div,
    remove_null(
      list(
        id = paste0(tabPanelName, 'Plot'),
        class=if(any(grepl("plot", displayedPanel, ignore.case = TRUE))) {NULL} else {"collapse"},
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
            round = TRUE
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
            round = TRUE
          )
        ),
        do.call(
          sliderInput,
          list(
            inputId = paste0(tabPanelName, "binwidth"),
            label = "bin width",
            min = step_origin_binwidth,
            max = max_binwidth,
            step = step_origin_binwidth,
            value = loonWidgetsInfo$binwidth,
            round = TRUE
          )
        ),
        do.call(
          sliderInput,
          list(
            inputId = paste0(tabPanelName, "origin"),
            label = "origin",
            min = min_origin,
            max = max_origin,
            step = step_origin_binwidth,
            value = loonWidgetsInfo$origin,
            round = TRUE
          )
        ),
        fixedRow(
          column(
            3,
            h6("axes:")
          ),
          column(
            9,
            verticalLayout(
              do.call(
                checkboxGroupInput,
                list(paste0(tabPanelName, "plotAxes1"),
                     label = NULL,
                     choices = c("swap", "scales"),
                     selected = c(if(loonWidgetsInfo$swapInShiny) "swap",
                                  if(loonWidgetsInfo$showScales) "scales"),
                     inline = TRUE)

              ),
              do.call(
                checkboxGroupInput,
                list(paste0(tabPanelName, "plotAxes2"),
                     label = NULL,
                     choices = c("guides", "labels"),
                     selected = c(if(loonWidgetsInfo$showGuides) "guides",
                                  if(loonWidgetsInfo$showLabels) "labels"),
                     inline = TRUE)
              )
            )
          )
        ),
        fixedRow(
          column(
            3,
            h6("show:")
          ),
          column(
            9,
            do.call(
              checkboxGroupInput,
              list(paste0(tabPanelName, "plotShow"),
                   label = NULL,
                   choices = c("stackedColors", "outlines"),
                   selected = c(if(loonWidgetsInfo$showStackedColors) "stackedColors",
                                if(loonWidgetsInfo$showOutlines) "outlines"),
                   inline = TRUE
              )
            )
          )
        ),
        fixedRow(
          column(
            3,
            h6("yshows:")
          ),
          column(
            9,
            do.call(
              radioButtons,
              list(paste0(tabPanelName, "yshows"),
                   label = NULL,
                   choices = c("frequency", "density"),
                   selected = loonWidgetsInfo$yshows,
                   inline = TRUE)
            )
          )
        ),
        fixedRow(
          column(
            3,
            h6("scale:")
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
