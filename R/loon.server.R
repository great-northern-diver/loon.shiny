loon.server <- function(input, output, session, update = TRUE, loon.grobs, gtable = NULL,
                        showWorldView = TRUE, loonWidgetsInfo, selectBy = NULL,
                        colorList = loon::l_getColorList(),
                        plotRegionBackground = "gray92", arrangeGrobArgs = list()) {

  arrangeGrobArgs <- remove_null(arrangeGrobArgs, as_list = FALSE)

  noneInteractiveGrobs_index <- get_noneInteractiveGrobs_index(loon.grobs)

  # Get each grob position
  # The position is calculated twice, the other is in `server` function,.
  # Check that for details
  positions <- tryCatch(
    expr = {
      loonGrob_positions(gtable,
                         loon.grobs,
                         arrangeGrobArgs = arrangeGrobArgs)
    },
    error = function(e) NULL
  )

  # 1. check whether the layout_matrix, nrow, ncol, widths, heights are valid
  # 2. rearrange the grobs if widths are heights provided
  # since in the position specification (next line), no widths and heights are considered
  # if the layout_matrix is not rearranged, the selection in shiny will not work properly
  arrangeGrobArgs <- adjust_arrangeGrobArgs(arrangeGrobArgs, n = length(loon.grobs))

  n <- length(loon.grobs)
  tabPanelNames <- names(loon.grobs)

  runIndex <- seq(n)
  outputInfo <- lapply(runIndex, function(j) get_outputInfo(loon.grobs[[j]], loonWidgetsInfo[[j]]))
  output.grobs <- lapply(runIndex, function(j) NULL)

  selectBy <- get_selectBy(selectBy, loonWidgetsInfo)

  linkingGroups <- sapply(runIndex, function(j) loonWidgetsInfo[[j]]$linkingGroup)
  linkingInfo <- get_linkingInfo(linkingGroups, loonWidgetsInfo, tabPanelNames, n)

  count <- 0L
  server <- function(input, output, session) {

    # set action buttons
    button_list <- lapply(runIndex,
                          function(j) {
                            button_values(loon.grob = loon.grobs[[j]],
                                          tabPanelName = tabPanelNames[j],
                                          input = input,
                                          colorList = colorList,
                                          loonWidgetsInfo = loonWidgetsInfo[[j]])
                          }
    )

    # In server function, the order of execution is
    # `update_sidebarPanel` --> render `plot` --> render `world view` --> `update_sidebarPanel`
    # update tab panel
    shiny::observe({

      pos <- get_currentSiderBar(positions, input, noneInteractiveGrobs_index)

      if(length(pos) > 0) {
        shiny::updateNavbarPage(
          session, "navBarPage", selected = tabPanelNames[pos]
        )
      }

      currentSiderBar <- input[["navBarPage"]]
      runIndex <<- c(which(tabPanelNames == currentSiderBar), which(tabPanelNames!= currentSiderBar))

      # update ui
      # slider bar names (xlim to ylim, vice versa), values, ...
      # color check box
      lapply(runIndex,
             function(j) {

               update_sidebarPanel(
                 loon.grob = loon.grobs[[j]],
                 buttons = button_list[[j]],
                 session,
                 input,
                 colorList = colorList,
                 linkingInfo = linkingInfo,
                 linkingGroup = linkingGroups[j],
                 linkingGroups = linkingGroups,
                 tabPanelName = tabPanelNames[j],
                 tabPanelNames = tabPanelNames,
                 outputInfo = outputInfo[[j]]
               )
             }
      )

      output$plots <-  shiny::renderPlot({

        loon_reactive_grobs <- lapply(runIndex,
                                      function(j) {

                                        reactive_grobs_info <- loon_reactive(
                                          loon.grob = loon.grobs[[j]],
                                          output.grob = output.grobs[[j]],
                                          linkingInfo = linkingInfo,
                                          buttons = button_list[[j]],
                                          position = positions[j, ],
                                          selectBy = selectBy,
                                          linkingGroup = linkingGroups[j],
                                          input,
                                          colorList = colorList,
                                          tabPanelName = tabPanelNames[j],
                                          outputInfo = outputInfo[[j]]
                                        )

                                        # loon grobs
                                        loon.grobs[[j]] <<- reactive_grobs_info$loon.grob
                                        output.grobs[[j]] <<- reactive_grobs_info$output.grob

                                        # update output info
                                        outputInfo[[j]] <<- reactive_grobs_info$outputInfo

                                        # update linking Group
                                        linkingGroups[j] <<- outputInfo[[j]]$linkingGroup

                                        # update linkingInfo
                                        linkingInfo <<- outputInfo[[j]]$linkingInfo

                                        # update button list
                                        button_list[[j]] <<- outputInfo[[j]]$buttons

                                        return(reactive_grobs_info$output.grob)
                                      }
        )

        # the `positions` matrix is calculated again.
        # reason: inside the function, we call `grid::convertUnit()` to
        # to convert an equivalent unit object.
        # The new "unit" (`unitTo`) is `npc`, only if the graphics are drawn,
        # the conversion is precise.
        if(count == 0) {
          positions <<- loonGrob_positions(gtable,
                                           loon.grobs,
                                           arrangeGrobArgs = arrangeGrobArgs)

          count <<- count + 1
        }

        # Update display
        # If it is a facet grob or ggplot grob
        # since, rather than displays
        # tklabels are packed on the window
        # use the gtable, all tklabels can be preserved.
        grid::grid.draw(set_grobFromGtable(gtable,
                                           newGrobs = loon_reactive_grobs[order(runIndex)],
                                           plotRegionBackground = plotRegionBackground,
                                           arrangeGrobArgs = arrangeGrobArgs))
      })

      if("itemLabels" %in% input[[paste0(currentSiderBar, "itemLabels")]]) {
        output$text <- shiny::renderText({
          info <- outputInfo[[runIndex[1L]]]
          brushId <- info$brushId
          loonWidgetsInfo <- info$loonWidgetsInfo
          itemLabel <- loonWidgetsInfo$itemLabel

          if(length(brushId) == 0 || length(itemLabel) == 0) NULL
          else {
            itemLabel[brushId]
          }
        })
      } else {
        output$text <- shiny::renderText({NULL})
      }

      if(showWorldView) {
        # only update the current world view
        output[[paste0(currentSiderBar, "plot_world_view")]] <- shiny::renderPlot({

          id <- which(tabPanelNames %in% currentSiderBar)

          grid::grid.draw(loon_worldView(output.grobs[[id]],
                                         input, currentSiderBar,
                                         colorList = colorList,
                                         loonWidgetsInfo = outputInfo[[id]]$loonWidgetsInfo))
        })
      }
    })
  }

  server
}
