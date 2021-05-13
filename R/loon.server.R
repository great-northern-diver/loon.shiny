loon.server <- function(input, output, session, update = TRUE, loon_grobs, gtable = NULL,
                        layout_matrix, showWorldView = TRUE, nrow = NULL, ncol = NULL,
                        loonWidgets_info, selectBy = NULL,
                        arrangeGrobArgs = list()) {

  if(length(arrangeGrobArgs) != 0) {
    if(is.null(names(arrangeGrobArgs))) stop("names cannot be ignored in arrangeGrobArgs")
    if(!all(names(arrangeGrobArgs) %in% methods::formalArgs(gridExtra::arrangeGrob))) stop("undefined arguments for arrangeGrob")
  }

  arrangeGrobArgs$nrow <- nrow
  arrangeGrobArgs$ncol <- ncol
  if(!is.null(layout_matrix)) {
    arrangeGrobArgs$layout_matrix <- layout_matrix
  }
  noneInteractiveGrobs_index <- get_noneInteractiveGrobs_index(loon_grobs)

  # get each grob position
  positions <- loonGrob_positions(gtable,
                                  loon_grobs, 
                                  layout_matrix, 
                                  nrow = arrangeGrobArgs$nrow, 
                                  ncol = arrangeGrobArgs$ncol)

  n <- length(loon_grobs)
  tabPanelNames <- names(loon_grobs)

  output_info <- lapply(1:n, function(j) get_output_info(loon_grobs[[j]], loonWidgets_info[[j]]))
  output_grobs <- lapply(1:n, function(j) NULL)
  
  selectBy <- get_selectBy(selectBy, loonWidgets_info)
  
  linkingGroups <- sapply(1:n, function(j) loonWidgets_info[[j]]$linkingGroup)
  linkingInfo <- get_linkingInfo(linkingGroups, loonWidgets_info, tabPanelNames) 
  
  runIndex <- seq(n)

  server <- function(input, output, session) {
    
    # set action buttons  
    button_list <- lapply(runIndex,
                          function(j) {
                            button_values(loon_grob = loon_grobs[[j]],
                                          tabPanelName = tabPanelNames[j],
                                          input = input,
                                          colorList = loonWidgets_info[[j]]$colorList)
                          }
    )

    # update tab panel
    shiny::observe({
      
      pos <- get_currentSiderBar(positions, input, noneInteractiveGrobs_index)
      
      if(length(pos) != 0) {
        shiny::updateNavbarPage(
          session, "navBarPage", selected = tabPanelNames[pos]
        )
      }

      currentSiderBar <- input[["navBarPage"]]
      runIndex <<- c(which(tabPanelNames == currentSiderBar), which(tabPanelNames!= currentSiderBar))

      output$plots <-  shiny::renderPlot({

        loon_reactive_grobs <- lapply(runIndex,
                                      function(j) {
                                        
                                        reactive_grobs_info <- loon_reactive(
                                          loon_grob = loon_grobs[[j]],
                                          output_grob = output_grobs[[j]],
                                          linkingInfo = linkingInfo,
                                          buttons = button_list[[j]],
                                          position = positions[j, ],
                                          selectBy = selectBy,
                                          linkingGroup = linkingGroups[j],
                                          input,
                                          tabPanelName = tabPanelNames[j],
                                          output_info = output_info[[j]]
                                        )
                                        
                                        # loon grobs
                                        loon_grobs[[j]] <<- reactive_grobs_info$loon_grob
                                        output_grobs[[j]] <<- reactive_grobs_info$output_grob
                                        
                                        # update output info
                                        output_info[[j]] <<- reactive_grobs_info$output_info
                                        
                                        # update linking Group
                                        linkingGroups[j] <<- output_info[[j]]$linkingGroup
                                        
                                        # update linkingInfo
                                        linkingInfo <<- output_info[[j]]$linkingInfo
                                        
                                        return(reactive_grobs_info$output_grob)
                                      }
        )
        # Update display
        # If it is a facet grob or ggplot grob
        # since, rather than displays
        # tklabels are packed on the window
        # use the gtable, all tklabels can be preserved.
        grid::grid.draw(set_grobFromGtable(gtable, 
                                           newGrobs = loon_reactive_grobs[order(runIndex)], 
                                           arrangeGrobArgs = arrangeGrobArgs))
      })

      if(showWorldView) {
        # only update the current world view
        output[[paste0(currentSiderBar, "plot_world_view")]] <- shiny::renderPlot({
          
          id <- which(tabPanelNames %in% currentSiderBar)
          
          loon_reactive_worldView_grob <- loon_reactive_worldView(
            loon_grob = loon_grobs[[id]],
            buttons = button_list[[id]],
            input,
            tabPanelName = currentSiderBar,
            output_info = output_info[[id]]
          )
          
          grid::grid.draw(loon_reactive_worldView_grob)
        })  
      }
      
      # update ui
      # update slider bar names (xlim to ylim, vice versa), values, ...
      # update color check box
      lapply(runIndex,
             function(j) {

               update_sidebarPanel(
                 loon_grob = loon_grobs[[j]],
                 buttons = button_list[[j]],
                 session,
                 input,
                 linkingInfo = linkingInfo,
                 linkingGroup = linkingGroups[j],
                 linkingGroups = linkingGroups,
                 tabPanelName = tabPanelNames[j],
                 tabPanelNames = tabPanelNames,
                 output_info = output_info[[j]]
               )
             }
      )
    })
  }

  server
}
