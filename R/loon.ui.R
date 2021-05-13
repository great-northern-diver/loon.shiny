loon.ui <- function(loon_grobs, 
                    plotWidth = "100%", 
                    plotHeight = "400px", 
                    inspectorWidth = 330, 
                    inspectorHeight = "auto",
                    top = 60, 
                    left = "auto", 
                    right = 20, 
                    bottom = "auto",
                    loonWidgets_info,
                    selectBy = NULL, 
                    showWorldView = TRUE,
                    envir = parent.frame(), 
                    ...) {
  
  loon_grobs <- adjust_loon_grobs(loon_grobs, loonWidgets_info)
  # update loon_grobs in parent env
  assign("loon_grobs", loon_grobs, envir = envir)
  tabPanelNames <- names(loon_grobs)
  
  n <- length(loon_grobs)
  selectBy <- get_selectBy(selectBy, loonWidgets_info)
  
  linkingGroups <- sapply(seq(n),
                          function(i) {
                            loonWidgets_info[[i]]$linkingGroup
                          }
  )
  
  navbarMenuNames <- sapply(seq(n), function(i) loonWidgets_info[[i]]$navbarMenuName)
  
  # set loon inspector
  sidebarPanel_args <- lapply(seq(n),
                              function(i){
                                loon_sidebarPanel(
                                  loon_grob = loon_grobs[[i]],
                                  tabPanelName = tabPanelNames[i],
                                  selectBy = selectBy, 
                                  linkingGroup = linkingGroups[i], 
                                  linkingGroups = linkingGroups,
                                  loonWidgets_info = loonWidgets_info[[i]],
                                  showWorldView = showWorldView
                                )
                              }
  )
  
  # set ui
  args <- list(...)
  ui <- shiny::fluidPage(
    if(!is.null(args$titlePanel_title)) {
      if(is.null(args$titlePanel_align)) args$titlePanel_align <- "center"
      if(is.null(args$titlePanel_size)) args$titlePanel_size <- function(title, align) shiny::h2(title, align)
      titlePanel(title = args$titlePanel_size(args$titlePanel_title, align = args$titlePanel_align))
    },
    shiny::absolutePanel(id = "controls", 
                         class = "panel panel-default",
                         draggable = TRUE, 
                         top = top, 
                         left = left, 
                         right = right, 
                         bottom = bottom,
                         width = inspectorWidth, 
                         height = inspectorHeight,
                         set_tabPanel(sidebarPanel_args, navbarMenuNames)
    ),
    shiny::plotOutput(outputId="plots",
                      brush = shiny::brushOpts(id = "plot_brush",
                                               resetOnNew = (selectBy == "sweeping")),
                      dblclick = "plot_click",
                      width = plotWidth, 
                      height = plotHeight)
  )
  
  ui
}
