loon.ui <- function(loon.grobs,
                    plotRegionWidth = "100%",
                    plotRegionHeight = "400px",
                    inspectorWidth = 330,
                    inspectorHeight = "auto",
                    top = 60,
                    left = "auto",
                    right = 20,
                    bottom = "auto",
                    loonWidgetsInfo,
                    selectBy = NULL,
                    colorList = loon::l_getColorList(),
                    showWorldView = TRUE,
                    displayedPanel = NULL,
                    envir = parent.frame(),
                    ...) {

  loon.grobs <- adjust_loon.grobs(loon.grobs, loonWidgetsInfo)
  # update loon.grobs in parent env
  assign("loon.grobs", loon.grobs, envir = envir)
  tabPanelNames <- names(loon.grobs)

  n <- length(loon.grobs)
  selectBy <- get_selectBy(selectBy, loonWidgetsInfo)

  linkingGroups <- sapply(seq(n),
                          function(i) {
                            loonWidgetsInfo[[i]]$linkingGroup
                          }
  )

  navbarMenuNames <- sapply(seq(n), function(i) loonWidgetsInfo[[i]]$navbarMenuName)

  # set loon inspector
  sidebarPanel_args <- lapply(seq(n),
                              function(i){
                                loon_sidebarPanel(
                                  loon.grob = loon.grobs[[i]],
                                  tabPanelName = tabPanelNames[i],
                                  colorList = colorList,
                                  selectBy = selectBy,
                                  linkingGroup = linkingGroups[i],
                                  linkingGroups = linkingGroups,
                                  loonWidgetsInfo = loonWidgetsInfo[[i]],
                                  showWorldView = showWorldView,
                                  displayedPanel = displayedPanel
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
                      brush = shiny::brushOpts(id = "plotBrush",
                                               resetOnNew = (selectBy == "sweeping")),
                      dblclick = "plotClick",
                      width = plotRegionWidth,
                      height = plotRegionHeight)
  )

  ui
}
