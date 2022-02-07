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
                    toolboxWidth = "300px",
                    toolboxLocation = c(-20, 10),
                    envir = parent.frame(),
                    ...) {

  loon.grobs <- adjust_loon_grobs(loon.grobs, loonWidgetsInfo)
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

  toolbox.x <- if(toolboxLocation[1] > 0) {
    paste0("+", toolboxLocation[1])
  } else {
    paste0("-", abs(toolboxLocation[1]))
  }


  toolbox.y <- if(toolboxLocation[2] > 0) {
    paste0("+", toolboxLocation[2])
  } else {
    paste0("-", abs(toolboxLocation[2]))
  }

  # set ui
  args <- list(...)
  ui <- shiny::fluidPage(
    if(!is.null(args$titlePanel_title)) {
      if(is.null(args$titlePanel_align))
        args$titlePanel_align <- "center"
      if(is.null(args$titlePanel_size))
        args$titlePanel_size <- function(title, align) shiny::h2(title, align)
      titlePanel(title = args$titlePanel_size(args$titlePanel_title,
                                              align = args$titlePanel_align))
    },
    ##################### tooltip #####################
    # Inspired by https://stackoverflow.com/questions/27965931/tooltip-when-you-mouseover-a-ggplot-on-shiny
    #   tags$head(tags$style('
    #    #tooltip {
    #     position: absolute;
    #     width: 300px;
    #     z-index: 100;
    #    }
    # ')),
    # tags$script('
    #   $(document).ready(function(){
    #     // id of the plot
    #     $("#plots").mousemove(function(e){
    #
    #       // ID of uiOutput
    #       $("#tooltip").show();
    #       $("#tooltip").css({
    #         top: (e.pageY + 5) + "px",
    #         left: (e.pageX + 5) + "px"
    #       });
    #     });
    #   });
    # '),
    tags$head(
      tags$style(
        paste0(
          "#tooltip {position: absolute;width: ",
          toolboxWidth,
          ";z-index: 100;}"
        )
      )
    ),
    tags$script(
      paste0(
        "$(document).ready(function(){$('#plots').mousemove(function(e){
         $('#tooltip').show(); $('#tooltip').css({top: (e.pageY ",
      toolbox.y,
      ") + 'px',left: (e.pageX ",
      toolbox.x,
      ") + 'px'});});});"
      )
    ),
    ##################### end #####################

    shiny::absolutePanel(id = "controls",
                         class = "panel panel-default",
                         draggable = TRUE,
                         top = top,
                         left = left,
                         right = right,
                         bottom = bottom,
                         width = inspectorWidth,
                         height = inspectorHeight,
                         set_tabPanel(sidebarPanel_args,
                                      navbarMenuNames)
    ),
    shiny::plotOutput(outputId = "plots",
                      width = plotRegionWidth,
                      height = plotRegionHeight,
                      brush = shiny::brushOpts(id = "plotBrush",
                                               resetOnNew = (selectBy == "sweeping")),
                      dblclick = "plotClick",
                      hover = shiny::hoverOpts(id = "plotHover", delay = 0)
    ),
    shiny::uiOutput("tooltip")
  )

  ui
}
