#' @title Automatically Create a \code{shiny} App Based on Interactive \code{Loon} Widgets
#' 
#' @description Interactive \code{loon} widgets displayed in \code{shiny} app
#' @param widgets a \code{loon} widget or a list of \code{loon} widgets.
#' @param showWorldView logical; whether to show the world view.
#' @param plotWidth Image width, same with \code{height}.
#' @param plotHeight Image height. Must be a valid \code{CSS} unit (like '100%', '400px') or a number, 
#'                    which will be coerced to a string and have 'px' appended. More details, see \code{\link{plotOutput}}.
#' @param selectBy The way to brush, can be 'brushing' (keep the brush whenever the plot is updated), 
#'                  'sweeping' (clear the brush whenever the plot is updated) or 'byDefault' (determined by \code{loon} widget 'selectBy', 
#'                  if 'selectBy' is sweeping, then \code{selectBy} will be assigned to 'sweeping', vice versa)
#' @param layout_matrix Optional layout matrix to place \code{loon} widgets, see \code{\link{grid.arrange}}.
#' @param nrow Number of rows, see \code{\link{grid.arrange}}.
#' @param ncol Number of columns, see \code{\link{grid.arrange}}.
#' @param colorList A list of colors displayed on modify panel.
#' @param top Distance between the top of the inspector panel, and the top of the page or parent container.
#' @param left Distance between the left side of the inspector panel, and the left of the page or parent container.
#' @param right Distance between the right side of the inspector panel, and the right of the page or parent container.
#' @param bottom Distance between the bottom of the inspector panel, and the bottom of the page or parent container.
#' @param inspectorWidth Width of the inspector panel.
#' @param inspectorHeight Height of the inspector panel.
#' @param arrangeGrobArgs Other \code{arrangeGrob} arguments to place the \code{loon} widgets, see \code{\link{grid.arrange}}.
#' @param options \code{shinyApp} argument that should be passed to the \code{runApp} call, see \code{\link{shinyApp}}.
#' @param ... Named arguments to modify shiny app.
#' 
#' @details Useful hints:
#' \itemize{
#'  \item{}{The effect of any \code{actionButton}s can be terminated by a simple click on canvas.}
#'  \item{}{The choices of 'select_by_color' can be updated by double click on color 'apply' \code{actionButton}.}
#' }
#' 
#' @return A \code{shiny.appobj} object that represents the \code{loon.shiny} app. 
#' Printing the object or passing it to \code{\link{runApp}} will run the app.
#'
#' @import loon shiny grid gridExtra magrittr methods tcltk grDevices
#' @importFrom colourpicker colourInput
#' @importFrom stringr str_detect str_split
#' @importFrom base64enc dataURI
#' @importFrom stats rnorm setNames
#'
#' @export
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if(interactive()) {
#'   ############### Basic ###############
#'   p <- l_plot(iris, 
#'               color = iris$Species, 
#'               showGuides = TRUE, 
#'               showScales = TRUE)
#' 
#'   shiny.loon(p)
#'   
#'   ############### Link multiple plots ###############
#'   p1 <- l_plot(iris,
#'                linkingGroup = 'iris', 
#'                showLabels = FALSE)
#'   p2 <- l_hist(iris$Sepal.Length, 
#'                linkingGroup = 'iris', 
#'                showLabels = FALSE, 
#'                showStackedColors = TRUE)
#'   p3 <- l_hist(iris$Sepal.Width, linkingGroup = 'iris', 
#'                color = iris$Species, sync = 'push',
#'                showLabels = FALSE, swapAxes = TRUE, 
#'                showStackedColors = TRUE)
#'   shiny.loon(list(p1, p2, p3), 
#'              layout_matrix = matrix(c(2,NA,1,3), 
#'              nrow = 2, byrow = TRUE))
#' }
#' 
#' \dontrun{
#'   if (requireNamespace('loon.ggplot', quietly = TRUE)) {
#'       p <- ggplot(mpg, aes(displ, hwy)) +
#'         geom_point(data = transform(mpg, class = NULL), colour = 'grey85') +
#'         geom_point() +
#'         facet_wrap(~class)
#'       g <- loon.ggplot(p, activeGeomLayers = 2) # active the second layer
#'       shiny.loon(g)
#'   }
#' }


# TODO: 
# 1. "selectBy = sweeping", invert dynamic selection is not right: DONE
# 2. to add "sticky" in  l_hist, l_serialaxes: DONE
# 3. "by color": change checkboxGroup to selectInput: 
# 4. A bug. Turn "sticky" on with brush window, change to invert dynamic selection, try click on "plot" or "world": leave it
# 5. glyph set in l_plot
# 6. world view, only color, activition are dispatched. size and glyph are not: DONE
# 7. glyph set in l_graph (maybe no)
# 8. no itemLabel (grob thing)
# 9. no group layers
# 10. layers : no into a layer group and out a layer group buttons
# 11. add linkedStates on inspector: DONE
# 12. no sweeping and brushing (can be set at the beginning but cannot be set interactively)
# 13. add "sticky" to replace "shift": DONE

shiny.loon <- function(widgets, 
                       showWorldView = TRUE,
                       plotWidth = '500px', 
                       plotHeight = '500px',
                       selectBy = c('byDefault','brushing', 'sweeping'),
                       layout_matrix, 
                       nrow = NULL, 
                       ncol = NULL, 
                       colorList = loon::l_getColorList(),
                       top = 60, 
                       left = 'auto', 
                       right = 20, 
                       bottom = 'auto',
                       inspectorWidth = 350, 
                       inspectorHeight = 'auto',
                       options = list(), 
                       arrangeGrobArgs = list(), 
                       ...) {
  
  if(!loon::l_isLoonWidget(widgets) & !is(widgets, "l_compound")) {
    lapply(widgets,
           function(widget){
             if(!loon::l_isLoonWidget(widget) & !is(widget, "l_compound"))
               stop("loon widget does not exist")
           }
    )
  }
  
  loonInfo <- get_loonInfo(widgets, 
                           layout_matrix, 
                           nrow = nrow, 
                           ncol = ncol, 
                           colorList = col2hex(colorList))
  # get grobs
  loon_grobs <- loonInfo$loon_grobs
  # get widgets info
  loonWidgets_info <- loonInfo$loonWidgets_info
  # get locations
  layout_matrix <- loonInfo$layout_matrix
  nrow <- loonInfo$nrow
  ncol <- loonInfo$ncol
  gtable <- loonInfo$gtable
  
  selectBy <- match.arg(selectBy)
  
  ui <- loon.ui(
    loon_grobs = loon_grobs,
    plotWidth = plotWidth,
    plotHeight = plotHeight,
    loonWidgets_info = loonWidgets_info,
    selectBy = selectBy, 
    top = top, 
    left = left, 
    right = right, 
    bottom = bottom,
    inspectorWidth = inspectorWidth, 
    inspectorHeight = inspectorHeight,
    showWorldView = showWorldView,
    ...)
  
  server <- loon.server(
    input,
    output,
    session,
    loon_grobs = loon_grobs,
    gtable = gtable,
    layout_matrix = layout_matrix,
    showWorldView = showWorldView,
    nrow = nrow,
    ncol = ncol,
    loonWidgets_info = loonWidgets_info,
    selectBy = selectBy,
    arrangeGrobArgs = arrangeGrobArgs)
  
  shiny::shinyApp(ui, server, options = options)
}
