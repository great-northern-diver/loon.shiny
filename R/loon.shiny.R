#' @title Automatically Create a \code{shiny} App Based on Interactive \code{Loon} Widgets
#' @name loon.shiny
#' @description Interactive \code{loon} widgets displayed in a \code{shiny} app
#' @param widgets A \code{loon} widget or a list of \code{loon} widgets. If the input is a
#' \code{ggplot} object, the \code{ggplot} object will be turned into a \code{loon} widget
#' via \code{\link{ggplot2loon}}.
#' @param selectBy The way to brush, can be 'brushing' (keep the brush whenever the plot is updated),
#' 'sweeping' (clear the brush whenever the plot is updated) or 'byDefault' (determined by \code{loon} widget 'selectBy')
#' @param showWorldView Logical; whether to show the world view.
#' @param plotRegionWidth Plot region width. Must be a valid \code{CSS} unit (like '100%', '400px') or a number,
#' which will be coerced to a string and have 'px' appended.
#' @param plotRegionHeight Plot region height.
#' @param plotRegionBackground Plot region background color
#' @param layoutMatrix Optional layout matrix to place \code{loon} widgets. See \code{layout_matrix} in \code{\link{grid.arrange}}.
#' @param nrow Number of rows, see \code{\link{grid.arrange}}.
#' @param ncol Number of columns, see \code{\link{grid.arrange}}.
#' @param widths A unit vector giving the width of each plot.
#' @param heights A unit vector giving the height of each plot.
#' @param displayedPanel A string vector. The default is an empty string \code{""} so that
#' none inspector components (\code{Plot}, \code{Linking}, \code{Select}, etc) are open automatically.
#' The available strings are \code{c("Plot", "Select", "Linking", "Modify", "Layer", "Glyph")}
#' @param colorList A list of colors displayed on modify panel.
#' @param inspectorLocation A length four vector representing the distance between the
#' \code{bottom}, \code{left}, \code{top} and \code{right} of the inspector panel
#' and the \code{bottom}, \code{left}, \code{top} and \code{right} of the page or
#' parent container.
#' @param inspectorWidth Width of the inspector panel.
#' @param inspectorHeight Height of the inspector panel.
#' @param options \code{shinyApp} argument that should be passed to the \code{runApp} call, see \code{\link{shinyApp}}.
#' @param ... Named arguments to modify shiny app.
#'
#' @details
#' \itemize{
#'  \item{Useful hints for a \code{loon.shiny} app}
#'  {
#'  \itemize{
#'  \item{}{The inspector can be switched either by ``toggling tabpanel'' in the bar menu or
#'  the last mouse gesture input (\code{<double-click>}) on the plot region}
#'  \item{}{To downlight the selected elements, one has to double click on the plot region}
#'  \item{}{In \code{loon}, holding down the \code{<shift>} key while pressing the left button keeps the current selection states.
#'  In \code{loon.shiny} app, \code{<shift>} key is replaced by a `sticky` radiobutton. If the `sticky` mode is on,
#'  while sweeping, current selection states remain; else new selection will eliminate the previous selection states.}
#'  }
#'  }
#'  \item{Useful hints for a \code{loon.shiny} markdown file}{
#'  \itemize{
#'  \item{}{Based on our experience, setting `out.width` or `out.height` (try "10px") in the chunk could give a better layout}
#'  \item{}{To modify the app size, set `options = list(height = **, width = **)` in \code{loon.shiny()}}
#'  }
#'  }
#' }
#'
#' @return A \code{shiny.appobj} object that represents the \code{loon.shiny} app.
#' Printing the object or passing it to \code{\link{runApp}} will run the app.
#'
#' @import loon shiny grid gridExtra methods tcltk grDevices stats loon.ggplot
#' @importFrom colourpicker colourInput
#' @importFrom base64enc dataURI
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
#'   loon.shiny(p)
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
#'   loon.shiny(list(p1, p2, p3),
#'              layoutMatrix = matrix(c(2,NA,1,3),
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
#'       loon.shiny(g)
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

loon.shiny <- function(widgets,
                       selectBy = c("byDefault","brushing", "sweeping"),
                       showWorldView = TRUE,
                       plotRegionWidth = "500px",
                       plotRegionHeight = "500px",
                       plotRegionBackground = "gray92",
                       layoutMatrix = NULL,
                       nrow = NULL,
                       ncol = NULL,
                       widths = NULL,
                       heights = NULL,
                       displayedPanel = "",
                       colorList = loon::l_getColorList(),
                       inspectorLocation = c("auto", "auto", "60px", "20px"),
                       inspectorWidth = "350px",
                       inspectorHeight = "auto",
                       options = list(),
                       ...) {

  if(is.ggplot(widgets)) {
    message("The input is a `ggplot` object and will be transformed to a `loon` widget.")
    widgets <- loon.ggplot::ggplot2loon(widgets)
  }

  if(!loon::l_isLoonWidget(widgets) & !is(widgets, "l_compound")) {
    lapply(widgets,
           function(widget){
             if(!loon::l_isLoonWidget(widget) & !is(widget, "l_compound"))
               stop("loon widget does not exist")
           }
    )
  }

  # use the underscore case to match argument in the `arrangeGrob`
  layout_matrix <- layoutMatrix
  if(length(colorList) == 0) stop("`colorList` cannot be length zero")

  stopifnot(length(inspectorLocation) == 4)

  # html background color may have chance to fail recognizing the color name
  # but it can understand the hex code 100%
  colorList <- col2hex(colorList)

  loonInfo <- get_loonInfo(widgets,
                           layout_matrix,
                           nrow = nrow,
                           ncol = ncol)
  # get grobs
  loon.grobs <- loonInfo$loon.grobs
  # get widgets info
  loonWidgetsInfo <- loonInfo$loonWidgetsInfo
  # get locations
  layout_matrix <- loonInfo$layout_matrix
  nrow <- loonInfo$nrow
  ncol <- loonInfo$ncol
  gtable <- loonInfo$gtable

  selectBy <- match.arg(selectBy)

  ui <- loon.ui(
    loon.grobs = loon.grobs,
    plotRegionWidth = plotRegionWidth,
    plotRegionHeight = plotRegionHeight,
    loonWidgetsInfo = loonWidgetsInfo,
    selectBy = selectBy,
    top = inspectorLocation[3],
    left = inspectorLocation[2],
    right = inspectorLocation[4],
    bottom = inspectorLocation[1],
    inspectorWidth = inspectorWidth,
    inspectorHeight = inspectorHeight,
    showWorldView = showWorldView,
    colorList = colorList,
    displayedPanel = displayedPanel,
    ...)

  server <- loon.server(
    input,
    output,
    session,
    loon.grobs = loon.grobs,
    gtable = gtable,
    showWorldView = showWorldView,
    loonWidgetsInfo = loonWidgetsInfo,
    selectBy = selectBy,
    colorList = colorList,
    plotRegionBackground = plotRegionBackground,
    arrangeGrobArgs = list(
      widths = widths,
      heights = heights,
      nrow = nrow,
      ncol = ncol,
      layout_matrix = layout_matrix
    ))

  shiny::shinyApp(ui, server, options = options)
}
