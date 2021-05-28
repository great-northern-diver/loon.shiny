#' shiny.loon (deprecated)
#' @description Package \code{loon.shiny} only provides one exported function. To make it easier for
#' users to remember, we decide to name the function with the selfsame name, \code{loon.shiny}.
#' @export
#' @inheritParams loon.shiny
#' @keywords internal
#' @name shiny.loon-deprecated
#'
shiny.loon <- function(widgets,
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

  .Deprecated("loon.shiny", package= "loon.shiny")

  loon.shiny(widgets = widgets,
             selectBy = selectBy,
             showWorldView = showWorldView,
             plotRegionWidth = plotRegionWidth,
             plotRegionHeight = plotRegionHeight,
             plotRegionBackground = plotRegionBackground,
             layoutMatrix = layoutMatrix,
             nrow = nrow,
             ncol = ncol,
             widths = widths,
             heights = heights,
             displayedPanel = displayedPanel,
             colorList = colorList,
             inspectorLocation = inspectorLocation,
             inspectorWidth = inspectorWidth,
             inspectorHeight = inspectorHeight,
             options = options,
             ...)
}
