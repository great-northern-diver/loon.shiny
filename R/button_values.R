button_values <- function(loon.grob, tabPanelName, input, colorList, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("button_values", obj)
}

button_values.default <- function(loon.grob, tabPanelName, input, colorList, ...) NULL

button_values.l_plot <- function(loon.grob, tabPanelName, input, colorList, ...) {

  names <- c("all", "none", "invert", "deactive", "reactive", c(colorList, "colorApply"),
             "circle", "square", "triangle", "push", "pull", "alphaApply",
             "ocircle", "osquare", "otriangle",
             "ccircle", "csquare", "ctriangle",
             "glyphSet", "halign", "valign", "hdist",
             "vdist", "grid", "jitter", "reset",
             "absToPlus", "absToMinus",  "relToPlus", "relToMinus",
             "select", "plot", "world", "layerUp", "layerDown", "layerVisible",
             "layerInvisible", "layerPlus",
             "layerMinus", "scaleToLayer", "layerSet")
  setNames(rep(0L, length(names)), names)
}

button_values.l_graph <- function(loon.grob, tabPanelName, input, colorList, ...) {
  button_values.l_plot(loon.grob, tabPanelName, input, colorList)
}

button_values.l_hist <- function(loon.grob, tabPanelName, input, colorList, ...) {

  names <- c("all", "none", "invert", "deactive", "reactive", c(colorList, "colorApply"),
             "plot", "world", "layerUp", "layerDown", "layerVisible", "push", "pull",
             "layerInvisible", "layerPlus", "binwidth", "origin",
             "layerMinus", "scaleToLayer", "layerSet")
  x <- setNames(rep(0L, length(names)), names)

  args <- list(...)
  loonWidgetsInfo <- args$loonWidgetsInfo

  x["binwidth"] <- loonWidgetsInfo$binwidth
  x["origin"] <- loonWidgetsInfo$origin
  x
}

button_values.l_serialaxes <- function(loon.grob, tabPanelName, input, colorList, ...) {

  names <- c("all", "none", "invert", "deactive", "reactive", c(colorList, "colorApply"),
             "alphaApply", "absToPlus", "absToMinus",  "relToPlus", "relToMinus",
             "push", "pull")
  setNames(rep(0L, length(names)), names)
}

