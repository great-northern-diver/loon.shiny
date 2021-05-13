loonWidget_name <- function(widget) {
  UseMethod("loonWidget_name", widget)
}

loonWidget_name.default <- function(widget) stop("unspecified")

loonWidget_name.l_plot <- function(widget) "Scatterplot"
loonWidget_name.l_hist <- function(widget) "Histogram"
loonWidget_name.l_graph <- function(widget) "Graph"
loonWidget_name.l_serialaxes <- function(widget) "Serialaxes"

loonWidget_name.l_facet_ggplot <- function(widget) "Ggplot"
loonWidget_name.l_ts <- function(widget) "Time Series"
loonWidget_name.l_pairs <- function(widget) "Pairs"
