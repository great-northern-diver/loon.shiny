loonGrob_name <- function(loon.grob) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("loonGrob_name", obj)
}

loonGrob_name.default <- function(loon.grob) {
  name <- grobName(loon.grob)
  strsplit(name, split = "[.]")[[1]][2]
}

loonGrob_name.l_plot <- function(loon.grob) "plot"
loonGrob_name.l_hist <- function(loon.grob) "hist"
loonGrob_name.l_graph <- function(loon.grob) "graph"
loonGrob_name.l_serialaxes <- function(loon.grob) "serialaxes"
