loonGrob_name <- function(loon_grob) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("loonGrob_name", obj)
}

loonGrob_name.default <- function(loon_grob) {
  name <- grobName(loon_grob)
  stringr::str_split(name, pattern = "[.]")[[1]][2]
}

loonGrob_name.l_plot <- function(loon_grob) "plot"
loonGrob_name.l_hist <- function(loon_grob) "hist"
loonGrob_name.l_graph <- function(loon_grob) "graph"
loonGrob_name.l_serialaxes <- function(loon_grob) "serialaxes"
