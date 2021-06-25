`%||%` <- function (x, y) {
  if (is.null(x))
    y
  else x
}

isZero <- function (x, neps = 1, eps = .Machine$double.eps, ...) {
  if (is.character(eps)) {
    eps <- match.arg(eps, choices = c("double.eps", "single.eps"))
    if (eps == "double.eps") {
      eps <- .Machine$double.eps
    }
    else if (eps == "single.eps") {
      eps <- sqrt(.Machine$double.eps)
    }
  }
  (abs(x) < neps * eps)
}

is.gTree <- function(x) {
  inherits(x, "gTree")
}

is.nullGrob <- function(x) {
  if(grid::is.grob(x)) {
    inherits(x, "null")
  } else FALSE # it is not even a grob
}

select_color <- function() loon::l_getOption("select-color")
bounder_color <- function() loon::l_getOption("foreground")

log_ceiling <- function(x, base = 2) {
  x <- min(abs(x))
  10^(floor(log10(x + 1e-2)) - base)
}

col2hex <- function (cname)  {
  colMat <- grDevices::col2rgb(cname)
  grDevices::rgb(red = colMat[1, ]/255, green = colMat[2, ]/255, blue = colMat[3, ]/255)
}

remove_null <- function(..., as_list = TRUE) {
  if(as_list)
    Filter(Negate(is.null),
           list(...)
    )
  else
    Filter(Negate(is.null), ...)
}

# layout_coords
# layout_coords <- getFromNamespace("layout_coords", "loon.ggplot")
# layout_coords.l_facet_ggplot <- getFromNamespace("layout_coords.l_ggplot", "loon.ggplot")
# layout_coords.l_plot <- getFromNamespace("layout_coords.l_plot", "loon.ggplot")
# layout_coords.l_hist <- getFromNamespace("layout_coords.l_hist", "loon.ggplot")
layout_coords <- function(target) {
  UseMethod("layout_coords", target)
}

layout_coords.l_plot <- function(target) {
  ggLayout <- matrix(c(1,1), nrow = 1)
  colnames(ggLayout) <- c("row", "col")
  ggLayout
}

layout_coords.l_hist <- function(target) {
  ggLayout <- matrix(c(1,1), nrow = 1)
  colnames(ggLayout) <- c("row", "col")
  ggLayout
}

layout_coords.l_facet_ggplot <- function(target) {
  plots <- l_getPlots(target)
  ggLayout <- as.data.frame(
    t(sapply(strsplit(names(plots), split = ""),
             function(i){
               xpos <- which(i %in% "x" == TRUE)
               ypos <- which(i %in% "y" == TRUE)
               len_str <- length(i)
               c(as.numeric(paste0(i[(xpos + 1) : (ypos - 1)], collapse = "")),
                 as.numeric(paste0(i[(ypos + 1) : (len_str)], collapse = "")))
             })
    )
  )
  colnames(ggLayout) <- c("row", "col")
  ggLayout
}

get_unit <- function(x, unit = "native", is.unit = TRUE, as.numeric = FALSE) {

  if(length(x) == 0) return(numeric(0L))

  if(getRversion() >= "4.0.0") {

    y <- unclass(x)

    if(!is.list(y)) {
      if(as.numeric) return(as.numeric(x))
      return(x)
    }

    if(unit == "native" && is.unit) {

      unit.y <- y[[1]][[2]]
      unit.x <- grepl(unit, as.character(unit.y))

      u <- unit.y[unit.x]

    } else {
      for(i in seq(length(y))) {

        unit.y <- y[[i]][[2]]
        unit.x <- grepl(unit, as.character(unit.y))

        if(i == 1) {
          if(is.unit) {
            u <- unit.y[unit.x]
          } else {
            u <- unit.y[!unit.x]
          }
        } else {
          u <- if(is.unit) {
            unit.c(u, unit.y[unit.x])
          } else {
            unit.c(u, unit.y[!unit.x])
          }
        }
      }
    }

  } else {
    unit1 <- x[["arg1"]]
    unit2 <- x[["arg2"]]

    u <- if(is.unit) {
      if(grepl(unit, as.character(unit1)))
        unit1
      else
        unit2
    } else {
      if(grepl(unit, as.character(unit1)))
        unit2
      else
        unit1
    }
  }

  if(as.numeric) return(as.numeric(u))
  u
}

get_model_display_order <- utils::getFromNamespace("get_model_display_order", "loon")
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "input", "output", "session"))
