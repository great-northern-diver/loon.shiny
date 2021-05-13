get_grobFromGtable <- function(l_className, gtable, widgets) {
  class(l_className) <- l_className
  UseMethod("get_grobFromGtable", l_className)
}

get_grobFromGtable.l_facet_ggplot <- function(l_className, gtable, widgets) {
  
  grobs <- if(is.gtable(gtable)) gtable$grobs else 
    stop("It is not a `gtable` object", call. = FALSE)
  
  wNames <- sapply(widgets, as.character)
  
  gs <- lapply(grobs, 
               function(grob) {
                 if(is.gtable(grob)) {
                   g <- grob$grobs
                   g[names(g) %in% wNames]
                 } else NULL
               })
  
  unlist(gs, recursive = FALSE)
}

get_grobFromGtable.l_facet_wrap <- function(l_className, gtable, widgets) {
  
  grobs <- if(is.gtable(gtable)) gtable$grobs else 
    stop("It is not a `gtable` object", call. = FALSE)
  
  wNames <- sapply(widgets, as.character)
  
  gs <- lapply(grobs, 
               function(grob) {
                 if(is.gtable(grob)) {
                   g <- grob$grobs
                   g[names(g) %in% wNames]
                 } else NULL
               })
  
  unlist(gs, recursive = FALSE)
}

get_grobFromGtable.l_facet_grid <- function(l_className, gtable, widgets) {
  
  grobs <- if(is.gtable(gtable)) gtable$grobs else 
    stop("It is not a `gtable` object", call. = FALSE)
  
  wNames <- sapply(widgets, as.character)
  gs <- lapply(grobs, 
               function(grob) {
                 if(is.gtable(grob)) {
                   g <- grob$grobs
                   namesG <- vapply(g, function(x) x$name, character(1L))
                   g[namesG %in% wNames]
                 } else NULL
               })
  
  unlist(gs, recursive = FALSE)
}

is.gtable <- function (x) {
  inherits(x, "gtable")
}
