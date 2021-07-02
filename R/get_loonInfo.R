get_loonInfo <- function(widgets, layout_matrix, nrow, ncol, navbarMenuName = NULL) {
  UseMethod("get_loonInfo", widgets)
}

get_loonInfo.default <- function(widgets, layout_matrix, nrow, ncol, navbarMenuName = NULL) {
  stop("The class of widgets are `", paste(class(widgets), collapse = ","),
       "` that is not recognized", call. = FALSE)
}

get_loonInfo.loon <- function(widgets, layout_matrix, nrow, ncol, navbarMenuName = NULL) {

  if(missing(layout_matrix)) layout_matrix <- NULL
  navbarMenuName <- navbarMenuName %||% navbar_menu_name(widgets)

  loon.grobs <- loon::loonGrob(widgets)
  names <- loonGrob_name(loon.grobs)

  loonWidgetsInfo <- get_loonWidgetsInfo(widgets,
                                           loon.grobs = loon.grobs,
                                           navbarMenuName = navbarMenuName)

  list(
    loon.grobs =  setNames(list(loon.grobs), names),
    loonWidgetsInfo = setNames(list(loonWidgetsInfo), names),
    layout_matrix = layout_matrix,
    nrow = nrow,
    ncol = ncol
  )
}

get_loonInfo.l_facet_ggplot <- function(widgets, layout_matrix, nrow, ncol,
                                  navbarMenuName = NULL) {

  if(missing(layout_matrix)) layout_matrix <- NULL
  l_className <- "l_facet_ggplot"

  plots <- l_getPlots(widgets)
  og <- loon::loonGrob(widgets)
  gtable <- grid::getGrob(og, l_className)

  loon.grobs <- get_grobFromGtable(l_className, gtable, widgets)
  layout <- l_getLocations(widgets)

  info <- get_compoundInfo(widgets = widgets, loon.grobs = loon.grobs,
                           layout = layout,
                           navbarMenuName = navbarMenuName)
  info$gtable <- gtable
  info
}

get_loonInfo.l_facet_wrap <- function(widgets, layout_matrix, nrow, ncol,
                                      navbarMenuName = NULL) {

  if(missing(layout_matrix)) layout_matrix <- NULL
  l_className <- "l_facet_wrap"

  gtable <- grid::getGrob(loon::loonGrob(widgets), l_className)
  loon.grobs <- get_grobFromGtable(l_className, gtable, widgets)
  layout <- l_getLocations(widgets)

  info <- get_compoundInfo(widgets = widgets,
                           loon.grobs = loon.grobs,
                           layout = layout,
                           navbarMenuName = navbarMenuName)
  info$gtable <- gtable
  info
}

get_loonInfo.l_facet_grid <- function(widgets, layout_matrix, nrow, ncol,
                                      navbarMenuName = NULL) {

  if(missing(layout_matrix)) layout_matrix <- NULL
  l_className <- "l_facet_grid"

  gtable <- grid::getGrob(loon::loonGrob(widgets), l_className)
  loon.grobs <- get_grobFromGtable(l_className, gtable, widgets)
  names(loon.grobs) <- vapply(loon.grobs, function(x) x$name, character(1L))

  layout <-  l_getLocations(widgets)

  info <- get_compoundInfo(widgets = widgets,
                           loon.grobs = loon.grobs,
                           layout = layout,
                           navbarMenuName = navbarMenuName)
  info$gtable <- gtable
  info
}


get_loonInfo.l_compound <- function(widgets, layout_matrix, nrow, ncol,
                                    navbarMenuName = NULL) {

  if(missing(layout_matrix)) layout_matrix <- NULL
  l_className <- class(widgets)[1L]

  gtable <- grid::getGrob(loon::loonGrob(widgets), l_className)
  loon.grobs <- gtable$grobs
  layout <- get_layout_matrix(widgets, gtable$layout, nrow, ncol)

  get_compoundInfo(widgets = widgets, loon.grobs = loon.grobs,
                   layout = layout,
                   navbarMenuName = navbarMenuName)
}

get_loonInfo.list <- function(widgets, layout_matrix, nrow, ncol,
                              navbarMenuName = NULL) {

  if(missing(layout_matrix)) layout_matrix <- NULL

  n <- length(widgets)
  navbarMenuNames <- names(widgets)
  duplicate <- if(!is.null(navbarMenuNames)) {
    if(any(duplicated(names(widgets)))) TRUE else FALSE
  } else TRUE

  get_navbarMenuName <- function(widget, navbarMenuName, i) {
    if(is.null(navbarMenuName)) {
      paste0(loonWidget_name(widget), i)
    } else {
      if(navbarMenuName == "") {
        paste0(loonWidget_name(widget), i)
      } else {
        if(duplicate) {
          paste0(navbarMenuName, i)
        } else navbarMenuName
      }
    }
  }

  loonInfos <- lapply(seq(n),
                      function(i) {

                        widget <- widgets[[i]]
                        navbarMenuName <- navbarMenuNames[i]

                        get_loonInfo(widget,
                                     layout_matrix = NULL,
                                     nrow = NULL,
                                     ncol = NULL,
                                     navbarMenuName = get_navbarMenuName(widget, navbarMenuName, i)
                        )
                      }
  )

  loon.grobs <- unlist(
    lapply(seq(n), function(i) loonInfos[[i]]$loon.grobs),
    recursive=FALSE
  )

  loonWidgetsInfo <- unlist(
    lapply(seq(n), function(i) loonInfos[[i]]$loonWidgetsInfo),
    recursive=FALSE
  )

  layout <- get_layout_matrix(widgets,
                              layout = lapply(seq(n),
                                              function(i)
                                                loonInfos[[i]]$layout_matrix),
                              nrow = nrow,
                              ncol = ncol,
                              layout_matrix = layout_matrix)


  m <- length(loon.grobs)
  navbarMenuNames <- sapply(seq(m), function(i) loonWidgetsInfo[[i]]$navbarMenuName)
  is_duplicated <- sapply(navbarMenuNames,
                          function(navbarMenuName) {
                            if(length(which(navbarMenuName == navbarMenuNames)) == 1) FALSE else TRUE
                          })

  names <- sapply(seq(m),
                  function(i) {
                    if(is_duplicated[i]) paste0(loonGrob_name(loon.grobs[[i]]), i) else navbarMenuNames[i]
                  })

  names(loonWidgetsInfo) <- names(loon.grobs) <- names

  list(
    loon.grobs = loon.grobs,
    loonWidgetsInfo = loonWidgetsInfo,
    layout_matrix = layout$layout_matrix,
    nrow = layout$nrow,
    ncol = layout$ncol
  )
}

get_compoundInfo <- function(widgets, loon.grobs, layout, navbarMenuName = NULL) {

  navbarMenuName <- navbarMenuName %||% navbar_menu_name(widgets)

  loonWidgetsInfo <- extend_loonWidgetsInfo(loonWidgetsInfo =
                                                get_loonWidgetsInfo(widgets,
                                                                     loon.grobs = loon.grobs,
                                                                     navbarMenuName = navbarMenuName),
                                              loon.grobs,
                                              navbarMenuName = navbarMenuName)
  names <- sapply(seq(length(loon.grobs)),
                  function(i)
                    paste0(loonGrob_name(loon.grobs[[i]]), i))
  names(loonWidgetsInfo) <- names(loon.grobs) <- names

  list(
    loon.grobs = loon.grobs,
    loonWidgetsInfo = loonWidgetsInfo,
    layout_matrix = layout$layout_matrix,
    nrow = layout$nrow,
    ncol = layout$ncol
  )
}


navbar_menu_name <- function(x) {

  if(inherits(x, "l_plot")) {
    "Scatterplot"
  } else if(inherits(x, "l_hist")) {
    "Histogram"
  } else if(inherits(x, "l_graph")) {
    "Graph"
  } else if(inherits(x, "l_serialaxes")) {
    "Serialaxes"
  } else if(inherits(x, "l_facet_ggplot")) {
    "Ggplot"
  } else if(inherits(x, "l_ts")) {
    "Time series"
  } else if(inherits(x, "l_facet")) {
    "Facet"
  } else if(inherits(x, "l_pairs")) {
    "Pairs"
  } else if (inherits(x, "l_compound")) {
    "Compound"
  } else {
    "List"
  }
}
