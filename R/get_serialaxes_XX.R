get_showAxes <- function(loon_grob) {
  guides_grob <- grid::getGrob(loon_grob, "guides")
  !any(stringr::str_detect(guides_grob$childrenOrder, "arguments"))
}


get_showAxesLabels <- function(loon_grob) {
  labels_grob <- grid::getGrob(loon_grob, "labels")
  !any(stringr::str_detect(labels_grob$childrenOrder, "arguments"))
}

get_axesLayout <- function(loon_grob) {

  axesLayout <- "radial"

  serialaxes_grob <- grid::getGrob(loon_grob, "l_serialaxes")

  if(!"radialAxes" %in% serialaxes_grob$childrenOrder) axesLayout <- "parallel"

  axesLayout
}

get_showArea <- function(loon_grob) {

  axes_grob <- grid::getGrob(loon_grob, paste0(get_axesLayout(loon_grob), "Axes"))
  any(stringr::str_detect(axes_grob$childrenOrder, "showArea"))
}


get_scaledActiveData <- function(axes_grob, axesLayout_in_loon, ...) {

  num_child <- length(axes_grob$childrenOrder)

  if(axesLayout_in_loon == "parallel") {

    lapply(1:num_child,
           function(i) {

             child <- axes_grob$children[[i]]
             dat <- c(child$y)
             if(stringr::str_detect(child$name, "showArea")) dat[1:(length(dat)/2)] else dat
           }
    )
  } else {

    args <- list(...)
    radius <- args$radius
    angle <- args$angle

    lapply(1:num_child,
           function(i) {

             child <- axes_grob$children[[i]]

             dat <- get_unit(child$x, "npc", as.numeric = TRUE)
             dat <- dat[-length(dat)]
             dat/(radius * cos(angle))
           }
    )
  }
}

get_default_serialaxes <- function(axesLayout) {
  
  if(axesLayout == "parallel") xscale <- yscale <- c(-0.10,  1.12) else xscale <- yscale <- c(-0.2, 1.2)
  
  list(
    xscale = xscale, 
    yscale = yscale
  )
}