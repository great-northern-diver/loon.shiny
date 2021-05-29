get_showAxes <- function(loon.grob) {
  guidesGrob <- grid::getGrob(loon.grob, "guides")
  !any(grepl(guidesGrob$childrenOrder, pattern = "arguments"))
}


get_showAxesLabels <- function(loon.grob) {
  labelsGrob <- grid::getGrob(loon.grob, "labels")
  !any(grepl(labelsGrob$childrenOrder, pattern = "arguments"))
}

get_axesLayout <- function(loon.grob) {

  axesLayout <- "radial"

  serialaxesGrob <- grid::getGrob(loon.grob, "l_serialaxes")

  if(!"radialAxes" %in% serialaxesGrob$childrenOrder) axesLayout <- "parallel"

  axesLayout
}

get_showArea <- function(loon.grob) {

  axesGrob <- grid::getGrob(loon.grob, paste0(get_axesLayout(loon.grob), "Axes"))
  any(grepl(axesGrob$childrenOrder, pattern = "showArea"))
}


get_scaledActiveData <- function(axesGrob, axesLayoutInLoon, ...) {

  num_child <- length(axesGrob$childrenOrder)

  if(axesLayoutInLoon == "parallel") {

    lapply(1:num_child,
           function(i) {

             child <- axesGrob$children[[i]]
             dat <- c(child$y)
             if(grepl(child$name, pattern = "showArea")) dat[1:(length(dat)/2)] else dat
           }
    )
  } else {

    args <- list(...)
    radius <- args$radius
    angle <- args$angle

    lapply(1:num_child,
           function(i) {

             child <- axesGrob$children[[i]]

             dat <- get_unit(child$x, "npc", as.numeric = TRUE)
             dat <- dat[-length(dat)]
             dat/(radius * cos(angle))
           }
    )
  }
}

get_defaultSerialaxesSettings <- function(axesLayout) {

  if(axesLayout == "parallel") xscale <- yscale <- c(-0.10,  1.12) else xscale <- yscale <- c(-0.2, 1.2)

  list(
    xscale = xscale,
    yscale = yscale
  )
}
