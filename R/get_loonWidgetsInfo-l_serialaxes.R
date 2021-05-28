get_loonWidgetsInfo.l_serialaxes <- function(widgets, loon.grobs, ...) {

  args <- list(...)
  navbarMenuName <- args$navbarMenuName

  loon.grob <- loon.grobs
  axesLayout <- get_axesLayout(loon.grob)
  axesGrob <- grid::getGrob(loon.grob, paste0(axesLayout, "Axes"))
  N <- length(axesGrob$children)
  # area
  showArea <- get_showArea(loon.grob)

  viewPort <- get_viewPort(loon.grob)
  xlim <- viewPort[[2]]$xscale
  ylim <- viewPort[[2]]$yscale

  display_order <- get_display_order(widgets)

  linewidth <- index <- c()
  x <- y <- list()
  N <- length(axesGrob$children)

  lapply(1:N,
         function(i){

           child <- axesGrob$children[[i]]

           if(is(child,  "null")) {

             x[[i]] <<- NA
             y[[i]] <<- NA
             linewidth[i] <<- NA
           } else {

             x[[i]] <<- child$x
             y[[i]] <<- child$y
             linewidth[i] <<- if(showArea) NA else child$gp$lwd
           }
           index[i] <<- as.numeric(gsub("\\D", "", child$name))
         }
  )

  x <- x[display_order]
  y <- y[display_order]
  linewidth <- linewidth[display_order]
  index <- index[display_order]

  labelsGrob <- grid::getGrob(loon.grob, "axesLabels")
  lenSeqName <- length(labelsGrob$childrenOrder)
  seqName <- vapply(seq(lenSeqName),
                    function(i){
                      labelsGrob$children[[i]]$label
                    }, character(1L)
  )

  dat <- char2num.data.frame(widgets['data'])   # convert to numeric
  activeData <- dat[, seqName]

  if(is.null(activeData)) {

    variableScaledActiveData <- NULL
    observationScaledActiveData <- NULL
    dataScaledActiveData <- NULL
    noneScaledActiveData <- NULL
  } else {

    apply2min <- apply(dat, 2, "min")
    apply2max <- apply(dat, 2, "max")
    apply1min <- apply(activeData, 1, "min")
    apply1max <- apply(activeData, 1, "max")
    minD <- min(dat)
    maxD <- max(dat)

    variableScaledActiveData <- t(
      (t(activeData) - apply2min)/
        (apply2max  - apply2min)
    )

    observationScaledActiveData <- (activeData - apply1min) / (apply1max - apply1min)

    dataScaledActiveData <- (activeData - minD)/ (maxD - minD)
    noneScaledActiveData <- activeData
  }

  andrewsSeriesLength <- widgets['andrewsSeriesLength']

  list(
    itemLabel = widgets['itemLabel'],
    showItemLabels = widgets['showItemLabels'],
    showAxes = widgets['showAxes'],
    andrews = widgets['andrews'],
    showAxesLabels = widgets['showAxesLabels'],
    axesLayout = widgets['axesLayout'],
    showArea = widgets['showArea'],
    showGuides = widgets['showGuides'],
    showLabels = widgets['showLabels'],
    linkingGroup = widgets['linkingGroup'],
    linkingKey = widgets['linkingKey'],
    color = hex12tohex6(widgets['color']),
    active = widgets['active'],
    selected = widgets['selected'],
    title = widgets['title'],
    scaling = widgets['scaling'],
    N = N,
    labels = list(
      title = if(is.null(args$title)) widgets['title'] else args$title
    ),
    andrewsSeriesLength = andrewsSeriesLength,
    fourierTrans = andrews(k = lenSeqName, length.out = andrewsSeriesLength),
    linkingStates = loon::l_getLinkedStates(widgets),
    x = x,
    y = y,
    size = linewidth,
    index = index,
    lenSeqName = lenSeqName,
    seqName = seqName,
    xlim = xlim,
    ylim = ylim,
    N = N,
    variableScaledActiveData = variableScaledActiveData,
    observationScaledActiveData = observationScaledActiveData,
    dataScaledActiveData = dataScaledActiveData,
    noneScaledActiveData = noneScaledActiveData,
    display_order = display_order,
    navbarMenuName = navbarMenuName,
    lastSelection = integer(0),
    loonColor = list(
      background_color = loon::l_getOption("background"),
      foreground_color = loon::l_getOption("foreground"),
      guidesbackground_color = loon::l_getOption("guidesBackground"),
      guideslines_color = loon::l_getOption("guidelines")
    )
  )
}

andrews <- function(k = 4,
                    length.out = 50 * (k - 1),
                    ...) {

  stopifnot(
    {
      is.numeric(length.out)
      is.numeric(k)
    }
  )

  k <- as.integer(k)
  length.out <- as.integer(length.out)

  t <- seq(-base::pi, base::pi, length.out = length.out)

  values <- sapply(seq(k),
                   function(i) {
                     if(i == 1) return(rep(1/sqrt(2), length.out))
                     fun <- if((i %% 2) == 0) {
                       # even
                       base::sin
                     } else {
                       # odd
                       base::cos
                     }

                     fun(2^(floor(i/2) - 1) * t)
                   })
  # return a list
  # with defined period and matrix
  list(
    series = t,
    matrix = matrix(values, nrow = k, byrow = TRUE)
  )
}
