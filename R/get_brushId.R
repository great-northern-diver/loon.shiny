get_brushId <- function(loon.grob, coord, swapInShiny = FALSE, swapInLoon = FALSE,
                         position, brushInfo, vp, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("get_brushId", obj)
}

get_brushId.l_plot <- function(loon.grob, coord, swapInShiny = FALSE, swapInLoon = FALSE,
                                position, brushInfo, vp, clickInfo){

  if(length(coord$x) == 0 & length(coord$y) == 0) {

    numeric(0)

  } else {

    if(!is.null(brushInfo)) {

      newbound <- coordConvert(position, brushInfo, vp)
      newl <- newbound$newl
      newr <- newbound$newr
      newt <- newbound$newt
      newb <- newbound$newb

      xlim <- vp[[2]]$xscale
      ylim <- vp[[2]]$yscale

      if(swapInLoon) {
        if(swapInShiny) {

          l <- ylim[1]
          r <- ylim[2]
          b <- xlim[1]
          t <- xlim[2]

          x <- coord$x
          y <- coord$y
        } else {
          l <- xlim[1]
          r <- xlim[2]
          b <- ylim[1]
          t <- ylim[2]

          x <- coord$y
          y <- coord$x
        }
        newxy <- homo_trans(t, b, r, l, newr, newl, newt, newb, x, y)
      } else {
        if(swapInShiny) {

          l <- ylim[1]
          r <- ylim[2]
          b <- xlim[1]
          t <- xlim[2]

          x <- coord$y
          y <- coord$x
        } else {
          l <- xlim[1]
          r <- xlim[2]
          b <- ylim[1]
          t <- ylim[2]

          x <- coord$x
          y <- coord$y
        }
        newxy <- homo_trans(r, l, t, b, newr, newl, newt, newb, x, y)
      }

      xx <- newxy$x
      yy <- newxy$y

      intersect(
        which(xx >= brushInfo$xmin & xx <= brushInfo$xmax),
        which(yy >= brushInfo$ymin & yy <= brushInfo$ymax)
      )
    } else if (!is.null(clickInfo)) {

      newbound <- coordConvert(position, clickInfo, vp)
      newl <- newbound$newl
      newr <- newbound$newr
      newt <- newbound$newt
      newb <- newbound$newb

      xlim <- vp[[2]]$xscale
      ylim <- vp[[2]]$yscale

      if(swapInLoon) {
        if(swapInShiny) {

          l <- ylim[1]
          r <- ylim[2]
          b <- xlim[1]
          t <- xlim[2]

          x <- coord$x
          y <- coord$y
        } else {
          l <- xlim[1]
          r <- xlim[2]
          b <- ylim[1]
          t <- ylim[2]

          x <- coord$y
          y <- coord$x
        }
        newxy <- homo_trans(t, b, r, l, newr, newl, newt, newb, x, y)
      } else {
        if(swapInShiny) {

          l <- ylim[1]
          r <- ylim[2]
          b <- xlim[1]
          t <- xlim[2]

          x <- coord$y
          y <- coord$x
        } else {
          l <- xlim[1]
          r <- xlim[2]
          b <- ylim[1]
          t <- ylim[2]

          x <- coord$x
          y <- coord$y
        }
        newxy <- homo_trans(r, l, t, b, newr, newl, newt, newb, x, y)
      }

      xx <- newxy$x
      yy <- newxy$y

      intersect(
        which(xx >= (clickInfo$x - click_selection_adjustment()) & xx <= (clickInfo$x + click_selection_adjustment())),
        which(yy >= (clickInfo$y - click_selection_adjustment()) & yy <= (clickInfo$y + click_selection_adjustment()))
      )
    } else numeric(0)
  }
}

get_brushId.l_hist <- function(loon.grob, coord, swapInShiny = FALSE, swapInLoon = FALSE,
                                position, brushInfo, vp, clickInfo) {

  if(!is.null(brushInfo)) {

    newbound <- coordConvert(position, brushInfo, vp)
    newl <- newbound$newl
    newr <- newbound$newr
    newt <- newbound$newt
    newb <- newbound$newb

    # vp has been rotated, no need to swap xlim, ylim
    xlim <- vp[[2]]$xscale
    ylim <- vp[[2]]$yscale

    l <- xlim[1]
    r <- xlim[2]
    b <- ylim[1]
    t <- ylim[2]

    if(swapInShiny) {

      xmax <- coord$ymax
      xmin <- coord$ymin
      ymax <- coord$xmax
      ymin <- coord$xmin

      newxy_min <- homo_trans(r, l, t, b, newr, newl, newt, newb, xmin, ymin)
      newxy_max <- homo_trans(r, l, t, b, newr, newl, newt, newb, xmax, ymax)

      xmin <- newxy_min$x
      xmax <- newxy_max$x
      ymin <- newxy_min$y
      ymax <- newxy_max$y

    } else {

      xmax <- coord$xmax
      xmin <- coord$xmin
      ymax <- coord$ymax
      ymin <- coord$ymin

      newxy_min <- homo_trans(r, l, t, b, newr, newl, newt, newb, xmin, ymin)
      newxy_max <- homo_trans(r, l, t, b, newr, newl, newt, newb, xmax, ymax)

      xmin <- newxy_min$x
      xmax <- newxy_max$x
      ymin <- newxy_min$y
      ymax <- newxy_max$y
    }

    brush_bin <- intersect(
      which(
        (xmin <= brushInfo$xmin & brushInfo$xmin <= xmax) |
          (xmin <= brushInfo$xmax & brushInfo$xmax <= xmax) |
          (brushInfo$xmin <= xmin & xmax <= brushInfo$xmax)
      ),
      which(
        (ymin <= brushInfo$ymin & brushInfo$ymin <= ymax) |
          (ymin <= brushInfo$ymax & brushInfo$ymax <= ymax) |
          (brushInfo$ymin <= ymin & ymax <= brushInfo$ymax)
      )
    )

    binId2brushId(binNames = coord$binNames,
                  binId_group = coord$binId_group,
                  brush_bin = brush_bin)

  } else if(!is.null(clickInfo)) {

    newbound <- coordConvert(position, clickInfo, vp)
    newl <- newbound$newl
    newr <- newbound$newr
    newt <- newbound$newt
    newb <- newbound$newb

    # vp has been rotated, no need to swap xlim, ylim
    xlim <- vp[[2]]$xscale
    ylim <- vp[[2]]$yscale

    l <- xlim[1]
    r <- xlim[2]
    b <- ylim[1]
    t <- ylim[2]

    if(swapInShiny) {

      xmax <- coord$ymax
      xmin <- coord$ymin
      ymax <- coord$xmax
      ymin <- coord$xmin

      newxy_min <- homo_trans(r, l, t, b, newr, newl, newt, newb, xmin, ymin)
      newxy_max <- homo_trans(r, l, t, b, newr, newl, newt, newb, xmax, ymax)

      xmin <- newxy_min$x
      xmax <- newxy_max$x
      ymin <- newxy_min$y
      ymax <- newxy_max$y

    } else {

      xmax <- coord$xmax
      xmin <- coord$xmin
      ymax <- coord$ymax
      ymin <- coord$ymin

      newxy_min <- homo_trans(r, l, t, b, newr, newl, newt, newb, xmin, ymin)
      newxy_max <- homo_trans(r, l, t, b, newr, newl, newt, newb, xmax, ymax)

      xmin <- newxy_min$x
      xmax <- newxy_max$x
      ymin <- newxy_min$y
      ymax <- newxy_max$y
    }

    brush_bin <- intersect(
      which(xmin <= clickInfo$x & clickInfo$x <= xmax),
      which(ymin <= clickInfo$y & clickInfo$y <= ymax)
    )

    binId2brushId(binNames = coord$binNames,
                  binId_group = coord$binId_group,
                  brush_bin = brush_bin)

  } else integer(0)
}

get_brushId.l_graph <- function(loon.grob, coord, swapInShiny = FALSE, swapInLoon = FALSE,
                                 position, brushInfo, vp, clickInfo){

  get_brushId.l_plot(loon.grob, coord, swapInShiny, swapInLoon,
                      position, brushInfo, vp, clickInfo)
}

get_brushId.l_serialaxes <- function(loon.grob, coord, swapInShiny = FALSE, swapInLoon = FALSE,
                                      position, brushInfo, vp, axesLayoutInShiny) {

  if(!is.null(brushInfo)) {

    newbound <- coordConvert(position, brushInfo, vp)
    newl <- newbound$newl
    newr <- newbound$newr
    newt <- newbound$newt
    newb <- newbound$newb

    xlim <- vp[[2]]$xscale
    ylim <- vp[[2]]$yscale

    l <- xlim[1]
    r <- xlim[2]
    b <- ylim[1]
    t <- ylim[2]

    x <- coord$x
    y <- coord$y

    brushId <- c()

    # to make the `convert` function work properly
    grid::pushViewport(vp)

    lapply(seq(length(x)),
           function(i) {

             x_i <- convertX(x[[i]], unitTo = "native", TRUE)
             y_i <- convertY(y[[i]], unitTo = "native", TRUE)

             newxy <- homo_trans(r, l, t, b, newr, newl, newt, newb, x_i, y_i)
             xx <- extendPoints(newxy$x, length.out = 100)
             yy <- extendPoints(newxy$y, length.out = 100)

             intersectX <- intersect(which(xx >= brushInfo$xmin), which(xx <= brushInfo$xmax))
             intersectY <- intersect(which(yy >= brushInfo$ymin), which(yy <= brushInfo$ymax))

             if(length(intersect(intersectX, intersectY)) > 0) brushId[i] <<- i
           }
    )

    brushId <- brushId[which(!is.na(brushId))]

  } else numeric(0)
}

coordConvert <- function(position, brushInfo, vp) {

  l <- brushInfo$domain$left
  r <- brushInfo$domain$right
  b <- brushInfo$domain$bottom
  t <- brushInfo$domain$top

  offset <- get_offset(vp, t, b, l, r)
  t_offset <- offset$t_offset
  b_offset <- offset$b_offset
  l_offset <- offset$l_offset
  r_offset <- offset$r_offset

  domain_l <- l + (r - l) * position$l
  domain_r <- l + (r - l) * position$r
  domain_t <- t - (t - b) * position$t
  domain_b <- t - (t - b) * position$b

  list(
    newl = domain_l + l_offset,
    newr = domain_r - r_offset,
    newb = domain_b + b_offset,
    newt = domain_t - t_offset
  )
}

offset_unit <- function(x, unit = "native", is.unit = TRUE, as.numeric = FALSE) {
  if(getRversion() >= "4.0.0") {
    -get_unit(x, unit, is.unit, as.numeric)
  } else {
    get_unit(x, unit, is.unit, as.numeric)
  }
}

get_offset <- function(vp, t, b, l, r) {

  # grid::plotViewport
  plot_viewport <- vp[[1]]

  l_offset <- plot_viewport$x
  b_offset <- plot_viewport$y

  width <- plot_viewport$width
  r_offset <- offset_unit(width, "lines", as.numeric = FALSE) - l_offset

  height <- plot_viewport$height
  t_offset <- offset_unit(height, "lines", as.numeric = FALSE) - b_offset

  list(
    l_offset = convertX(l_offset, unitTo = "npc", TRUE) * (r - l),
    r_offset = convertX(r_offset, unitTo = "npc", TRUE) * (r - l),
    t_offset = convertY(t_offset, unitTo = "npc", TRUE) * (t - b),
    b_offset = convertY(b_offset, unitTo = "npc", TRUE) * (t - b)
  )
}

homo_trans <- function(r, l, t, b, newr, newl, newt, newb, x, y) {
  ax <- (newr - newl)/(r - l)
  bx <- (newl * r - newr * l)/(r - l)

  ay <- (newt - newb)/(t - b)
  by <- (newb * t - newt * b)/(t - b)

  # homogeneous coordinates
  homo_trans <- matrix(c(ax, 0, bx, 0, ay, by, 0, 0, 1), nrow = 3, byrow = T)
  coordinates <- matrix(c(x, y, rep(1, length(x))), nrow = 3, byrow = T)
  mapping_coordinates <- homo_trans %*% coordinates

  newx <- mapping_coordinates[1, ]
  newy <- mapping_coordinates[2, ]

  list(
    x = newx,
    y = newy
  )
}

extendPoints <- function(x, length.out = 100) {
  unlist(
    lapply(1:(length(x) - 1),
           function(i){
             seq(x[i], x[i+1], length.out = length.out)
           }
    )
  )
}

binId2brushId <- function(binNames, binId_group, brush_bin) {

  if(length(brush_bin) != 0) {
    unlist(
      lapply(brush_bin,
             function(b){
               which_bin <- which(binNames %in% binNames[b])
               this_bin_id <- which(which_bin == b)
               if(binNames[b] < 0) {
                 # gap is selected
                 integer(0)
               } else {

                 sort(binId_group[[binNames[b]]][[this_bin_id]])
               }
             }
      )
    )
  } else integer(0)
}
