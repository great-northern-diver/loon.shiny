get_brush_id <- function(loon_grob, coord, swap_in_shiny = FALSE, swap_in_loon = FALSE,
                         position, brush_info, vp, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("get_brush_id", obj)
}

get_brush_id.l_plot <- function(loon_grob, coord, swap_in_shiny = FALSE, swap_in_loon = FALSE,
                                position, brush_info, vp, click_info){

  if(length(coord$x) == 0 & length(coord$y) == 0) {
    
    numeric(0)
  
  } else {
    
    if(!is.null(brush_info)) {
      
      newbound <- coordConvert(position, brush_info, vp)
      newl <- newbound$newl
      newr <- newbound$newr
      newt <- newbound$newt
      newb <- newbound$newb

      xlim <- vp[[2]]$xscale
      ylim <- vp[[2]]$yscale

      if(swap_in_loon) {
        if(swap_in_shiny) {

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
        if(swap_in_shiny) {

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
        which(xx >= brush_info$xmin & xx <= brush_info$xmax),
        which(yy >= brush_info$ymin & yy <= brush_info$ymax)
      )
    } else if (!is.null(click_info)) {
      
      newbound <- coordConvert(position, click_info, vp)
      newl <- newbound$newl
      newr <- newbound$newr
      newt <- newbound$newt
      newb <- newbound$newb
      
      xlim <- vp[[2]]$xscale
      ylim <- vp[[2]]$yscale
      
      if(swap_in_loon) {
        if(swap_in_shiny) {
          
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
        if(swap_in_shiny) {
          
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
        which(xx >= (click_info$x - click_selection_adjustment()) & xx <= (click_info$x + click_selection_adjustment())),
        which(yy >= (click_info$y - click_selection_adjustment()) & yy <= (click_info$y + click_selection_adjustment()))
      )
    } else numeric(0)
  }
}

get_brush_id.l_hist <- function(loon_grob, coord, swap_in_shiny = FALSE, swap_in_loon = FALSE,
                                position, brush_info, vp, click_info) {
  
  if(!is.null(brush_info)) {

    newbound <- coordConvert(position, brush_info, vp)
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
    
    if(swap_in_shiny) {
      
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
        (xmin <= brush_info$xmin & brush_info$xmin <= xmax) |
          (xmin <= brush_info$xmax & brush_info$xmax <= xmax) |
          (brush_info$xmin <= xmin & xmax <= brush_info$xmax)
      ),
      which(
        (ymin <= brush_info$ymin & brush_info$ymin <= ymax) |
          (ymin <= brush_info$ymax & brush_info$ymax <= ymax) |
          (brush_info$ymin <= ymin & ymax <= brush_info$ymax)
      )
    )

    binId2brushId(binNames = coord$binNames, 
                  binId_group = coord$binId_group, 
                  brush_bin = brush_bin)
    
  } else if(!is.null(click_info)) {
    
    newbound <- coordConvert(position, click_info, vp)
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
    
    if(swap_in_shiny) {
      
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
      which(xmin <= click_info$x & click_info$x <= xmax),
      which(ymin <= click_info$y & click_info$y <= ymax)
    )
    
    binId2brushId(binNames = coord$binNames, 
                  binId_group = coord$binId_group, 
                  brush_bin = brush_bin)
    
  } else integer(0)
}

get_brush_id.l_graph <- function(loon_grob, coord, swap_in_shiny = FALSE, swap_in_loon = FALSE,
                                 position, brush_info, vp, click_info){
  
  get_brush_id.l_plot(loon_grob, coord, swap_in_shiny, swap_in_loon,
                      position, brush_info, vp, click_info)
}

get_brush_id.l_serialaxes <- function(loon_grob, coord, swap_in_shiny = FALSE, swap_in_loon = FALSE,
                                      position, brush_info, vp, axesLayout_in_shiny) {

  if(!is.null(brush_info)) {
    
    newbound <- coordConvert(position, brush_info, vp)
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
    
    brush_id <- c()

    # to make the `convert` function work properly
    grid::pushViewport(vp)
    
    lapply(seq(length(x)),
           function(i) {

             x_i <- convertX(x[[i]], unitTo = "native", TRUE)
             y_i <- convertY(y[[i]], unitTo = "native", TRUE)

             newxy <- homo_trans(r, l, t, b, newr, newl, newt, newb, x_i, y_i)
             xx <- extendPoints(newxy$x, length.out = 100)
             yy <- extendPoints(newxy$y, length.out = 100)

             intersectX <- intersect(which(xx >= brush_info$xmin), which(xx <= brush_info$xmax))
             intersectY <- intersect(which(yy >= brush_info$ymin), which(yy <= brush_info$ymax))

             if(length(intersect(intersectX, intersectY)) > 0) brush_id[i] <<- i
           }
    )

    brush_id <- brush_id[which(!is.na(brush_id))]

  } else numeric(0)
}

coordConvert <- function(position, brush_info, vp) {

  l <- brush_info$domain$left
  r <- brush_info$domain$right
  b <- brush_info$domain$bottom
  t <- brush_info$domain$top

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
  
  # plotViewport
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