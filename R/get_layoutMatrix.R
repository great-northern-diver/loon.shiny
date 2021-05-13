get_layout_matrix <- function(widgets, layout, nrow, ncol, ...) {
  UseMethod("get_layout_matrix", widgets)
}

get_layout_matrix.default <- function(widgets, layout, ...) {
  
  nrow <- max(layout$t, layout$b)
  ncol <- max(layout$l, layout$r)
  
  layout_matrix <- matrix(rep(NA, nrow*ncol), nrow = nrow, ncol = ncol)
  
  for(i in 1:length(layout$t)) {
    
    layout_i <- layout[i,]
    
    layout_matrix[layout_i$t:layout_i$b, layout_i$l:layout_i$r] <- layout_i$z
  }
  
  list(
    layout_matrix = layout_matrix,
    nrow = nrow,
    ncol = ncol
  )
}

get_layout_matrix.l_facet_ggplot <- function(widgets, layout, ...) {

  nrow <- max(layout$row)
  ncol <- max(layout$col)
  
  n <- dim(layout)[1]
  layout_matrix <- matrix(rep(NA, nrow*ncol), nrow = nrow, ncol = ncol)
  
  for(i in 1:n) {
    layout_matrix[layout[i,1], layout[i,2]] <- i
  }
  
  list(
    layout_matrix = layout_matrix,
    nrow = nrow,
    ncol = ncol
  )
}

get_layout_matrix.list <- function(widgets, layout, nrow, ncol, ...) {
 
  args <- list(...)
  layout_matrix <- args$layout_matrix 
  # number of widgets
  n <- length(layout)  
  # none of them are compound
  is_null_layout <- sapply(1:n, function(i) is.null(layout[[i]]))
  
  if(n == 1) {
    
    list(
      layout_matrix = layout[[1]],
      nrow = nrow,
      ncol = ncol
    )
    
  } else {

    if(!all(is_null_layout)) {
      
      if (is.null(nrow) && !is.null(ncol)) {
        nrow <- ceiling(n/ncol)
      }
      if (is.null(ncol) && !is.null(nrow)) {
        ncol <- ceiling(n/nrow)
      }
      stopifnot(nrow * ncol >= n)
      if (is.null(nrow) && is.null(ncol)) {
        nm <- grDevices::n2mfrow(n)
        nrow <- nm[1]
        ncol <- nm[2]
      }

      notShow <- lapply(1:n, 
                        function(i) {
                          if(i == 1) {
                            layout[[i]] <<- if(is.null(layout[[i]])) matrix(1, nrow = 1) else layout[[i]]
                          } else {
                            offset <- max(layout[[i-1]], na.rm = TRUE)
                            layout[[i]] <<- if(is.null(layout[[i]])) matrix(1 + offset, nrow = 1) else layout[[i]] + offset
                          }
                        }
      )
      
      # get Least common multiple
      for(i in 2:n) {
        
        dim_i <- dim(layout[[i]])
        
        if(i == 2) {
          dim_i_1 <-  dim(layout[[i-1]])
          
          lcm_row <- lcm(dim_i[1], dim_i_1[1])
          lcm_col <- lcm(dim_i[2], dim_i_1[2])
        } else {
          
          lcm_row <- lcm(lcm_row, dim_i[1])
          lcm_col <- lcm(lcm_col, dim_i[2])
        } 
      }

      # Kronecker Product
      lay <- lapply(1:n, 
                    function(i){
                      dim_i <- dim(layout[[i]])
                      row_i <- lcm_row/dim_i[1]
                      col_i <- lcm_col/dim_i[2]
                      kronecker(layout[[i]], matrix(rep(1, row_i * col_i), nrow = row_i))
                    }
      )

      if(is.null(layout_matrix)) {
      
        layout_matrix <- matrix(rep(NA, nrow * lcm_row * ncol * lcm_col), nrow =  nrow * lcm_row)
        
        count <- 0
        for(i in 1:nrow) {
          for(j in 1:ncol) {
            count <- count + 1
            layout_matrix[((i - 1) * lcm_row + 1):(i * lcm_row), ((j - 1) * lcm_col + 1):(j * lcm_col)] <- lay[[count]]
            if(count >= n) break
          }
        }
        
        nrow <- NULL
        ncol <- NULL

      } else {
        
        m <- max(layout_matrix, na.rm = T)
        if(m < n) warning("not all widgets can be displayed")

        dim_lm <- dim(layout_matrix)
        nrow <- dim_lm[1]
        ncol <- dim_lm[2]
        layout_matrix0 <- matrix(rep(NA, lcm_row * nrow * lcm_col * ncol), nrow = lcm_row * nrow)
        
        for(i in 1:nrow) {
          for(j in 1:ncol) {

            count <- layout_matrix[i,j]
            if(!is.na(count)) {
              layout_matrix0[((i - 1) * lcm_row + 1):(i * lcm_row), ((j - 1) * lcm_col + 1):(j * lcm_col)] <- lay[[count]]
            }
          }
        }
        
        layout_matrix <- layout_matrix0
        nrow <- NULL
        ncol <- NULL
      }
    }
    
    list(
      layout_matrix = layout_matrix,
      nrow = nrow,
      ncol = ncol
    )
  }
}


lcm <- function(x, y) {
  # choose the greater number
  if(x > y) {
    greater <- x
  } else {
    greater <- y
  }
  while(TRUE) {
    if((greater %% x == 0) && (greater %% y == 0)) {
      lcm <- greater
      break
    }
    greater <- greater + 1
  }
  return(lcm)
}
