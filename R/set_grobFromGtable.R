set_grobFromGtable <- function(gtable, newGrobs, arrangeGrobArgs) {
  l_className <- gtable$name
  class(l_className) <- l_className
  UseMethod("set_grobFromGtable", l_className)
}


set_grobFromGtable.default <- function(gtable, newGrobs, arrangeGrobArgs) {
  
  arrangeGrobArgs$grobs <- newGrobs
  
  grid::gTree(
    children = grid::gList(
      grid::rectGrob(gp  = grid::gpar(fill = compoundbackground_color(),
                                      col = NA),
                     name = "bounding box"),
      do.call(gridExtra::arrangeGrob, arrangeGrobArgs)
    ), name = "l_shiny"
  )
}

set_grobFromGtable.l_facet_ggplot <- function(gtable, newGrobs, arrangeGrobArgs) {
  
  namesNewGrobs <- vapply(newGrobs, function(ng) ng$name, character(1L))
  
  if(!is.gtable(gtable)) return(set_grobFromGtable.default(gtable, newGrobs, arrangeGrobArgs))
  
  grobs <- gtable$grobs
  len <- length(grobs)
  
  if(len == 0) return(set_grobFromGtable.default(gtable, newGrobs, arrangeGrobArgs)) 
  
  for(i in seq(len)) {
    
    grob <- grobs[[i]]
    
    if(is.gtable(grob)) {
      
      childGrobs <- grob$grobs
      
      grob$grobs <- lapply(childGrobs, 
                           function(cg) {
                             ith <- which(namesNewGrobs %in% cg$name)
                             if(length(ith) > 0) {
                               newGrobs[[ith]]
                             } else cg
                           })
      
    } else NULL
    
    grobs[[i]] <- grob
  }
  
  gtable$grobs <- grobs 
  return(gtable)
}

set_grobFromGtable.l_facet_wrap <- function(gtable, newGrobs, arrangeGrobArgs) {
  
  namesNewGrobs <- vapply(newGrobs, function(ng) ng$name, character(1L))
  
  if(!is.gtable(gtable)) return(set_grobFromGtable.default(gtable, newGrobs, arrangeGrobArgs))
  
  grobs <- gtable$grobs
  len <- length(grobs)
  
  if(len == 0) return(set_grobFromGtable.default(gtable, newGrobs, arrangeGrobArgs)) 
  
  for(i in seq(len)) {
    
    grob <- grobs[[i]]
    
    if(is.gtable(grob)) {
      
      childGrobs <- grob$grobs
      
      grob$grobs <- lapply(childGrobs, 
                           function(cg) {
                             ith <- which(namesNewGrobs %in% cg$name)
                             if(length(ith) > 0) {
                               newGrobs[[ith]]
                             } else cg
                           })
      
    } else NULL
    
    grobs[[i]] <- grob
  }
  
  gtable$grobs <- grobs 
  return(gtable)
}

set_grobFromGtable.l_facet_grid <- function(gtable, newGrobs, arrangeGrobArgs) {
  
  namesNewGrobs <- vapply(newGrobs, function(ng) ng$name, character(1L))

  if(!is.gtable(gtable)) return(set_grobFromGtable.default(gtable, newGrobs, arrangeGrobArgs))
  
  grobs <- gtable$grobs
  len <- length(grobs)
  
  if(len == 0) return(set_grobFromGtable.default(gtable, newGrobs, arrangeGrobArgs)) 
  
  for(i in seq(len)) {
    
    grob <- grobs[[i]]
    
    if(is.gtable(grob)) {
      
      childGrobs <- grob$grobs
      
      grob$grobs <- lapply(childGrobs, 
                           function(cg) {
                             ith <- which(namesNewGrobs %in% cg$name)
                             if(length(ith) > 0) {
                               newGrobs[[ith]]
                             } else cg
                           })
      
    } else NULL
    
    grobs[[i]] <- grob
  }
  
  gtable$grobs <- grobs 
  return(gtable)
}