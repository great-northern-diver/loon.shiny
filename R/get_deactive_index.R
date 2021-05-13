get_deactive_index <- function(loon_grob, index, ...) {
  obj <- character(0)
  class(obj) <- names(loon_grob$children)
  UseMethod("get_deactive_index", obj)
}

get_deactive_index.l_plot <- function(loon_grob, index, ...) {

  args <- list(...)
  pointsTree_name <- args$pointsTree_name

  if(pointsTree_name != "points: missing glyphs") {

    newGrob <- grid::getGrob(loon_grob, pointsTree_name)
    which_is_deactive <- numeric(0)

    lapply(index,
           function(i) {

             if(stringr::str_detect(newGrob$children[[i]]$name, "primitive_glyph")) {

               if(stringr::str_detect(grobName(newGrob$children[[i]]), "grob")) {
                 which_is_deactive[i] <<- i
               }

             } else if(stringr::str_detect(newGrob$children[[i]]$name, "pointrange_glyph")) {

               the_children <- newGrob$children[[i]]$children
               if(
                 all(
                   sapply(1:length(the_children),
                          function(j) {
                            stringr::str_detect(grobName(the_children[[j]]), "grob")
                          }
                   )
                 )
               ) {
                 which_is_deactive[i] <<- i
               }

             } else if(stringr::str_detect(newGrob$children[[i]]$name, "text_glyph")) {

               if(stringr::str_detect(grobName(newGrob$children[[i]]), "grob")) {
                 which_is_deactive[i] <<- i
               }

             } else if(stringr::str_detect(newGrob$children[[i]]$name, "serialaxes_glyph")) {

               the_children <- newGrob$children[[i]]$children
               if(
                 all(
                   sapply(1:length(the_children),
                          function(j) {
                            stringr::str_detect(grobName(the_children[[j]]), "grob")
                          }
                   )
                 )
               ) {
                 which_is_deactive[i] <<- i
               }

             } else if(stringr::str_detect(newGrob$children[[i]]$name, "polygon_glyph")) {

               if(stringr::str_detect(grobName(newGrob$children[[i]]), "grob")) {
                 which_is_deactive[i] <<- i
               }

             } else if(stringr::str_detect(newGrob$children[[i]]$name, "image_glyph")) {

               the_children <- newGrob$children[[i]]$children
               if(
                 all(
                   sapply(1:length(the_children),
                          function(j) {
                            stringr::str_detect(grobName(the_children[[j]]), "grob")
                          }
                   )
                 )
               ) {
                 which_is_deactive[i] <<- i
               }

             } else stop("not inplemented")
           }
    )

    which_is_deactive[which(!is.na(which_is_deactive))]
  } else numeric(0)
}


get_deactive_index.l_hist <- function(loon_grob, index) {

  newGrob <- grid::getGrob(loon_grob, "histogram")
  which_is_deactive <- numeric(0)

  lapply(index,
         function(i) {
           if(stringr::str_detect(grobName(newGrob$children[[i]]), "grob")) {
             which_is_deactive[i] <<- i
           }
         }
  )

  which_is_deactive[which(!is.na(which_is_deactive))]
}


get_deactive_index.l_graph <- function(loon_grob, index) {

  newGrob <- grid::getGrob(loon_grob, "graph nodes")
  which_is_deactive <- numeric(0)

  lapply(index,
         function(i) {
           if(stringr::str_detect(grobName(newGrob$children[[i]]), "grob")) {
             which_is_deactive[i] <<- i
           }
         }
  )

  which_is_deactive[which(!is.na(which_is_deactive))]
}

get_deactive_index.l_serialaxes <- function(loon_grob, index, ...) {

  args <- list(...)
  axes_gPath <- args$axes_gPath

  axes_grob <- grid::getGrob(loon_grob, axes_gPath)
  which_is_deactive <- numeric(0)

  lapply(index,
         function(i) {
           if(stringr::str_detect(grobName(axes_grob$children[[i]]), "grob")) {
             which_is_deactive[i] <<- i
           }
         }
  )

  which_is_deactive[which(!is.na(which_is_deactive))]
}
