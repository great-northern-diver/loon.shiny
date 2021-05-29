get_deactive_index <- function(loon.grob, index, ...) {
  obj <- character(0)
  class(obj) <- names(loon.grob$children)
  UseMethod("get_deactive_index", obj)
}

get_deactive_index.l_plot <- function(loon.grob, index, ...) {

  args <- list(...)
  pointsTreeName <- args$pointsTreeName

  if(pointsTreeName != "points: missing glyphs") {

    newGrob <- grid::getGrob(loon.grob, pointsTreeName)
    whichIsDeactive <- numeric(0)

    lapply(index,
           function(i) {

             if(grepl("primitive_glyph",newGrob$children[[i]]$name)) {

               if(grepl("grob", grobName(newGrob$children[[i]]))) {
                 whichIsDeactive[i] <<- i
               }

             } else if(grepl("pointrange_glyph", newGrob$children[[i]]$name)) {

               the_children <- newGrob$children[[i]]$children
               if(
                 all(
                   sapply(1:length(the_children),
                          function(j) {
                            grepl("grob", grobName(the_children[[j]]))
                          }
                   )
                 )
               ) {
                 whichIsDeactive[i] <<- i
               }

             } else if(grepl("text_glyph", newGrob$children[[i]]$name)) {

               if(grepl("grob", grobName(newGrob$children[[i]]))) {
                 whichIsDeactive[i] <<- i
               }

             } else if(grepl("serialaxes_glyph", newGrob$children[[i]]$name)) {

               the_children <- newGrob$children[[i]]$children
               if(
                 all(
                   sapply(1:length(the_children),
                          function(j) {
                            grepl("grob", grobName(the_children[[j]]))
                          }
                   )
                 )
               ) {
                 whichIsDeactive[i] <<- i
               }

             } else if(grepl("polygon_glyph", newGrob$children[[i]]$name)) {

               if(grepl("grob", grobName(newGrob$children[[i]]))) {
                 whichIsDeactive[i] <<- i
               }

             } else if(grepl("image_glyph", newGrob$children[[i]]$name)) {

               the_children <- newGrob$children[[i]]$children
               if(
                 all(
                   sapply(1:length(the_children),
                          function(j) {
                            grepl("grob", grobName(the_children[[j]]))
                          }
                   )
                 )
               ) {
                 whichIsDeactive[i] <<- i
               }

             } else stop("not inplemented")
           }
    )

    whichIsDeactive[which(!is.na(whichIsDeactive))]
  } else numeric(0)
}


get_deactive_index.l_hist <- function(loon.grob, index) {

  newGrob <- grid::getGrob(loon.grob, "histogram")
  whichIsDeactive <- numeric(0)

  lapply(index,
         function(i) {
           if(grepl("grob", grobName(newGrob$children[[i]]))) {
             whichIsDeactive[i] <<- i
           }
         }
  )

  whichIsDeactive[which(!is.na(whichIsDeactive))]
}


get_deactive_index.l_graph <- function(loon.grob, index) {

  newGrob <- grid::getGrob(loon.grob, "graph nodes")
  whichIsDeactive <- numeric(0)

  lapply(index,
         function(i) {
           if(grepl("grob", grobName(newGrob$children[[i]]))) {
             whichIsDeactive[i] <<- i
           }
         }
  )

  whichIsDeactive[which(!is.na(whichIsDeactive))]
}

get_deactive_index.l_serialaxes <- function(loon.grob, index, ...) {

  args <- list(...)
  axesGpath <- args$axesGpath

  axesGrob <- grid::getGrob(loon.grob, axesGpath)
  whichIsDeactive <- numeric(0)

  lapply(index,
         function(i) {
           if(grepl("grob", grobName(axesGrob$children[[i]]))) {
             whichIsDeactive[i] <<- i
           }
         }
  )

  whichIsDeactive[which(!is.na(whichIsDeactive))]
}
