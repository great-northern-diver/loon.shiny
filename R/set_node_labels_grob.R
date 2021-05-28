set_node_labelsGrob <- function(loon.grob, whichIsDeactive) {

  newGrob <- grid::getGrob(loon.grob, "graph labels")
  which_is_active <- setdiff(1:length(newGrob$children), whichIsDeactive)

  lapply(which_is_active,
         function(i){
           newGrob$children[[i]] <<- do.call(
             grid::textGrob,
             getGrobArgs(newGrob$children[[i]])
           )
         }
  )

  grid::setGrob(
    gTree = loon.grob,
    gPath = "graph labels",
    newGrob = newGrob
  )
}
