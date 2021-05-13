set_node_labels_grob <- function(loon_grob, which_is_deactive) {

  newGrob <- getGrob(loon_grob, "graph labels")
  which_is_active <- setdiff(1:length(newGrob$children), which_is_deactive)

  lapply(which_is_active,
         function(i){
           newGrob$children[[i]] <<- do.call(
             textGrob,
             getGrobArgs(newGrob$children[[i]])
           )
         }
  )

  setGrob(
    gTree = loon_grob,
    gPath = "graph labels",
    newGrob = newGrob
  )
}
