get_noneInteractiveGrobs_index <- function(loon.grobs) {

  n <- length(loon.grobs)
  id <- sapply(1:n,
               function(i){
                 loon.grob <- loon.grobs[[i]]
                 if(grepl(grid::grobName(loon.grob), pattern = "gTree")) NA else i
               }
  )
  id[which(!is.na(id))]
}
