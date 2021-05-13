get_noneInteractiveGrobs_index <- function(loon_grobs) {

  n <- length(loon_grobs)
  id <- sapply(1:n,
               function(i){
                 loon_grob <- loon_grobs[[i]]
                 if(stringr::str_detect(grid::grobName(loon_grob), "gTree")) NA else i
               }
  )
  id[which(!is.na(id))]
}
