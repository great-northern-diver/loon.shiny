extend_loonWidgets_info <- function(loonWidgets_info, loon_grobs, navbarMenuName) {
  
  count <- 0
  len_loon_grobs <- length(loon_grobs)
  len_loonWidgets_info <- length(loonWidgets_info)
  
  if(len_loon_grobs > len_loonWidgets_info) {

    noneInteractiveGrobs_index <- get_noneInteractiveGrobs_index(loon_grobs)
    if(length(noneInteractiveGrobs_index) != (length(loon_grobs) - length(loonWidgets_info))) stop("unexpected loon grobs")
    
    lapply(1:len_loon_grobs,
           function(i) {
             if(i %in% noneInteractiveGrobs_index) {
               
               count <<- count + 1
               none_loonWidgets_info(navbarMenuName = navbarMenuName)
             } else {
               loonWidgets_info[[i - count]]
             }
           })

  } else if(length(loon_grobs) < length(loonWidgets_info)) {
    stop("unexpected loon grobs length") 
  } else loonWidgets_info
}