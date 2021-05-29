extend_loonWidgetsInfo <- function(loonWidgetsInfo, loon.grobs, navbarMenuName) {

  count <- 0
  len_loon.grobs <- length(loon.grobs)
  len_loonWidgetsInfo <- length(loonWidgetsInfo)

  if(len_loon.grobs > len_loonWidgetsInfo) {

    noneInteractiveGrobs_index <- get_noneInteractiveGrobs_index(loon.grobs)
    if(length(noneInteractiveGrobs_index) != (length(loon.grobs) - length(loonWidgetsInfo))) stop("unexpected loon grobs")

    lapply(1:len_loon.grobs,
           function(i) {
             if(i %in% noneInteractiveGrobs_index) {

               count <<- count + 1
               none_loonWidgetsInfo(navbarMenuName = navbarMenuName)
             } else {
               loonWidgetsInfo[[i - count]]
             }
           })

  } else if(length(loon.grobs) < length(loonWidgetsInfo)) {
    stop("unexpected loon grobs length")
  } else loonWidgetsInfo
}
