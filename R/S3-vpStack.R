# It is not suggested
# it modifies the data structure of the grid graphics
# names.vpStack <- function(x) {
#   vapply(x,
#          function(v) {
#            v$name
#          }, character(1L))
# }
#
# `[.vpStack` <- function(x, i) {
#   x[[which(names(x) %in% i)]]
# }

vpStack_names <- function(x) {
    vapply(x,
           function(v) {
             v$name
           }, character(1L))
}

get_vp_from_vpStack <- function(x, i) {
  x[[which(vpStack_names(x) %in% i)]]
}
