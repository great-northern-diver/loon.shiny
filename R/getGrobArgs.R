getGrobArgs <- function (grob) {
  args <- lapply(names(grob), FUN = function(x) {grob[[x]]})
  args <- setNames(args, names(grob))
  args
}
