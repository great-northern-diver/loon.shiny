get_loonWidgetsInfo <- function(widgets, loon.grobs, ...) {
  UseMethod("get_loonWidgetsInfo", widgets)
}

get_loonWidgetsInfo.default <- function(widgets, loon.grobs, ...) {
  stop("Unknow widget", call. = FALSE)
}

get_loonWidgetsInfo.l_compound <- function(widgets, loon.grobs, ...) {
  lapply(widgets,
         function(widget) {
           get_loonWidgetsInfo(widget,
                               loon.grobs[[as.character(widget)]],
                               ...)
         }
  )
}

get_loonWidgetsInfo.list <- function(widgets, loon.grobs, ...) {
  get_loonWidgetsInfo.l_compound(widgets, loon.grobs, ...)
}

get_loonWidgetsInfo.l_facet_ggplot <- function(widgets, loon.grobs,...) {

  args <- list(...)
  navbarMenuName <- args$navbarMenuName

  plots <- l_getPlots(widgets)
  subtitles <- l_getSubtitles(widgets)

  colSubtitles <- subtitles$colSubtitles
  rowSubtitles <- subtitles$rowSubtitles

  lapply(seq(length(plots)),
         function(i){
           colSubtitle <- colSubtitles[i]
           rowSubtitle <- rowSubtitles[i]

           get_loonWidgetsInfo(plots[[i]],
                               loon.grobs = loon.grobs[[as.character(plots[[i]])]],
                               title = paste0(c(colSubtitle, rowSubtitle), collapse = "\n"),
                               navbarMenuName = navbarMenuName)
         }
  )
}

none_loonWidgetsInfo <- function(...) {

  args <- list(...)
  navbarMenuName <- args$navbarMenuName

  list(
    linkingGroup = "none",
    linkingKey = NULL,
    selectByLoon = NULL,
    linkedStates = NULL,
    navbarMenuName = navbarMenuName
  )
}
