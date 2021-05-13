get_loonWidgets_info <- function(widgets, loon_grobs, colorList, ...) {
  UseMethod("get_loonWidgets_info", widgets)
}

get_loonWidgets_info.default <- function(widgets, loon_grobs, colorList, ...) {
  stop("Unknow widget", call. = FALSE)
}

get_loonWidgets_info.l_compound <- function(widgets, loon_grobs, colorList, ...) {
  lapply(widgets,
         function(widget) {
           get_loonWidgets_info(widget, 
                                loon_grobs[[as.character(widget)]], 
                                colorList, 
                                ...)
         }
  )
}

get_loonWidgets_info.list <- function(widgets, loon_grobs, colorList, ...) {
  get_loonWidgets_info.l_compound(widgets, loon_grobs, colorList, ...)
}

get_loonWidgets_info.l_facet_ggplot <- function(widgets, loon_grobs, colorList, ...) {
  
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
           
           get_loonWidgets_info(plots[[i]], 
                                loon_grobs = loon_grobs[[as.character(plots[[i]])]],
                                colorList, 
                                title = paste0(c(colSubtitle, rowSubtitle), collapse = "\n"), 
                                navbarMenuName = navbarMenuName)
         }
  )
}

none_loonWidgets_info <- function(...) {
  
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