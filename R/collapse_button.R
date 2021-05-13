collapse_button <- function(x, tabPanelName) {
  HTML(
    paste0("<button data-toggle='collapse' style='font-size:smaller' data-target=", 
           "'#",
           tabPanelName,
           x,
           "'",
           ">",
           x,
           "</button>")
  )
}