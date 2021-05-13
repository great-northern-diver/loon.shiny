adjust_canvas_size <- function(canvas_size) {

  canvas_size <- round(as.numeric(gsub("\\D", "", canvas_size)) / 100) * 100

  if(canvas_size < 100) {
    "100px"
  } else if (canvas_size > 1000) {
    "1000px"
  } else {
    paste0(canvas_size, "px")
  }
}
