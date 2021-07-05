loon_default_size <- function(){
  list(
    point_size = 0.57735,
    text_size = 5.946236,
    adjusted_size = 10.29919
  )
}

default_size <- function(){
  0.6
}

default_step_size <- function(line = FALSE){
  if(line) 1 else 0.2
}

pixels_2_lines <- function(x) {
  x / 20
}

point_default_pch <- function() 19

minimumSize <- function() 0.1
