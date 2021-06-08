char2num.data.frame <- utils::getFromNamespace("char2num.data.frame", "loon")
l_getSubtitles <- utils::getFromNamespace("l_getSubtitles", "loon.ggplot")

# This function is temporary
# after loon is updated to 1.3.7
# this function will be switched to
# `loon::l_colorName`
l_colorName <- function(color, error = TRUE) {

  color.id <- function(x, error = TRUE, env = environment()) {

    invalid.color <- c()

    colors <- vapply(x,
                     function(color) {

                       # hex code color
                       # hex12to6 will give warnings if the hex code is not 12
                       # as_hex6color can accommodate 6 digits and 12 digits code
                       tryCatch(
                         expr = {
                           color <- as_hex6color(color)
                           c2 <- grDevices::col2rgb(color)
                           coltab <- grDevices::col2rgb(colors())
                           cdist <- apply(coltab, 2, function(z) sum((z - c2)^2))
                           colors()[which(cdist == min(cdist))][1]
                         },
                         error = function(e) {

                           assign("invalid.color",
                                  c(invalid.color, color),
                                  envir = env)

                           return(color)

                         }
                       )

                     }, character(1))

    if(error && length(invalid.color) > 0) {
      stop("The input " ,
           paste(invalid.color, collapse = ", "),
           " are not valid color names", call. = FALSE)
    }
    colors
  }

  # the input colors are 6/12 digits hex code
  uniColor <- unique(color)
  colorName <- color.id(uniColor, error = error)
  len <- length(colorName)

  for(i in seq(len)) {
    color[color == uniColor[i]] <- colorName[i]
  }
  color
}
