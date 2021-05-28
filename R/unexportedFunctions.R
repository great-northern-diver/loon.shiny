char2num.data.frame <- utils::getFromNamespace("char2num.data.frame", "loon")
l_getSubtitles <- utils::getFromNamespace("l_getSubtitles", "loon.ggplot")

# it is exported in loon >= 1.3.7 version
# and should be removed later.
l_colorName <- function(x, error = TRUE) {

  if(utils::packageVersion("loon") > "1.3.6") {

    loon::l_colorName(x, error = error)

  } else {

    color.id <- utils::getFromNamespace("color.id", "loon")

    hex2colorName <- function(color) {
      # the input colors are 6/12 digits hex code
      uniColor <- unique(color)
      colorName <- color.id(uniColor)
      len <- length(colorName)

      for(i in seq(len)) {
        color[color == uniColor[i]] <- colorName[i]
      }
      color
    }
    hex2colorName(color)
  }
}
