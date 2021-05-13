char2num.data.frame <- utils::getFromNamespace("char2num.data.frame", "loon")

# export from loon.ggplot
l_getSubtitles <- function(target) {
  
  # find the parent `tk` window name
  parent <- as.character(tkwinfo("parent",  target[[1L]]))
  # access all children
  children <- as.character(tkwinfo("child",  parent))
  
  xLabelPathName <- children[grepl("xlabel", children)]
  yLabelPathName <- children[grepl("ylabel", children)]
  titlePathName <- children[grepl("title", children)]
  
  # label
  xlabel <- if(length(xLabelPathName) > 0) {
    paste0(as.character(tkcget(xLabelPathName, "-text")), collapse = " ")
  } else {
    NULL
  }
  ylabel <- if(length(yLabelPathName) > 0) {
    paste0(as.character(tkcget(yLabelPathName, "-text")), collapse = "")
  } else {
    NULL
  }
  title <- if(length(titlePathName) > 0) {
    paste0(as.character(tkcget(titlePathName, "-text")), collapse = " ")
  } else {
    NULL
  }
  
  # FacetWrap or FacetGrid
  FacetWrap <- any(grepl("wrap", children))
  FacetGrid <- any(grepl("grid", children))
  
  # title column subtitle or row subtitle
  if(FacetWrap) {
    
    columnlabelPathName <- children[grepl("columnlabel", children)]
    colSubtitles <- if(length(columnlabelPathName) > 0L) {
      unname(
        sapply(columnlabelPathName,
               function(path) {
                 paste0(as.character(tkcget(path, "-text")), collapse = " ")
               })
      )
    } else NULL
    rowSubtitles <- NULL
    
    labelPathName1L <- c(columnlabelPathName)[1L]
    pathSplit <- strsplit(labelPathName1L, "-")[[1L]]
    byCOLSChar <- pathSplit[grepl("byCOLS", pathSplit)]
    byCOLS <- grepl("TRUE", byCOLSChar)
    byROWSChar <- pathSplit[grepl("byROWS", pathSplit)]
    byROWS <- grepl("TRUE", byROWSChar)
    
  } else if (FacetGrid) {
    
    columnlabelPathName <- children[grepl("columnlabel", children)]
    rowlabelPathName <- children[grepl("rowlabel", children)]
    
    colSubtitles <- if(length(columnlabelPathName) > 0) {
      unname(
        sapply(columnlabelPathName,
               function(path) {
                 paste0(as.character(tkcget(path, "-text")), collapse = " ")
               })
      )
    } else NULL
    
    rowSubtitles <- if(length(rowlabelPathName) > 0) {
      unname(
        sapply(rowlabelPathName,
               function(path) {
                 paste0(as.character(tkcget(path, "-text")), collapse = "")
               })
      )
    } else NULL
    
    labelPathName1L <- c(columnlabelPathName, rowlabelPathName)[1L]
    pathSplit <- strsplit(labelPathName1L, "-")[[1L]]
    byCOLSChar <- pathSplit[grepl("byCOLS", pathSplit)]
    byCOLS <- grepl("TRUE", byCOLSChar)
    byROWSChar <- pathSplit[grepl("byROWS", pathSplit)]
    byROWS <- grepl("TRUE", byROWSChar)
    
  } else {
    colSubtitles <- NULL
    rowSubtitles <- NULL
    byCOLS <- FALSE
    byROWS <- FALSE
  }
  
  list(
    xlabel = xlabel,
    ylabel = ylabel,
    title = title,
    FacetWrap = FacetWrap,
    FacetGrid = FacetGrid,
    colSubtitles = colSubtitles,
    rowSubtitles = rowSubtitles,
    byCOLS = byCOLS,
    byROWS = byROWS
  )
}

as_hex6color <- function(color) {
  
  if(length(color) > 0){
    col <- vapply(color, function(x) {
      if (x == "") "" else loon::l_hexcolor(x)
    }, character(1))
    col <- suppressWarnings(loon::hex12tohex6(col))
    col[color == ""] <- NA
    col
  } else {
    NA
  }
}

color.id <- function(col) {
  vapply(col,
         function(color) {
           
           # real color
           if(!grepl("#", color)) return(color)
           
           # hex code color
           # hex12to6 will give warnings if the hex code is not 12
           # as_hex6color can accommodate 6 digits and 12 digits code
           tryCatch(
             expr = {
               color <- as_hex6color(color)
               c2 <- grDevices::col2rgb(color)
               coltab <- grDevices::col2rgb(
                 grDevices::colors()
               )
               cdist <- apply(coltab, 2, function(z) sum((z - c2)^2))
               grDevices::colors()[which(cdist == min(cdist))][1]
             },
             error = function(e) {
               color
             }
           )
           
         }, character(1))
}


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
