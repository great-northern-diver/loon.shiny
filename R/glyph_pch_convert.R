
glyph_to_pch <- function(glyph) {

  vapply(glyph, function(x) {
    switch(
      x,
      circle = 19,
      ocircle = 1,
      ccircle = 21,
      square = 15,
      osquare = 0,
      csquare = 22,
      triangle = 17,
      otriangle = 2,
      ctriangle = 24,
      diamond = 18,
      odiamond = 5,
      cdiamond = 23,
      19
    )
  }, numeric(1))

}

pch_to_glyph <- function(pch) {

  if(all(!is.na(pch))) {
    unname(
      vapply(pch,
             function(p) {

               p <- as.character(p)

               switch(p,
                 "19" = "circle" ,
                 "1" = "ocircle",
                 "21" = "ccircle",
                 "15" = "square",
                 "0" = "osquare",
                 "22" = "csquare",
                 "17" = "triangle",
                 "2" = "otriangle",
                 "24" = "ctriangle",
                 "18" = "diamond",
                 "5" = "odiamond",
                 "23" = "cdiamond",
                 {
                   p
                 }
               )
             }, character(1)
      )
    )
  } else rep(NA, length(pch))
}


