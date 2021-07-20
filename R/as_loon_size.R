as_loon_size <- function(size,
                         type = c("points", "texts", "images",
                                  "radial", "parallel", "polygon",
                                  "lines"),
                         adjust = 1, # A HACK
                         ...) {

  # **`size` is a ggplot size**
  # **returned a loon size**
  if(is.null(size))
    return(as.numeric(loon::l_getOption("size")))

  switch(type,
         points = {
           # From Adrian Waddell's Thesis
           # Glyph Type:
           ## Non-primitive Glyph
           ## size < 1 --> 8 (area in pixel)
           ## size >= 1 --> 12 * size (area in pixel)
           diameter.pt <- size * .pt
           diameter.px <- diameter.pt * pt2px(adjust = adjust)
           area <- (diameter.px/2)^2 * pi
           ifelse(area < 8, 1, area/12)
         },
         lines = {
           # output unit is mm
           # suppose the unit of loon is in px
           # (cm2px()/10 * ggplot2::.pt) * size
           # suppose the unit of loon is in mm
           size * .pt * pt2px(adjust = adjust)
         },
         texts = {
           ## Text Glyph
           ## size < 1 --> 2 (area in pixel)
           ## size >= 1 --> 2 + size (area in pixel)
           area.pt <- size * .pt
           area <- area.pt * pt2px(adjust = adjust)
           ifelse(area < 2, 1, (area - 2))
         },
         images = {
           args <- list(...)
           # ratio = height/width
           ratio <- args$ratio %||% 1
           ## Image Glyph
           ## size < 1 --> 20 (area in pixel)
           ## size >= 1 --> 600 * size (area in pixel)

           # size is height
           height.px <- size * cm2px(adjust = adjust)
           area <- height.px^2/ratio
           ifelse(area < 20, 1, area/600)
         },
         polygon = {
           # unit is cm
           # to px
           size <- size * cm2px(adjust = adjust)
           ifelse(size < 4, 1, (size/6)^2)
         },
         radial = {
           diameter.px <- size * cm2px(adjust = adjust)
           area <- (diameter.px/2)^2 * pi
           ifelse(area < 25, 1, area/400)
         },
         parallel = {
           args <- list(...)
           # ratio = height/width
           p <- args$p %||% 5
           area <- (size * cm2px(adjust = adjust))^2 * 2
           ifelse(area < 9 * (p - 1), 1, area/(64 * (p - 1)))
         },
         {
           # default is point
           # From Adrian Waddell's Thesis
           # Glyph Type:
           ## Non-primitive Glyph
           ## size < 1 --> 8 (area in pixel)
           ## size >= 1 --> 12 * size (area in pixel)
           diameter.pt <- size * .pt
           diameter.px <- diameter.pt * pt2px(adjust = adjust)
           area <- (diameter.px/2)^2 * pi
           ifelse(area < 8, 1, area/12)
         }
  )
}

loon_default_size <- function(type = c("points", "texts", "images",
                                       "radial", "parallel", "polygon",
                                       "lines", "adjust")) {
  switch(type,
         points = {
           # as_grid_size(4, 'point', pch = 19)
           5.86323
         },
         text = {
           # as_grid_size(4, 'text')
           4.5
         },
         adjust = {
           # also used in loonGrob
           0.6
         },
         images = {
           # as_grid_size(4, 'images', ratio = 1)
           1.291346
         },
         radial = {
           # as_grid_size(4, 'radial')
           1.18974
         },
         parallel = {
           # as_grid_size(4, 'parallel', p = 5)
           0.5964471
         },
         polygon = {
           # as_grid_size(4, 'polygon')
           0.3163138
         },
         lines = {
           # as_grid_size(4, 'lines')
           3
         }, {
           # else pch
           1
         }
  )
}

step_size <- function(pch) {
  vapply(pch,
         function(p) {
             switch(p,
                    points = {
                      1 #pt
                    },
                    text = {
                      1 #pt
                    },
                    images = {
                      pt2mm()
                    },
                    radial = {
                      pt2mm()
                    },
                    parallel = {
                      pt2mm()
                    },
                    polygon = {
                      pt2mm()
                    },
                    lines = {
                      1
                    }, {
                      1
                    }
             )
         },
         numeric(1L))
}

pt2cm <- function() 0.03514598 # convertUnit(unit(1, "pt"), "cm", valueOnly = TRUE)
pt2mm <- function() 0.3514598 # convertUnit(unit(1, "pt"), "mm", valueOnly = TRUE)
.pt <- 2.845276
pt2px <- function(adjust = 1) 4/3 * adjust
px2pt <- function(adjust = 1) 3/4 * adjust
cm2px <- function(adjust = 1) {
  # grid::convertUnit(grid::unit(1, "cm"), "pt",
  #                   valueOnly = TRUE) * pt2px(adjust = adjust)
  .pt * 10 * pt2px(adjust = adjust)
}
px2cm <- function(adjust = 1) {
  # px2pt(adjust = adjust) * grid::convertUnit(grid::unit(1, "pt"), "cm",
  #                   valueOnly = TRUE)
  px2pt(adjust = adjust) /(.pt * 10)
}
