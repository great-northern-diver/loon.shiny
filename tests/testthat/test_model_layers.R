context("test model layers")
library(loon.shiny)
pdf(NULL)

test_that("test l_plot", {
  p <- l_plot(iris)
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
  p['swapAxes'] <- TRUE
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
  # different glyphs
  ###################### polygon glyph ######################
  x_star <-
    c(-0.000864304235090734, 0.292999135695765, 0.949870354364736,
      0.474503025064823, 0.586862575626621, -0.000864304235090734,
      -0.586430423509075, -0.474070872947277, -0.949438202247191,
      -0.29256698357822)
  y_star <-
    c(-1, -0.403630077787381, -0.308556611927398, 0.153846153846154,
      0.808556611927398, 0.499567847882455, 0.808556611927398,
      0.153846153846154, -0.308556611927398, -0.403630077787381)
  x_cross <-
    c(-0.258931143762604, -0.258931143762604, -0.950374531835206,
      -0.950374531835206, -0.258931143762604, -0.258931143762604,
      0.259651397291847, 0.259651397291847, 0.948934024776722,
      0.948934024776722, 0.259651397291847, 0.259651397291847)
  y_cross <-
    c(-0.950374531835206, -0.258931143762604, -0.258931143762604,
      0.259651397291847, 0.259651397291847, 0.948934024776722,
      0.948934024776722, 0.259651397291847, 0.259651397291847,
      -0.258931143762604, -0.258931143762604, -0.950374531835206)
  x_hexagon <-
    c(0.773552290406223, 0, -0.773552290406223, -0.773552290406223,
      0, 0.773552290406223)
  y_hexagon <-
    c(0.446917314894843, 0.894194756554307, 0.446917314894843,
      -0.447637568424085, -0.892754249495822, -0.447637568424085)

  p <- l_plot(1:3, 1:3)

  gl <- l_glyph_add_polygon(p, x = list(x_star, x_cross, x_hexagon),
                            y = list(y_star, y_cross, y_hexagon))

  p['glyph'] <- gl
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
  gl['showArea'] <- FALSE
  p['glyph'] <- gl
  x
  expect_equal(class(x), "shiny.appobj")
  ###################### serialaxes glyph ######################
  p <- l_plot(iris)
  gs <- l_glyph_add_serialaxes(p, data = iris, showArea=FALSE)
  p['glyph'] <- gs
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
  gs['axesLayout'] <- "parallel"
  p['glyph'] <- gs
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
  gs['showArea'] <- TRUE
  p['glyph'] <- gs
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
  gs['showEnclosing'] <- TRUE
  p['glyph'] <- gs
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
  ###################### image glyph ######################
  # p <- with(olive, l_plot(palmitic ~ stearic, color = Region))
  # img_paths <- list.files(file.path(find.package(package = 'loon'), "images"), full.names = TRUE)
  # imgs <- setNames(l_image_import_files(img_paths),
  #                  tools::file_path_sans_ext(basename(img_paths)))
  # i <- pmatch(gsub("^[[:alpha:]]+-","", olive$Area), names(imgs), duplicates.ok = TRUE)
  #
  # g <- l_glyph_add_image(p, imgs[i], label="Flags")
  # p['glyph'] <- g
  # x <- loon.shiny(p)
  # x
  # expect_equal(class(x), "shiny.appobj")
  ###################### pointrange glyph ######################
  p <- l_plot(x = 1:3, color = c('red', 'blue', 'green'), showScales=TRUE)
  g <- l_glyph_add_pointrange(p, ymin=(1:3)-(1:3)/5, ymax=(1:3)+(1:3)/5)
  p['glyph'] <- g
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
  ###################### text glyph ######################
  p <- l_plot(iris, color = iris$Species)
  g <- l_glyph_add_text(p, iris$Species, "test_label")
  p['glyph'] <- g
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
})

test_that("test l_hist", {
  p <- l_hist(iris)
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
  p['swapAxes'] <- TRUE
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
})


test_that("test l_serialaxes", {
  p <- l_serialaxes(iris)
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
})

test_that("test l_pairs", {
  p <- l_pairs(iris, showHistograms = TRUE)
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
  p <- l_pairs(iris, showHistograms = TRUE, histLocation = "diag")
  x <- loon.shiny(p)
  x
  expect_equal(class(x), "shiny.appobj")
})

test_that("test l_graph", {
  p <- l_navgraph(iris)
  g <- p$graph
  x <- loon.shiny(g)
  x
  expect_equal(class(x), "shiny.appobj")
  g['swapAxes'] <- TRUE
  x <- loon.shiny(g)
  x
  expect_equal(class(x), "shiny.appobj")
})

