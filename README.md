# loon.shiny <img src="man/figures/logo.png" align="right" width="120" />

[![Travis build status](https://travis-ci.org/z267xu/loon.shiny.svg?branch=master)](https://travis-ci.org/z267xu/loon.shiny)
[![Codecov test coverage](https://codecov.io/gh/z267xu/loon.shiny/branch/master/graph/badge.svg)](https://codecov.io/gh/z267xu/loon.shiny?branch=master)

Display loon widgets in shiny app

Documentation: [https://great-northern-diver.github.io/loon.shiny/](https://great-northern-diver.github.io/loon.shiny/)

## Introduction

[`Shiny`](https://shiny.rstudio.com/) provides interactive web applications in R. `JavaScript`, `CSS` and `Html` are wrapped in `r` functions. Users with zero experience on such areas can also build **fantastic**, **responsive** and **powerful** web pages. A `shiny` application is composed of two components, a `ui` (user interface) object and a `server` function. This UI/server pair are passed as arguments to the `shinyApp` function that creates a `shiny` app object.

[`Loon`](https://cran.r-project.org/web/packages/loon/vignettes/introduction.html) is an uncurated interactive toolkit engaged in an open-ended, creative and unscripted data exploration. Designed for interactive exploratory data analysis, `Loon` provides **true direct manipulations**. It can be horizontally/vertically panned, horizontally/vertically zoomed, and have plot elements linked to one another to effect such coordinated display behaviour as the selection of points, brushing, etc.

In interactive data analysis, one of the major difficulties is to reproduce and present analysis procedure. Package `loon.shiny` transforms `loon` widgets into `shiny` web apps. The benefit is that the presentation of `loon` is not necessary to be fixed. The `loon` widgets can be rendered to an html file by `Rmarkdown` so that analysts who explore data in `loon` now can present their interactive graphics in `Rmarkdown` which can help other users to explore some other possibilities even to draw different conclusions. 

## Installation

```
devtools::install_github("z267xu/loon.shiny")
```

## Usage

1. Basic `shiny` web app
```
library(loon.shiny)
p <- with(mtcars, 
       l_plot(hp, mpg, color = cyl, size = wt)
)
shiny.loon(p)
```
![](man/figures/loonShiny.gif)

2. Compound plots in `shiny` web app wrapping in `rmarkdown` file
![](man/figures/shinyDemo.gif)

3. `Ggplot` --> `loon` --> `shiny`
```
library(loon.ggplot)
g <- ggplot(mtcars, 
            mapping = aes(x = wt, y = hp)
) + 
geom_point(mapping = aes(color = factor(gear))) + 
geom_smooth()

shiny.loon(
  loon.ggplot::ggplot2loon(g),
  left = 20,
  right = "auto"
)
```
![](man/figures/ggplotLoonShiny.PNG)
Comparing with `ggplot` to `shiny`, `ggplot` --> `loon` --> `shiny` gathers more interactivity. In this shiny app, users can direct manipulate graphs, such as highlighting, changing colors and sizes, modifying layer positions and etc.

## Bugs report

```
https://github.com/z267xu/loon.shiny/issues
```
