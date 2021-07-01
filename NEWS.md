# loon.shiny 1.0.1

1. Fix two bugs: 

    - once a layer is removed, the names of the rest layers should remain. 
  
    - once a layer is removed, the region should be adjusted (world view is changed)
  
2. For facets (`l_facet_grid`, `l_facet_wrap` and `l_facet_ggplot`), the labels of each panel can be displayed. The selection for each panel works well.  

# loon.shiny 1.0.0 

1. "clean" the code, i.e. representing variables with Camel case and functions with underscore case; give more meaningful names; remove hacks, etc.

2. make the logic in server function more intuitive

3. remove redundant/unnecessary computations

4. function `shiny.loon()` --> function `loon.shiny()`

5. new argument `displayedPanel`: The default is an empty string `""` so that none inspector components (`Plot`, `Linking`, `Select`, etc) are open automatically. The available strings are `c("Plot", "Select", "Linking", "Modify", "Layer", "Glyph")`

6. Transparency can be adjusted for points

# loon.shiny 0.1.0

Date of Birth: 2021-05-10
