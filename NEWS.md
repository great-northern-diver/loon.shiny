# loon.shiny 1.0.3

No major changes. Some minor changes are made on documents.

# loon.shiny 1.0.2

Fix a bug: in `loon` 1.3.9, as a `loon` widget is transformed to a `grid` object (i.e., `loonGrob`), once all points' plotting states are the same, they will be set as a scalar (e.g., col = "black"), rather a vector (e.g., col = ["black", "black", ...]), to reduce the memory consumption. However, in the previous `loon.shiny` versions, we only treat aesthetic attributes as vectors.

# loon.shiny 1.0.1

1. Fix two bugs: 

    - once a layer is removed, the names of the rest layers should remain. 
  
    - once a layer is removed, the region should be adjusted (world view is changed)
  
2. For facets (`l_facet_grid`, `l_facet_wrap` and `l_facet_ggplot`), the labels of each panel can be displayed. The selection for each panel works well.

3. Item labels (querying): a checkbox `itemLabels` is added on the inspector. If it is checked, pause the mouse on top of a point, a toolbox showing the detailed information of this point will be displayed. The size of the toolbox is determined by the argument `toolboxWidth` and the location is determined by the argument `toolboxLocation`.

# loon.shiny 1.0.0 

1. "clean" the code, i.e. representing variables with Camel case and functions with underscore case; give more meaningful names; remove hacks, etc.

2. make the logic in server function more intuitive

3. remove redundant/unnecessary computations

4. function `shiny.loon()` --> function `loon.shiny()`

5. new argument `displayedPanel`: The default is an empty string `""` so that none inspector components (`Plot`, `Linking`, `Select`, etc) are open automatically. The available strings are `c("Plot", "Select", "Linking", "Modify", "Layer", "Glyph")`

6. Transparency can be adjusted for points

# loon.shiny 0.1.0

Date of Birth: 2021-05-10
