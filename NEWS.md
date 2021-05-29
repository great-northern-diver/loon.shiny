# loon.shiny 0.1.1

1. "clean" the code, i.e. representing variables with Camel case and functions with underscore case; give more meaningful names; remove hacks, etc.

2. make the logic in server function more intuitive

3. remove redundant/unnecessary computations

4. function `shiny.loon()` --> function `loon.shiny()`

5. new argument `displayedPanel`: The default is an empty string `""` so that none inspector components (`Plot`, `Linking`, `Select`, etc) are open automatically. The available strings are `c("Plot", "Select", "Linking", "Modify", "Layer", "Glyph")`

# loon.shiny 0.1.0

Date of Birth: 2021-05-10
