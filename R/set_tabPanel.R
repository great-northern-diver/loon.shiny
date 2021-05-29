set_tabPanel <- function(sidebarPanel_args, tabPanelNames) {

  path <- file.path(find.package(package = 'loon.shiny'), "images")

  # It is mainly for the test
  # In `devtools::test`, it is looking for the Github source
  # not the library source. Hence, the path is different
  tryCatch(
    suppressWarnings(
      base64enc::dataURI(file=paste0(path, "/LoonIcon.png"), mime="image/png")
    ),
    error = function(e) {
      path <<- file.path(find.package(package = 'loon.shiny'), "inst/images")
    }
  )


  unique_tabPanelNames <- unique(tabPanelNames)

  if(is.null(tabPanelNames))
    stop("Unknown tab panel names")

  n <- length(unique_tabPanelNames)

  do.call(
    navbarPage,
    c(
      list(
        title = div(
          tags$img(src = base64enc::dataURI(file=paste0(path, "/LoonIcon.png"), mime="image/png"),
                   height = "20px",
                   weight = "20px"
          ), " Loon Inspector"
        ),
        id = "navBarPage"
      ),
      lapply(1:n,
             function(i) {
               name <- unique_tabPanelNames[i]
               j <- which(name == tabPanelNames)

               if(length(j) == 1) {
                 sidebarPanel_args[[j]]
               } else {
                 do.call(
                   navbarMenu,
                   c(
                     list(title = name),
                     sidebarPanel_args[j]
                   )
                 )
               }
             }
      )
    )
  )
}
