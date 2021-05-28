get_selectBy <- function(selectBy = NULL, loonWidgetsInfo){
  
  selectBy <- if(is.null(selectBy)) {
    "sweeping" # default setting
  } else {
    if(!selectBy %in% c("byDefault", "brushing", "sweeping")) stop("selectBy must be one of `byDefault`, `brushing` and `sweeping`") else selectBy[1]
  }
  
  n <- length(loonWidgetsInfo)
  
  if(selectBy == "byDefault") {
    
    selectBy <- names(
      sort(
        table(
          unlist(
            lapply(1:n,
                   function(i) {
                     loonWidgetsInfo[[i]]$selectByLoon
                   })
          )
        ),decreasing=TRUE
      )[1]
    )
    
    if(is.null(selectBy)) selectBy <- "sweeping" 
  }
  
  selectBy
}
