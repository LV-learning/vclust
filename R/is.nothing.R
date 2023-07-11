
is.nothing <- function(x){
  if(identical(x,character(0))){
    return(TRUE)
  }else if(is.null(x) == TRUE){
    return(TRUE)
  }else if(is.na(x) == TRUE){
    return(TRUE)
  }else if(is.nan(x) == TRUE){
    return(TRUE)
  }
  return(FALSE)
}
