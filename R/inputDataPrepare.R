inputDataPrepare <- function(data_path,
                             x_names,
                             naString = NULL){
  if(is.null(naString)){
    naString <- c("*","9999","NA","<NA>")
  }
  print('#######naString is################')
  print(naString)
  dt <- as.data.frame(read.csv(data_path,header = FALSE, stringsAsFactors = FALSE,na.strings = naString))
  dt_types <- all(sapply(dt,typeof) == "character")
  if(dt_types){
    dt <- as.data.frame(read.csv(data_path,header = TRUE, stringsAsFactors = FALSE,na.strings = naString))
    if(!sjmisc::is_empty(x_names)){
      names(dt) <- x_names
    }
  }else{
    if(sjmisc::is_empty(x_names)){
      warning("Please provide variable names; Your data doesn't seem have the header")
    }
    names(dt) <- x_names
  }
  dt$original_id <- row.names(dt)
  print(dt)
  return(dt)
}
