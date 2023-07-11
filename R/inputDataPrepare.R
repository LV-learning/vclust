inputDataPrepare <- function(data_path,x_names){
  dt <- as.data.frame(read.csv(data_path,header = FALSE, stringsAsFactors = FALSE,na.strings = c("*","9999","NA","<NA>")))
  dt_types <- all(sapply(dt,typeof) == "character")
  if(dt_types){
    dt <- as.data.frame(read.csv(data_path,header = TRUE, stringsAsFactors = FALSE,na.strings = c("*","9999","NA","<NA>")))
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
  return(dt)
}
