x_namesPrepare <- function(x_names_input){
  if(typeof(x_names_input) != "character"){
    warning("wrong x_names type")
  }
  
  if(is.null(x_names_input)){
    warning("x_names can not be null")
  }
  
  x_names_input <- unlist(strsplit(x_names_input," |\t|\n|\f|\v|\r"))
  x_names_input <-  x_names_input[x_names_input != ""]
  n_length <- floor(length(x_names_input)/6) 
  results <- ""
  for(i in 1:n_length){
    results <- paste(results,paste(x_names_input[1:6],collapse = " "),"\n",sep = " ")
    x_names_input <- x_names_input[7:length(x_names_input)]
  }
  results <- paste(results,paste(x_names_input,collapse = " "),sep = " ")
  return(results)
} 



useObsPrepare <- function(x_names_input){
  if(typeof(x_names_input) != "character"){
    warning("wrong x_names type")
  }
  
  if(is.null(x_names_input)){
    warning("x_names can not be null")
  }
  
  x_names_input <- trimws(unlist(strsplit(x_names_input,"and")))
  x_names_input <-  x_names_input[x_names_input != ""]
  n_length <- floor(length(x_names_input)/4) 
  results <- ""
  for(i in 1:n_length){
    results <- paste(results,paste(x_names_input[1:4],collapse = " and ")," and ","\n",sep = " ")
    x_names_input <- x_names_input[5:length(x_names_input)]
  }
  results <- paste(results,paste(x_names_input,collapse = " and "),sep = " ")
  return(results)
} 

