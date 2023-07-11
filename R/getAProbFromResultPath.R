getAProbFromResultPath <- function(folder_path,
                                   ##model classes
                                   n){
  directory <- stringr::str_split(folder_path,"")[[1]]
  directory <- c(directory[1:(length(directory)-2)],as.character(n),"/")
  directory <- paste(directory,collapse = "")
  splits_dir_name <- stringr::str_split(directory,"/")[[1]]
  model_name_prefix <- splits_dir_name[length(splits_dir_name)-1]
  model_name_prefix <- paste(stringr::str_split(model_name_prefix,"")[[1]],collapse = "")
  pp_path <- paste(directory,model_name_prefix,".pp",sep="")
  #print(pp_path)
  print("pp_path is")
  print(pp_path)
  dt <- read_pp(pp_path)
  print(ncol(dt))
  if(ncol(dt) > 1){
    dt <- as.data.frame(dt[,(ncol(dt)-n):(ncol(dt))])
    names_dt <- c(paste("P",1:n,sep = ""),"trajectory_clusters")
    names(dt) <- names_dt
  }else if(ncol(dt) == 1){
    dt <- as.data.frame(dt)
    names(dt) <- c("trajectory_clusters")
  }
  dt
}
