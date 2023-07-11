#' @importFrom utils write.csv write.table
createBinaryO <- function(y_names,
                          input_dt,
                          output_path_prefix,
                          O_threshold,
                          O_sign,
                          O_max_min_mean){
  output_dir_out <- ""
  O_sign <- unlist(sapply(O_sign, FUN = function(x){
    if(toupper(trimws(x)) == "GT"){
      ">"
    }else if(toupper(trimws(x)) == "LT"){
      "<"
    }else if(toupper(trimws(x)) == "GE") {
      ">="
    }else if(toupper(trimws(x)) == "LE"){
      "<="
    }else if(toupper(trimws(x)) == "EQ") {
      "=="
    }else{
      x
    }
  }))
  O_max_min_mean <- tolower(trimws(O_max_min_mean))
  if(length(O_threshold) == 1){
    input_dt[,"O"] <- apply(input_dt[,y_names],
                            1,
                            FUN=function(x)ifelse(sjmisc::is_empty(x),
                                                  NA,
                                                  do.call(O_max_min_mean,list(x,na.rm = ifelse(O_max_min_mean == "min",F,T)))))


    text_all <- paste('O <- ifelse(input_dt$O ',
                      O_sign,
                      ' O_threshold, 1, 0)',
                      sep = '')
    print(text_all)
    eval(parse(text = text_all))
  }else{
    warning("The Ogroups_cutpoint has multiple entries. Ogroups_cutpoint_max_min_mean is ignored")
    if(length(O_sign) == 1){
      O_sign <- rep(O_sign,length(O_threshold))
    }
    if(length(O_sign) != length(O_threshold) ){
      print(O_sign)
      stop("wrong length of O_sign for multiple thresholds")
    }
    logic_exp <- paste(paste(paste("input_dt",y_names,sep="$"),O_sign,O_threshold,sep=""),collapse = " & ")
    text_all <- paste('O <- ifelse(',logic_exp,",1,0)",sep="")
    print(text_all)
    eval(parse(text = text_all))
  }
  O[is.na(O)] <- 0
  O <- data.frame(O = O)
  print(O)
  output_dir <- paste(output_path_prefix,
                      "cP",
                      2,
                      "/",
                      sep = "")
  print(output_dir)
  if(dir.exists(output_dir) == FALSE){
    dir.create(output_dir)
  }
  ###save class membership
  output_dir_prob <- paste(output_dir,
                           "cP",
                           2,
                           ".pp",
                           sep = "")
  print(output_dir_prob)
  write.table(O,
              output_dir_prob,
              sep = " ",
              col.names = FALSE,row.names = FALSE)


  output_dir_out <- output_dir

  print("output_dir_out is")
  print(output_dir_out)

  return(list(2,output_dir_out))
}

