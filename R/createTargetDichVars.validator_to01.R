createTargetDichVars <- function(validator,dt){
  UseMethod("createTargetDichVars")
}

##if binary binary_variable = threshold is for 0 and != is for 1
createTargetDichVars.validator_to01 <- function(validator,
                                                dt){
  ##check if threshold have the same length as validator
  # if(length(validator$thresholds) != length(validator$validator_variables)){
  #   stop("validator thresholds have different length as validator variables")
  # }
  if(length(validator$thresholds) == 1){
    a_threshold <- validator$thresholds[1]
    a_sign <- validator$signs[1]
    dt <- data.frame(variable_start_to_end = apply(dt[,validator$validator_variables,drop=FALSE],
                                                   1,
                                                   FUN=function(x)ifelse(sjmisc::is_empty(x),
                                                                         NA,
                                                                         do.call(validator$max_min_mean,list(x,na.rm = ifelse(validator$max_min_mean == "min",F,T))))),
                     dt)
    text_all <- paste('dt$variable_start_to_end <- ifelse(dt$variable_start_to_end ',
                      a_sign,
                      ' a_threshold, 1, 0)',
                      sep = '')
    print("command for createTargetDichVars is")
    print(text_all)
    eval(parse(text = text_all))
  }else{
    warning("The threshold has multiple entries. max_min_mean is ignored")
    if(length(validator$signs) == 1){
      signs <- rep(validator$signs,length(validator$thresholds))
    }else{
      signs <- validator$signs
    }
    if(length(signs) != length(validator$thresholds) ){
      print(signs)
      stop("wrong length of signs for multiple thresholds")
    }
    logic_exp <- paste(paste(paste("dt",validator$validator_variables,sep="$"),signs,validator$thresholds,sep=""),collapse = " & ")
    text_all <- paste('variable_start_to_end  <- ifelse(',logic_exp,",1,0)",sep="")
    print(text_all)
    eval(parse(text = text_all))
    dt <- cbind(variable_start_to_end,dt)
  }

  dt[is.na(dt[,"variable_start_to_end"]),"variable_start_to_end"] <- validator$validator_source_all_missing
  print(dt$variable_start_to_end )
  ##check if variable_start_to_end have only one level.
  if(length(unique(stats::na.omit(dt$variable_start_to_end))) <= 1){
    print(dt$variable_start_to_end)
    stop("generated validator target of type to01 has only 1 or 0 unique value")
  }
  return(dt)
}

