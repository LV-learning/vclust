removeValVarNa.validator_direct <- function(validator,
                                            dt){
  if(is.null(validator$validator_variables)){
    warning("validator_variables is null, return the data frame without removement of NA/n")
    return(dt)
  }
  #check if validator variables has only one level
  tmp <- apply(dt[,validator$validator_variables],2,FUN=function(x)length(unique(x)))
  names_less1 <- names(tmp[tmp <= 1])
  if(!identical(names_less1, character(0))){
    stop(paste("column",paste(names_less1, collapse = " "),"has/have 1 or 0 unique value"))
  }
  dt
}
