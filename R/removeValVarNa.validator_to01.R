removeValVarNa <- function(validator,
                           dt){
  UseMethod("removeValVarNa")
}
removeValVarNa.validator_to01 <- function(validator,
                                          dt){
  ##handle validator_variables == NULL
  if(is.null(validator$validator_variables)){
    warning("validator_variables is null, return the data frame without removement of NA/n")
    return(dt)
  }
  ##screen cases missing all validators
  dt[!apply(is.na(dt[, validator$validator_variables, drop = FALSE]), 1, all),]
}
