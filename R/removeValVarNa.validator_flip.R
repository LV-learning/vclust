removeValVarNa.validator_flip <- function(validator,
                                          dt){
  ##handle validator_variables == NULL
  if(is.null(validator$validator_variables)){
    warning("validator_variables is null, return the data frame without removement of NA/n")
    return(dt)
  }
  ##screen cases missing all validators
  dt <- dt[!apply(is.na(dt[, validator$validator_variables, drop = FALSE]), 1, all),]
  ##listwise deletion for flipped_predictors
  dt[stats::complete.cases(dt[,validator$flipped_predictors_variables[validator$flipped_predictors_variables %in% names(dt)]]),]
}
