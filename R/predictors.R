predictors <- function(validator){
  UseMethod("predictors")
}
predictors.validator_to01 <- function(validator){
  "variable_start_to_end"
}
predictors.validator_direct <- function(validator){
  paste(validator$validator_variables,collapse = "+")
}
predictors.validator_flip <- function(validator){
  predictor_part <- validator$flipped_predictors_variables
  if(validator$flipped_predictors_cluster == TRUE){
    predictor_part <- c(predictor_part,"cluster")
  }
  if(validator$flipped_predictors_pp == TRUE){
    predictor_part <- c(predictor_part,paste("P",1:validator$n,sep = ""))
  }
  paste(predictor_part,collapse = "+")
}
