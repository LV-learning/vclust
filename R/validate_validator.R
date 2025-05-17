validate_validator <- function(x){
  print(x)
  arguments <- unclass(x)
  print(arguments$flip_outcome_type)
  if(!sjmisc::is_empty(arguments$flip_outcome_type) && arguments$flip_outcome_type == 'binary'){
    stopifnot(identical(length(arguments$validator_variables),1L) )
  }
  ##thresholds should have same length as validator_variables
  if(!("validator_direct" %in% class(x)| "validator_flip" %in% class(x) | "validator_continuous" %in% class(x))){
    stopifnot(is.numeric(arguments$thresholds))
    # if(!identical(length(arguments$thresholds),length(arguments$validator_variables))){
    #   stop("thresholds should have same length as validator variables")
    # }
    ##thresholds and validator_variables and validator shouldn't have NA
    if(any(is.na(arguments$thresholds))){
      stop("thresholds type couldn't be NA")
    }
  }
  if(any(is.na(arguments$validator_variables))){
    stop("validator_variables type couldn't be NA")
  }
  if(("validator_flip" %in% class(x))){
    stopifnot(!all(is.null(arguments$flipped_predictors_variables),is.null(arguments$flipped_predictors_cluster),is.null(arguments$flipped_predictors_pp)))
    stopifnot(!all(is.null(arguments$flipped_predictors_variables),arguments$flipped_predictors_cluster == FALSE,arguments$flipped_predictors_pp == FALSE))
  }
  x
}
