#' Construct a Validator
#'
#' @return return a validator object
#' @export
#'
validator <- function(validator_cutpoint = NULL,
                      validator_cutpoint_sign = NULL,
                      validator_cutpoint_max_min_mean = NULL,
                      predicted_cluster_combination = NULL,
                      predicted_cluster_n = NULL,
                      validator_source_variables,
                      listwise_deletion_variables = NULL,
                      validator_type,
                      validator_position = NULL,
                      flip_outcome_type = NULL,
                      flipped_predictors_variables = NULL,
                      flipped_predictors_cluster = NULL,
                      flipped_predictors_pp = NULL,
                      supervised_model = "logistic",
                      logistic = TRUE,
                      alpha = 1,
                      lambda = 0,
                      seed_num = c(seed_num_split = NA,
                                   seed_num_kfold = NA,
                                   seed_num_supervised_model = NA),
                      validator_source_all_missing = 0){
  thresholds <- validator_cutpoint
  signs <- validator_cutpoint_sign
  max_min_mean <- validator_cutpoint_max_min_mean
  validator_variables <- validator_source_variables

  if(!is.null(validator_position)){
    if(tolower(trimws(validator_position)) == "predicted"){
      flip_outcome_type = validator_type
      validator_type = "flip"
      flipped_predictors_cluster = TRUE
      flipped_predictors_pp = FALSE
    }else{
      if(tolower(trimws(validator_type)) == "binary"){
        validator_type = "direct"
      }else if(tolower(trimws(validator_type)) == "cutpoint"){
        validator_type = "to01"
      }else if(tolower(trimws(validator_type)) == "combination"){
        validator_type = "direct"
      }
    }
  }else{
    if(tolower(trimws(validator_type)) == "binary"){
      validator_type = "direct"
    }else if(tolower(trimws(validator_type)) == "cutpoint"){
      validator_type = "to01"
    }else if(tolower(trimws(validator_type)) == "combination"){
      validator_type = "direct"
    }
  }



  ##if thresholds length is 1 and validator_variables length > 1
  ## replicate thresholds to length(validator_variables) thresholds
  # if(length(thresholds) == 1 & length(validator_variables) > 1){
  #   thresholds <- rep(thresholds,length(validator_variables))
  # }
  if(!logistic){
    supervised_model <- "no model"
  }
  if(!is.null(predicted_cluster_combination)){
    predicted_cluster_combination <- strsplit(predicted_cluster_combination," |,")[[1]]
    m <- length(predicted_cluster_combination)
    predicted_cluster_combination <- paste(paste("P",which(predicted_cluster_n %in% predicted_cluster_combination),sep = ""),collapse="")
    print(predicted_cluster_combination)
    predicted_cluster_combination <- paste("C",
                                           m,
                                           "No",
                                           9,
                                           "comb",
                                           predicted_cluster_combination,
                                           sep='')
    predicted_cluster_n <- length(predicted_cluster_n)
  }

  # GT, LT, GE, LE, EQ,

  signs <- unlist(sapply(signs, FUN = function(x){
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

  validate_validator(new_validator(thresholds = thresholds,
                                   signs = signs,
                                   max_min_mean = max_min_mean,
                                   predicted_cluster_combination = predicted_cluster_combination,
                                   predicted_cluster_n = predicted_cluster_n,
                                   validator_variables = validator_variables,
                                   listwise_deletion_variables = listwise_deletion_variables,
                                   validator_type = validator_type,
                                   flip_outcome_type = flip_outcome_type,
                                   flipped_predictors_variables = flipped_predictors_variables,
                                   flipped_predictors_cluster = flipped_predictors_cluster,
                                   flipped_predictors_pp = flipped_predictors_pp,
                                   supervised_model = supervised_model,
                                   alpha = alpha,
                                   lambda = lambda,
                                   seed_num = seed_num,
                                   validator_source_all_missing = validator_source_all_missing))
}

