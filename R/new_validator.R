##constructor
new_validator <- function(thresholds,
                          signs,
                          max_min_mean,
                          predicted_cluster_combination = NULL,
                          predicted_cluster_n = NULL,
                          validator_variables,
                          listwise_deletion_variables = NULL,
                          validator_type,
                          flip_outcome_type,
                          flipped_predictors_variables = NULL,
                          flipped_predictors_cluster = NULL,
                          flipped_predictors_pp = NULL,
                          supervised_model,
                          alpha,
                          lambda,
                          seed_num,
                          validator_source_all_missing,
                          contVarName){
  if(!sjmisc::is_empty(flip_outcome_type)) stopifnot(is.character(flip_outcome_type) && identical(length(flip_outcome_type), 1L))
  stopifnot(is.character(validator_variables))
  stopifnot(is.character(validator_type))
  stopifnot(is.character(supervised_model))
  stopifnot(sjmisc::is_empty(signs)|is.character(signs))
  stopifnot(sjmisc::is_empty(max_min_mean)|is.character(max_min_mean))
  stopifnot((length(seed_num) == 3))
  #stopifnot(length(predicted_cluster_combination) <= 1)
  stopifnot(length(predicted_cluster_n) <= 1)
  ##one validator can only have one type
  stopifnot(identical(length(validator_type), 1L))
  ##validator type couldn't be NA
  stopifnot(!is.na(validator_type))
  stopifnot(identical(listwise_deletion_variables, NULL)|is.character(listwise_deletion_variables))
  class_type <- paste("validator",validator_type,sep = "_")
  structure(list(thresholds = thresholds,
                 signs = signs,
                 max_min_mean = max_min_mean,
                 predicted_cluster_combination = predicted_cluster_combination,
                 predicted_cluster_n = predicted_cluster_n,
                 validator_variables = validator_variables,
                 flip_outcome_type = flip_outcome_type,
                 listwise_deletion_variables = listwise_deletion_variables,
                 flipped_predictors_variables = flipped_predictors_variables,
                 flipped_predictors_cluster = flipped_predictors_cluster,
                 flipped_predictors_pp = flipped_predictors_pp,
                 supervised_model = supervised_model,
                 alpha = alpha,
                 lambda = lambda,
                 seed_num = seed_num,
                 validator_source_all_missing = validator_source_all_missing,
                 contVarName = contVarName),
            class = class_type)
}
