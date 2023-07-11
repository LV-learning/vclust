#' @importFrom  stats aggregate var
calculateMetricsForAProbNoPCDCategoryNoCV.validator_flip <- function(validator,
                                                                     categoryVec,
                                                                     input_and_pp_and_var_df,
                                                                     seed_num,
                                                                     lr_maxiter,
                                                                     n,
                                                                     pcd_dropping_pct){

  #categoryVec <- as.factor(categoryVec)
  ##handle some NAs
  validator$n <- n
  predictor_part <- predictors(validator)
  alpha <- validator$alpha
  lambda <- validator$lambda
  supervised_model <- validator$supervised_model
  if(!sjmisc::is_empty(seed_num['seed_num_supervised_model'])){
    set.seed(seed_num['seed_num_supervised_model'])
  }
  roc_res <- data.frame()
  input_and_pp_and_var_df$cluster <- categoryVec
  if(supervised_model == "glmnet"){
    metrics_ij <- genGlmnetMetricsOpt(y = input_and_pp_and_var_df[,"variable_start_to_end"],
                                      predictor_part = predictor_part,
                                      input_and_pp_and_var_df = input_and_pp_and_var_df,
                                      lr_maxiter = lr_maxiter,
                                      alpha = alpha,
                                      lambda = lambda)
  }else if(supervised_model %in% c("logistic regression", "logistic")){
    metrics_ij <- genLogisticMetricsOpt(y = input_and_pp_and_var_df[,"variable_start_to_end"],
                                        predictor_part = predictor_part,
                                        input_and_pp_and_var_df = input_and_pp_and_var_df,
                                        lr_maxiter = lr_maxiter)
  }else if(supervised_model %in% c(NULL,"no model")){
    metrics_ij <- genMetricsABinaryXOpt(y = input_and_pp_and_var_df[,"variable_start_to_end"],
                                        predictor_part = predictor_part,
                                        input_and_pp_and_var_df = input_and_pp_and_var_df,
                                        lr_maxiter = lr_maxiter)

  }
  roc_res <- rbind(roc_res, metrics_ij[[2]])
  metrics_ij <- metrics_ij[[1]]
  acc_m <- metrics_ij[1]
  auc_m <- metrics_ij[2]
  sensg_m <- metrics_ij[3]
  spcg_m <- metrics_ij[4]
  kappa_m <- metrics_ij[5]
  res_vec <- c(acc_m,NA,auc_m,NA,sensg_m,NA,spcg_m,NA,kappa_m,NA)
  names(res_vec) <- c('acc_m',
                      'acc_d',
                      'auc_m',
                      'auc_d',
                      'sensg_m',
                      'sensg_d',
                      'spcg_m',
                      'spcg_d',
                      'kappa_m',
                      'kappa_d')
  roc_res <-
    aggregate(roc_res[, c("TPR", "FPR")], by = list(roc_res$threshold), mean)
  return(list(res_vec, roc_res))

}
