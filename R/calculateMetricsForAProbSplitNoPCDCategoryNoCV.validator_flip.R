#' @importFrom  stats aggregate var
calculateMetricsForAProbSplitNoPCDCategoryNoCV.validator_flip <- function(validator,
                                                                          categoryVec,
                                                                          input_and_pp_and_var_df,
                                                                          seed_num,
                                                                          validation_data_fraction,
                                                                          train_ind,
                                                                          lr_maxiter,
                                                                          n,
                                                                          pcd_dropping_pct){
  validator$n <- n
  alpha <- validator$alpha
  lambda <- validator$lambda
  supervised_model <- validator$supervised_model
  predictor_part <- predictors(validator)
  cat_vec_train <- categoryVec[train_ind,1]
  cat_vec_test <- categoryVec[!rownames(categoryVec) %in% train_ind,1]
  input_dt_train <- input_and_pp_and_var_df[train_ind,]
  input_dt_test <- input_and_pp_and_var_df[!rownames(input_and_pp_and_var_df)%in%train_ind,]
  #print(input_dt_train[,paste("P",1:n,sep = "")])
  ##results with CV
  res_roc_train <- data.frame()
  res_roc_test <- data.frame()
  coefficients = data.frame()
  if(!sjmisc::is_empty(seed_num['seed_num_supervised_model'])){
    set.seed(seed_num['seed_num_supervised_model'])
  }
  input_dt_train$cluster <- cat_vec_train
  input_dt_test$cluster <- cat_vec_test
  if(supervised_model == "glmnet"){
    metrics_ij <- genGlmnetMetricsOpt(y = input_dt_train[,"variable_start_to_end"],
                                      predictor_part = predictor_part,
                                      input_and_pp_and_var_df = input_dt_train,
                                      lr_maxiter = lr_maxiter,
                                      alpha = alpha,
                                      lambda = lambda)
  }else if(supervised_model %in% c("logistic regression", "logistic")){
    metrics_ij <- genLogisticMetricsOpt(y = input_dt_train[,"variable_start_to_end"],
                                        predictor_part = predictor_part,
                                        input_and_pp_and_var_df = input_dt_train,
                                        lr_maxiter = lr_maxiter)
  }else if (supervised_model %in% c(NULL,"no model")){
    metrics_ij <- genMetricsABinaryXOpt(y = input_dt_train[,"variable_start_to_end"],
                                        predictor_part = predictor_part,
                                        input_and_pp_and_var_df = input_dt_train,
                                        lr_maxiter = lr_maxiter)
  }

  coeff_df <- metrics_ij[[3]]
  coeff_df$covariates <- row.names(coeff_df)
  coefficients <- rbind(coefficients,coeff_df)
  print(coefficients)
  coefficients <- coefficients %>% group_by(covariates) %>% summarise(mean=mean(Estimate), SD=sd(Estimate), SE = mean(`Std. Error`))
  res_roc_train <- rbind(res_roc_train, metrics_ij[[2]])
  res_roc_train <-
    aggregate(res_roc_train[, c("TPR", "FPR")], by = list(res_roc_train$threshold), mean)
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
  if(supervised_model == "glmnet"){
    metrics_j <- genGlmnetMetricsNoCV(y = input_dt_train[,"variable_start_to_end"],
                                      predictor_part = predictor_part,
                                      input_and_pp_and_var_df = input_dt_train,
                                      y_test = input_dt_test[,"variable_start_to_end"],
                                      input_and_pp_and_var_df_test = input_dt_test,
                                      lr_maxiter = lr_maxiter,
                                      alpha = alpha,
                                      lambda = lambda)
  }else if(supervised_model %in% c("logistic regression", "logistic")){
    metrics_j <- genLogisticMetricsNoCV(y = input_dt_train[,"variable_start_to_end"],
                                        predictor_part = predictor_part,
                                        input_and_pp_and_var_df = input_dt_train,
                                        y_test = input_dt_test[,"variable_start_to_end"],
                                        input_and_pp_and_var_df_test = input_dt_test,
                                        lr_maxiter = lr_maxiter)
  }else if (supervised_model %in% c(NULL,"no model")){
    metrics_j <- genMetricsABinaryXNoCV(y = input_dt_train[,"variable_start_to_end"],
                                        predictor_part = predictor_part,
                                        input_and_pp_and_var_df = input_dt_train,
                                        y_test = input_dt_test[,"variable_start_to_end"],
                                        input_and_pp_and_var_df_test = input_dt_test,
                                        lr_maxiter = lr_maxiter)
  }
  res_roc_test <- rbind(res_roc_test, metrics_j[[2]])
  res_roc_test <-
    aggregate(res_roc_test[, c("TPR", "FPR")], by = list(res_roc_test$threshold), mean)
  metrics_j <- metrics_j[[1]]
  accgmc_test <- metrics_j[1]
  aucmc_test <- metrics_j[2]
  sensgmc_test <- metrics_j[3]
  spcgmc_test <- metrics_j[4]
  kappamc_test <- metrics_j[5]
  #accurracy
  acc_m_test <- mean(accgmc_test)
  acc_d_test <- NA
  #auc
  auc_m_test <- mean(aucmc_test)
  auc_d_test <- NA
  #sensitivity
  sensg_m_test <- mean(sensgmc_test)
  sensg_d_test <- NA
  #specificity
  spcg_m_test <- mean(spcgmc_test)
  spcg_d_test <- NA
  #kappa
  #print(kappamc_test)
  kappa_m_test <- mean(kappamc_test,na.rm = TRUE)
  kappa_d_test <- NA

  res_vec_test <- c(acc_m_test,acc_d_test,auc_m_test,auc_d_test,sensg_m_test,sensg_d_test,spcg_m_test,spcg_d_test,kappa_m_test,kappa_d_test)
  names(res_vec_test) <- c('acc_m_test',
                           'acc_d_test',
                           'auc_m_test',
                           'auc_d_test',
                           'sensg_m_test',
                           'sensg_d_test',
                           'spcg_m_test',
                           'spcg_d_test',
                           'kappa_m_test',
                           'kappa_d_test'
  )
  res_vec <- c(res_vec, res_vec_test)
  res_roc_train$dataset <- "train"
  res_roc_test$dataset <- "test"
  return(list(res_vec, rbind(res_roc_train, res_roc_test),
              coefficients=coefficients))
}
