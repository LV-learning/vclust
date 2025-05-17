calculateMetricsForAProbSplitNoPCDCategoryNoCV.validator_continuous <- function(validator,
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
  input_and_pp_and_var_df$cluster <- categoryVec
  res_roc_train <- data.frame()
  res_roc_test <- data.frame()
  if(!sjmisc::is_empty(seed_num['seed_num_supervised_model'])){
    set.seed(seed_num['seed_num_supervised_model'])
  }
  mean_sd_dt_train <- data.frame()
  mean_sd_dt_test <- data.frame()
  coefficients = data.frame()

  input_dt_train$cluster <- cat_vec_train
  input_dt_test$cluster <- cat_vec_test
  if(supervised_model %in% c("linear regression", "lr")){
    metrics_ij <- genLinearRegressionOpt(y = input_dt_train[,"variable_start_to_end"],
                                        predictor_part = predictor_part,
                                        input_and_pp_and_var_df = input_dt_train)
  }
  gsd <- var(input_and_pp_and_var_df[input_and_pp_and_var_df$cluster == 1,validator$contVarName], na.rm=TRUE)^0.5
  mean_sd_dt_train <- rbind(mean_sd_dt_train, data.frame(repeated = 1,
                                                         kfold = 1,
                                                         mean = metrics_ij[[2]],
                                                         sd = gsd,
                                                         n = length(input_and_pp_and_var_df[input_and_pp_and_var_df$cluster == 1,validator$contVarName]),
                                                         train_or_test = "train"
  ))
  coeff_df <- metrics_ij[[3]]
  coeff_df$covariates <- row.names(coeff_df)
  coefficients <- rbind(coefficients,coeff_df)
  print(coefficients)
  coefficients <- coefficients %>% group_by(covariates) %>% summarise(mean=mean(Estimate), SD=sd(Estimate), SE = mean(`Std. Error`))
  metrics_ij <- metrics_ij[[1]]
  MSE <- metrics_ij[1]
  RMSE <- metrics_ij[2]
  MAE <- metrics_ij[3]
  r_square <- metrics_ij[4]
  adj_r_square <- metrics_ij[5]
  aic <- metrics_ij[6]
  res_vec <- c(MSE,NA,RMSE,NA,MAE,NA,r_square,NA,adj_r_square,NA,aic,NA)
  names(res_vec) <- c(
    'MSE_m',
    'MSE_d',
    'RMSE_m',
    'RMSE_d',
    'MAE_m',
    'MAE_d',
    'r_square_m',
    'r_square_d',
    'adj_r_square_m',
    'adj_r_square_d',
    'aic_m',
    'aic_d'
  )
  if(supervised_model %in% c("linear regression", "lr")){
    metrics_j <- genLinearRegressionNoCV(y = input_dt_train[,"variable_start_to_end"],
                                        predictor_part = predictor_part,
                                        input_and_pp_and_var_df = input_dt_train,
                                        y_test = input_dt_test[,"variable_start_to_end"],
                                        input_and_pp_and_var_df_test = input_dt_test)
  }
  gsd <- var(input_and_pp_and_var_df[input_and_pp_and_var_df$cluster == 1,validator$contVarName], na.rm=TRUE)^0.5
  mean_sd_dt_test <- rbind(mean_sd_dt_test, data.frame(repeated = 1,
                                                       kfold = 1,
                                                       mean = metrics_j[[3]],
                                                       sd = gsd,
                                                       n = length(input_and_pp_and_var_df[input_and_pp_and_var_df$cluster == 1,validator$contVarName]),
                                                       train_or_test = "test"))
  dt_y_test <- metrics_j[[2]]
  metrics_j <- metrics_j[[1]]
  MSE_test <- metrics_j[1]
  RMSE_test <- metrics_j[2]
  MAE_test <- metrics_j[3]
  r_square_test <- metrics_j[4]
  adj_r_square_test <- metrics_j[5]
  aic_test <- metrics_j[6]

  #accurracy
  MSE_m_test <- mean(MSE_test)
  MSE_d_test <- NA
  #auc
  RMSE_m_test <- mean(RMSE_test)
  RMSE_d_test <- NA
  #sensitivity
  MAE_m_test <- mean(MAE_test)
  MAE_d_test <- NA
  #specificity
  r_square_m_test <- mean(r_square_test)
  r_square_d_test <- NA
  #kappa
  #print(kappamc_test)
  adj_r_square_m_test <- mean(adj_r_square_test, na.rm = TRUE)
  adj_r_square_d_test <- NA
  aic_m_test <- mean(aic_test)
  aic_d_test <- NA

  res_vec_test <- c(MSE_m_test,
                    MSE_d_test,
                    RMSE_m_test,
                    RMSE_d_test,
                    MAE_m_test,
                    MAE_d_test,
                    r_square_m_test,
                    r_square_d_test,
                    adj_r_square_m_test,
                    adj_r_square_d_test,
                    aic_m_test,
                    aic_d_test)
  names(res_vec_test) <- c('MSE_m_test',
                           'MSE_d_test',
                           'RMSE_m_test',
                           'RMSE_d_test',
                           'MAE_m_test',
                           'MAE_d_test',
                           'r_square_m_test',
                           'r_square_d_test',
                           'adj_r_square_m_test',
                           'adj_r_square_d_test',
                           'aic_m_test',
                           'aic_d_test'
  )
  res_vec <- c(res_vec, res_vec_test)
  return(list(list(res_matrix=as.data.frame(t(res_vec)),
                   mean_sd_dt=rbind(mean_sd_dt_train, mean_sd_dt_test),
                   coefficients=coefficients),
              NULL,
              dt_y_test))
}
