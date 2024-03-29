calculateMetricsForAProbNoPCDCategoryNoCV.validator_continuous <- function(validator,
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
  if(!sjmisc::is_empty(seed_num['seed_num_regression_model'])){
    set.seed(seed_num['seed_num_regression_model'])
  }
  input_and_pp_and_var_df$cluster <- categoryVec
  if (supervised_model %in% c("linear regression", "lr")) {
    metrics_ij <- genLinearRegressionOpt(
      y = input_and_pp_and_var_df[,"variable_start_to_end"],
      predictor_part = predictor_part,
      input_and_pp_and_var_df = input_and_pp_and_var_df
    )
  }
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
  return(list(res_vec))
}
