calculateMetricsForAProbSplitNoCV.validator_continuous <- function(validator,
                                                             pseudo_a_prob,
                                                             r_pseudo,
                                                             input_and_pp_and_var_df,
                                                             seed_num,
                                                             validation_data_fraction,
                                                             train_ind,
                                                             lr_maxiter,
                                                             n,
                                                             pcd_dropping_pct){
  validator$n <- n
  predictor_part <- predictors(validator)

  alpha <- validator$alpha
  lambda <- validator$lambda
  supervised_model <- validator$supervised_model
  #print(predictor_part)
  #set.seed(seed_num)
  pseudo_a_prob_train <- pseudo_a_prob[train_ind,]
  pseudo_a_prob_test <- pseudo_a_prob[!rownames(pseudo_a_prob) %in% train_ind,]
  input_dt_train <- input_and_pp_and_var_df[train_ind,]
  input_dt_test <- input_and_pp_and_var_df[!rownames(input_and_pp_and_var_df)%in%train_ind,]
  ##results with CV
  set.seed(seed_num['seed_num_kfold'])
  res_matrix <- c()
  if(length(pcd_dropping_pct) == 1){
    pcd_dropping_pct = rep(pcd_dropping_pct,3)
  }
  r_pseudo_threshold = round(pcd_dropping_pct[1]*r_pseudo)

  MSE = matrix(NA,1,r_pseudo)
  RMSE = matrix(NA,1,r_pseudo)
  MAE = matrix(NA,1,r_pseudo)
  r_square = matrix(NA,1,r_pseudo)
  adj_r_square = matrix(NA,1,r_pseudo)
  aic = matrix(NA,1,r_pseudo)
  coefficients = data.frame()

  if(!sjmisc::is_empty(seed_num['seed_num_regression_model'])){
    set.seed(seed_num['seed_num_regression_model'])
  }
  for(j in 1:r_pseudo){
    input_dt_train$cluster <- pseudo_a_prob_train[,j]
    if(supervised_model %in% c("linear regression", "lr")){
      metrics_ij <- genLinearRegressionOpt(y = input_dt_train[,"variable_start_to_end"],
                                          predictor_part = predictor_part,
                                          input_and_pp_and_var_df = input_dt_train)
    }
    coeff_df <- metrics_ij[[3]]
    coeff_df$covariates <- row.names(coeff_df)
    coefficients <- rbind(coefficients,coeff_df)
    metrics_ij <- metrics_ij[[1]]
    MSE[1,j] <- metrics_ij[1]
    RMSE[1,j] <- metrics_ij[2]
    MAE[1,j] <- metrics_ij[3]
    r_square[1,j] <- metrics_ij[4]
    adj_r_square[1,j] <- metrics_ij[5]
    aic[1,j] <- metrics_ij[6]
  }
  print(coefficients)
  coefficients <- coefficients %>% group_by(covariates) %>% summarise(mean=mean(Estimate), SD=sd(Estimate), SE = mean(`Std. Error`))
  if(sum(is.na(MSE[1,])) > r_pseudo_threshold){
    MSE_m <- NA
    MSE_d <- NA
  }else{
    MSE_m <- mean(MSE,na.rm = TRUE)
    MSE_d <- (var(MSE[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(MSE[1,]))))^.5
  }
  if(sum(is.na(RMSE[1,])) > r_pseudo_threshold){
    RMSE_m <- NA
    RMSE_d <- NA
  }else{
    RMSE_m <- mean(RMSE,na.rm = TRUE)
    RMSE_d <- (var(RMSE[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(RMSE[1,]))))^.5
  }
  if(sum(is.na(MAE[1,])) > r_pseudo_threshold){
    MAE_m <- NA
    MAE_d <- NA
  }else{
    MAE_m <- mean(MAE,na.rm = TRUE)
    MAE_d <- (var(MAE[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(MAE[1,]))))^.5
  }
  if(sum(is.na(r_square[1,])) > r_pseudo_threshold){
    r_square_m <- NA
    r_square_d <- NA
  }else{
    r_square_m <- mean(r_square,na.rm = TRUE)
    r_square_d <- (var(r_square[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(r_square[1,]))))^.5
  }
  if(sum(is.na(adj_r_square[1,])) > r_pseudo_threshold){
    adj_r_square_m <- NA
    adj_r_square_d <- NA
  }else{
    adj_r_square_m <- mean(adj_r_square,na.rm = TRUE)
    adj_r_square_d <- (var(adj_r_square[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(adj_r_square[1,]))))^.5
  }
  if(sum(is.na(aic[1,])) > r_pseudo_threshold){
    aic_m <- NA
    aic_d <- NA
  }else{
    aic_m <- mean(aic,na.rm = TRUE)
    aic_d <- (var(aic[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(aic[1,]))))^.5
  }

  res_vec <-
    c(MSE_m,
      MSE_d,
      RMSE_m,
      RMSE_d,
      MAE_m,
      MAE_d,
      r_square_m,
      r_square_d,
      adj_r_square_m,
      adj_r_square_d,
      aic_m,
      aic_d)
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

  ##results without CV
  MSE_test=matrix(NA,1,r_pseudo)
  RMSE_test=matrix(NA,1,r_pseudo)
  MAE_test=matrix(NA,1,r_pseudo)
  r_square_test=matrix(NA,1,r_pseudo)
  adj_r_square_test=matrix(NA,1,r_pseudo)
  aic_test=matrix(NA,1,r_pseudo)

  dt_y_test <- data.frame()
  for(j in 1:r_pseudo){

    input_dt_train$cluster <- pseudo_a_prob_train[,j]
    input_dt_test$cluster <- pseudo_a_prob_test[,j]

    if(supervised_model %in% c("linear regression", "lr")){
      metrics_j <- genLinearRegressionNoCV(y = input_dt_train[,"variable_start_to_end"],
                                          predictor_part = predictor_part,
                                          input_and_pp_and_var_df = input_dt_train,
                                          y_test = input_dt_test[,"variable_start_to_end"],
                                          input_and_pp_and_var_df_test = input_dt_test)
    }

    dt_y_test_tmp <- metrics_j[[2]]
    dt_y_test_tmp$pcd <- j
    dt_y_test <- rbind(dt_y_test,dt_y_test_tmp)

    metrics_j <- metrics_j[[1]]
    MSE_test[1,j] <- metrics_j[1]
    RMSE_test[1,j] <- metrics_j[2]
    MAE_test[1,j] <- metrics_j[3]
    r_square_test[1,j] <- metrics_j[4]
    adj_r_square_test[1,j] <- metrics_j[5]
    aic_test[1,j] <- metrics_j[6]
  }
  if(sum(is.na(MSE_test)) > r_pseudo_threshold){
    MSE_m_test <- NA
    MSE_d_test <- NA
  }else{
    MSE_m_test <- mean(MSE_test,na.rm = TRUE)
    MSE_d_test <- (var(MSE_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(MSE_test[1,]))))^.5
  }

  if(sum(is.na(RMSE_test)) > r_pseudo_threshold){
    RMSE_m_test <- NA
    RMSE_d_test <- NA
  }else{
    RMSE_m_test <- mean(RMSE_test,na.rm = TRUE)
    RMSE_d_test <- (var(RMSE_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(RMSE_test[1,]))))^.5
  }

  if(sum(is.na(MAE_test)) > r_pseudo_threshold){
    MAE_m_test <- NA
    MAE_d_test <- NA
  }else{
    MAE_m_test <- mean(MAE_test,na.rm = TRUE)
    MAE_d_test <- (var(MAE_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(MAE_test[1,]))))^.5
  }

  if(sum(is.na(r_square_test)) > r_pseudo_threshold){
    r_square_m_test <- NA
    r_square_d_test <- NA
  }else{
    r_square_m_test <- mean(r_square_test,na.rm = TRUE)
    r_square_d_test <- (var(r_square_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(r_square_test[1,]))))^.5
  }

  if(sum(is.na(adj_r_square_test)) > r_pseudo_threshold){
    adj_r_square_m_test <- NA
    adj_r_square_d_test <- NA
  }else{
    adj_r_square_m_test <- mean(adj_r_square_test,na.rm = TRUE)
    adj_r_square_d_test <- (var(adj_r_square_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(adj_r_square_test[1,]))))^.5
  }

  if(sum(is.na(aic_test)) > r_pseudo_threshold){
    aic_m_test <- NA
    aic_d_test <- NA
  }else{
    aic_m_test <- mean(aic_test,na.rm = TRUE)
    aic_d_test <- (var(aic_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(aic_test[1,]))))^.5
  }

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
  res_vec <- c(res_vec,res_vec_test)
  return(list(res_vec,NULL,dt_y_test,coefficients=coefficients))
}
