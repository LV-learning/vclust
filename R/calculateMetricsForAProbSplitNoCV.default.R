calculateMetricsForAProbSplitNoCV <- function(validator,
                                              pseudo_a_prob,
                                              r_pseudo,
                                              input_and_pp_and_var_df,
                                              seed_num,
                                              validation_data_fraction,
                                              train_ind,
                                              lr_maxiter,
                                              n,
                                              pcd_dropping_pct){
  UseMethod("calculateMetricsForAProbSplitNoCV")
}

#' @importFrom  stats aggregate var
calculateMetricsForAProbSplitNoCV.default <- function(validator,
                                                      pseudo_a_prob,
                                                      r_pseudo,
                                                      input_and_pp_and_var_df,
                                                      seed_num,
                                                      validation_data_fraction,
                                                      train_ind,
                                                      lr_maxiter,
                                                      n,
                                                      pcd_dropping_pct){

  predictor_part <- predictors(validator)
  supervised_model <- validator$supervised_model
  #set.seed(seed_num)
  pseudo_a_prob_train <- pseudo_a_prob[train_ind,]
  pseudo_a_prob_test <- pseudo_a_prob[!rownames(pseudo_a_prob) %in% train_ind,]
  ##results with CV
  set.seed(seed_num['seed_num_kfold'])
  res_matrix <- c()
  if(length(pcd_dropping_pct) == 1){
    pcd_dropping_pct = rep(pcd_dropping_pct,3)
  }
  r_pseudo_threshold = round(pcd_dropping_pct[1]*r_pseudo)

  accgmc12=matrix(NA,1,r_pseudo)
  aucmc12=matrix(NA,1,r_pseudo)
  sensgmc12=matrix(NA,1,r_pseudo)
  spcgmc12=matrix(NA,1,r_pseudo)
  kappamc12=matrix(NA,1,r_pseudo)
  res_roc_train <- data.frame()
  res_roc_test <- data.frame()
  if(!sjmisc::is_empty(seed_num['seed_num_supervised_model'])){
    set.seed(seed_num['seed_num_supervised_model'])
  }
  for(j in 1:r_pseudo){
    if (supervised_model %in% c("logistic regression", "logistic")){
      metrics_ij <- genLogisticMetricsOpt(y = pseudo_a_prob_train[,j],
                                          predictor_part = predictor_part,
                                          input_and_pp_and_var_df = input_and_pp_and_var_df[train_ind,],
                                          lr_maxiter = lr_maxiter)
    }else if (supervised_model %in% c(NULL,"no model")){
      metrics_ij <- genMetricsABinaryXOpt(y = pseudo_a_prob_train[,j],
                                          predictor_part = predictor_part,
                                          input_and_pp_and_var_df = input_and_pp_and_var_df[train_ind,],
                                          lr_maxiter = lr_maxiter)
    }

    res_roc_train <- rbind(res_roc_train, metrics_ij[[2]])
    metrics_ij <- metrics_ij[[1]]
    accgmc12[1,j] <- metrics_ij[1]
    aucmc12[1,j] <- metrics_ij[2]
    sensgmc12[1,j] <- metrics_ij[3]
    spcgmc12[1,j] <- metrics_ij[4]
    kappamc12[1,j] <- metrics_ij[5]
  }
  res_roc_train <- aggregate(res_roc_train[,c("TPR","FPR")],by=list(res_roc_train$threshold),mean)
  if(sum(is.na(accgmc12[1,])) > r_pseudo_threshold){
    acc_m <- NA
    acc_d <- NA
  }else{
    acc_m <- mean(accgmc12,na.rm = TRUE)
    acc_d <- (var(accgmc12[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(accgmc12[1,]))))^.5
  }
  if(sum(is.na(aucmc12[1,])) > r_pseudo_threshold){
    auc_m <- NA
    auc_d <- NA
  }else{
    auc_m <- mean(aucmc12,na.rm = TRUE)
    auc_d <- (var(aucmc12[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(aucmc12[1,]))))^.5
  }
  if(sum(is.na(sensgmc12[1,])) > r_pseudo_threshold){
    sensg_m <- NA
    sensg_d <- NA
  }else{
    sensg_m <- mean(sensgmc12,na.rm = TRUE)
    sensg_d <- (var(sensgmc12[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(sensgmc12[1,]))))^.5
  }
  if(sum(is.na(spcgmc12[1,])) > r_pseudo_threshold){
    spcg_m <- NA
    spcg_d <- NA
  }else{
    spcg_m <- mean(spcgmc12,na.rm = TRUE)
    spcg_d <- (var(spcgmc12[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(spcgmc12[1,]))))^.5
  }
  if(sum(is.na(kappamc12[1,])) > r_pseudo_threshold){
    kappa_m <- NA
    kappa_d <- NA
  }else{
    kappa_m <- mean(kappamc12,na.rm = TRUE)
    kappa_d <- (var(kappamc12[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(kappamc12[1,]))))^.5
  }
  res_vec <- c(acc_m,acc_d,auc_m,auc_d,sensg_m,sensg_d,spcg_m,spcg_d,kappa_m,kappa_d)

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
  ##results without CV
  accgmc_test=matrix(NA,1,r_pseudo)
  aucmc_test=matrix(NA,1,r_pseudo)
  sensgmc_test=matrix(NA,1,r_pseudo)
  spcgmc_test=matrix(NA,1,r_pseudo)
  kappamc_test=matrix(NA,1,r_pseudo)
  for(j in 1:r_pseudo){
    if (supervised_model %in% c("logistic regression", "logistic")){
      metrics_j <- genLogisticMetricsNoCV(y = pseudo_a_prob_train[,j],
                                          predictor_part = predictor_part,
                                          input_and_pp_and_var_df = input_and_pp_and_var_df[train_ind,],
                                          y_test = pseudo_a_prob_test[,j],
                                          input_and_pp_and_var_df_test =
                                            input_and_pp_and_var_df[!rownames(input_and_pp_and_var_df)%in%train_ind,],
                                          lr_maxiter = lr_maxiter)
    }else if (supervised_model %in% c(NULL,"no model")){
      metrics_j <- genMetricsABinaryXNoCV(y = pseudo_a_prob_train[,j],
                                          predictor_part = predictor_part,
                                          input_and_pp_and_var_df = input_and_pp_and_var_df[train_ind,],
                                          y_test = pseudo_a_prob_test[,j],
                                          input_and_pp_and_var_df_test =
                                            input_and_pp_and_var_df[!rownames(input_and_pp_and_var_df)%in%train_ind,],
                                          lr_maxiter = lr_maxiter)
    }

    res_roc_test <- rbind(res_roc_test,metrics_j[[2]])
    metrics_j <- metrics_j[[1]]
    accgmc_test[1,j] <- metrics_j[1]
    aucmc_test[1,j] <- metrics_j[2]
    sensgmc_test[1,j] <- metrics_j[3]
    spcgmc_test[1,j] <- metrics_j[4]
    kappamc_test[1,j] <- metrics_j[5]
  }
  res_roc_test <- aggregate(res_roc_test[,c("TPR","FPR")],by=list(res_roc_test$threshold),mean)
  if(sum(is.na(accgmc_test[1,])) > r_pseudo_threshold){
    acc_m_test <- NA
    acc_d_test <- NA
  }else{
    acc_m_test <- mean(accgmc_test,na.rm = TRUE)
    acc_d_test <- (var(accgmc_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(accgmc_test[1,]))))^.5
  }

  if(sum(is.na(aucmc_test[1,])) > r_pseudo_threshold){
    auc_m_test <- NA
    auc_d_test <- NA
  }else{
    auc_m_test <- mean(aucmc_test,na.rm = TRUE)
    auc_d_test <- (var(aucmc_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(aucmc_test[1,]))))^.5
  }

  if(sum(is.na(sensgmc_test[1,])) > r_pseudo_threshold){
    sensg_m_test <- NA
    sensg_d_test <- NA
  }else{
    sensg_m_test <- mean(sensgmc_test,na.rm = TRUE)
    sensg_d_test <- (var(sensgmc_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(sensgmc_test[1,]))))^.5
  }
  if(sum(is.na(spcgmc_test[1,])) > r_pseudo_threshold){
    spcg_m_test <- NA
    spcg_d_test <- NA
  }else{
    spcg_m_test <- mean(spcgmc_test,na.rm = TRUE)
    spcg_d_test <- (var(spcgmc_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(spcgmc_test[1,]))))^.5
  }
  if(sum(is.na(kappamc_test[1,])) > r_pseudo_threshold){
    kappa_m_test <- NA
    kappa_d_test <- NA
  }else{
    kappa_m_test <- mean(kappamc_test,na.rm = TRUE)
    kappa_d_test <- (var(kappamc_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(kappamc_test[1,]))))^.5
  }

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
  res_vec <- c(res_vec,res_vec_test)
  res_roc_train$dataset <- "train"
  res_roc_test$dataset <- "test"
  return(list(res_vec,rbind(res_roc_train,res_roc_test)))
}
