#' @importFrom  stats aggregate var
calculateMetricsForAProbNoCV.validator_flip <- function(validator,
                                                        pseudo_a_prob,
                                                        r_pseudo,
                                                        input_and_pp_and_var_df,
                                                        seed_num,
                                                        lr_maxiter,
                                                        n,
                                                        pcd_dropping_pct){
  ##handle some NAs
  validator$n <- n
  predictor_part <- predictors(validator)
  #print(predictor_part)
  alpha <- validator$alpha
  lambda <- validator$lambda
  supervised_model <- validator$supervised_model
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
  roc_res <- data.frame()
  if(!sjmisc::is_empty(seed_num['seed_num_supervised_model'])){
    set.seed(seed_num['seed_num_supervised_model'])
  }
  for(j in 1:r_pseudo){
    input_and_pp_and_var_df$cluster <- pseudo_a_prob[,j]

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
    roc_res <- rbind(roc_res,metrics_ij[[2]])
    metrics_ij <- metrics_ij[[1]]
    accgmc12[1,j] <- metrics_ij[1]
    aucmc12[1,j] <- metrics_ij[2]
    sensgmc12[1,j] <- metrics_ij[3]
    spcgmc12[1,j] <- metrics_ij[4]
    kappamc12[1,j] <- metrics_ij[5]
  }
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
  roc_res <- aggregate(roc_res[,c("TPR","FPR")],by=list(roc_res$threshold),mean)
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

  return(list(res_vec,roc_res))
}
