#' @importFrom  stats aggregate var
calculateMetricsForAProbSplit.validator_direct <- function(validator,
                                                           pseudo_a_prob,
                                                           r_pseudo,
                                                           K_fold,
                                                           repeated_folds_R,
                                                           input_and_pp_and_var_df,
                                                           seed_num,
                                                           validation_data_fraction,
                                                           train_ind,
                                                           lr_maxiter,
                                                           n,
                                                           pcd_dropping_pct){

  predictor_part <- predictors(validator)
  alpha <- validator$alpha
  lambda <- validator$lambda
  supervised_model <- validator$supervised_model
  #set.seed(seed_num)
  pseudo_a_prob_train <- pseudo_a_prob[train_ind,]
  pseudo_a_prob_test <- pseudo_a_prob[!rownames(pseudo_a_prob) %in% train_ind,]
  ##results with CV
  set.seed(seed_num['seed_num_kfold'])
  myfold <- cvTools::cvFolds(length(train_ind), K_fold, R = repeated_folds_R)
  res_matrix <- c()
  res_roc_train <- data.frame()
  if(length(pcd_dropping_pct) == 1){
    pcd_dropping_pct = rep(pcd_dropping_pct,3)
  }
  r_pseudo_threshold = round(pcd_dropping_pct[1]*r_pseudo)
  K_fold_threshold = round(pcd_dropping_pct[2]*K_fold)
  r_threshold = round(pcd_dropping_pct[3]*repeated_folds_R)
  if(!sjmisc::is_empty(seed_num['seed_num_supervised_model'])){
    set.seed(seed_num['seed_num_supervised_model'])
  }
  acc_m_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)
  acc_v_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)

  auc_m_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)
  auc_v_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)

  sensg_m_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)
  sensg_v_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)

  spcg_m_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)
  spcg_v_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)

  kappa_m_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)
  kappa_v_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)
  coefficients = data.frame()
  for(r in 1:repeated_folds_R){
    accgmc12=matrix(NA,K_fold,r_pseudo)
    aucmc12=matrix(NA,K_fold,r_pseudo)
    sensgmc12=matrix(NA,K_fold,r_pseudo)
    spcgmc12=matrix(NA,K_fold,r_pseudo)
    kappamc12=matrix(NA,K_fold,r_pseudo)

    for(i in 1:K_fold){
      for(j in 1:r_pseudo){

        if(supervised_model == "glmnet"){
          metrics_ij <- genGlmnetMetrics(cvFolds_dt = myfold,
                                         y = pseudo_a_prob_train[,j],
                                         predictor_part = predictor_part,
                                         cur_fold = i,
                                         cur_r = r,
                                         input_and_pp_and_var_df = input_and_pp_and_var_df[train_ind,],
                                         lr_maxiter = lr_maxiter,
                                         alpha = alpha,
                                         lambda = lambda)
        }else if(supervised_model %in% c("logistic regression", "logistic")){
          metrics_ij <- genLogisticMetrics(cvFolds_dt = myfold,
                                           y = pseudo_a_prob_train[,j],
                                           predictor_part = predictor_part,
                                           cur_fold = i,
                                           cur_r = r,
                                           input_and_pp_and_var_df = input_and_pp_and_var_df[train_ind,],
                                           lr_maxiter = lr_maxiter)
        }else if (supervised_model %in% c(NULL,"no model")){
          metrics_ij <- genMetricsABinaryX(cvFolds_dt = myfold,
                                           y = pseudo_a_prob_train[,j],
                                           predictor_part = predictor_part,
                                           cur_fold = i,
                                           cur_r = r,
                                           input_and_pp_and_var_df = input_and_pp_and_var_df[train_ind,],
                                           lr_maxiter = lr_maxiter)
        }
        coeff_df <- metrics_ij[[3]]
        coeff_df$covariates <- row.names(coeff_df)
        coefficients <- rbind(coefficients,coeff_df)
        res_roc_train <- rbind(res_roc_train,metrics_ij[[2]])
        metrics_ij <- metrics_ij[[1]]

        accgmc12[i,j] <- metrics_ij[1]
        aucmc12[i,j] <- metrics_ij[2]
        sensgmc12[i,j] <- metrics_ij[3]
        spcgmc12[i,j] <- metrics_ij[4]
        kappamc12[i,j] <- metrics_ij[5]
      }
    }
    print("sensitivity is")
    print(sensgmc12)
    print("specificity is")
    print(spcgmc12)
    sensitivity_na_count_k  <- apply(sensgmc12, 2, FUN = function(x){sum(is.na(x))})
    specification_na_count_k <- apply(spcgmc12, 2, FUN = function(x){sum(is.na(x))})
    accuracy_na_count_k <- apply(accgmc12, 2, FUN = function(x){sum(is.na(x))})
    auc_na_count_k <- apply(aucmc12, 2, FUN = function(x){sum(is.na(x))})
    kappa_na_count_k <- apply(kappamc12, 2, FUN = function(x){sum(is.na(x))})

    sensitivity_na_count_k_larger_id <- which(sensitivity_na_count_k > K_fold_threshold)
    specification_na_count_k_larger_id <- which(specification_na_count_k > K_fold_threshold)
    accuracy_na_count_k_larger_id <- which(accuracy_na_count_k > K_fold_threshold)
    auc_na_count_k_larger_id <- which(auc_na_count_k > K_fold_threshold)
    kappa_na_count_k_larger_id <- which(kappa_na_count_k > K_fold_threshold)

    acc_m_k <- colMeans(accgmc12,na.rm = TRUE)
    acc_m_k[accuracy_na_count_k_larger_id] <- NA
    acc_d_k <- apply(accgmc12,2,var,na.rm = TRUE)/(K_fold - apply(accgmc12,2,FUN=function(x)sum(is.na(x))))
    acc_d_k[accuracy_na_count_k_larger_id] <- NA

    auc_m_k <- colMeans(aucmc12,na.rm = TRUE)
    auc_m_k[auc_na_count_k_larger_id] <- NA
    auc_d_k <- apply(aucmc12,2,var,na.rm = TRUE)/(K_fold - apply(aucmc12,2,FUN=function(x)sum(is.na(x))))
    auc_d_k[auc_na_count_k_larger_id] <- NA

    sensg_m_k <- colMeans(sensgmc12,na.rm = TRUE)
    sensg_m_k[sensitivity_na_count_k_larger_id] <- NA
    sensg_d_k <- apply(sensgmc12,2,var,na.rm = TRUE)/(K_fold - apply(sensgmc12,2,FUN=function(x)sum(is.na(x))))
    sensg_d_k[sensitivity_na_count_k_larger_id] <- NA

    spcg_m_k <- colMeans(spcgmc12,na.rm = TRUE)
    spcg_m_k[specification_na_count_k_larger_id] <- NA
    spcg_d_k <- apply(spcgmc12,2,var,na.rm = TRUE)/(K_fold - apply(spcgmc12,2,FUN=function(x)sum(is.na(x))))
    spcg_d_k[specification_na_count_k_larger_id] <- NA

    kappa_m_k <- colMeans(kappamc12,na.rm = TRUE)
    kappa_m_k[kappa_na_count_k_larger_id] <- NA
    kappa_d_k <- apply(kappamc12,2,var,na.rm = TRUE)/(K_fold - apply(kappamc12,2,FUN=function(x)sum(is.na(x))))
    kappa_d_k[kappa_na_count_k_larger_id] <- NA
    print(kappa_m_k)
    #r_pseudo_threshold


    if(sum(is.na(acc_m_k)) > r_pseudo_threshold){
      acc_m <- NA
      acc_d <- NA
    }else{
      acc_m_perRepPCD[,r] <- (acc_m_k)
      acc_v_perRepPCD[,r] <- (acc_d_k)
      acc_m <- mean(acc_m_k,na.rm = TRUE)
      acc_d <- (mean(acc_d_k,na.rm = TRUE) + var(acc_m_k,na.rm = TRUE))^0.5
    }
    if(sum(is.na(auc_m_k)) > r_pseudo_threshold){
      auc_m <- NA
      acc_d <- NA
    }else{
      auc_m_perRepPCD[,r] <- (auc_m_k)
      auc_v_perRepPCD[,r] <- (auc_d_k)
      auc_m <- mean(auc_m_k,na.rm = TRUE)
      auc_d <- (mean(auc_d_k,na.rm = TRUE) + var(auc_m_k,na.rm = TRUE))^0.5
    }

    if(sum(is.na(sensg_m_k)) > r_pseudo_threshold){
      sensg_m <- NA
      sensg_d <- NA
    }else{
      sensg_m_perRepPCD[,r] <- (sensg_m_k)
      sensg_v_perRepPCD[,r] <- (sensg_d_k)

      sensg_m <- mean(sensg_m_k,na.rm = TRUE)
      sensg_d <- (mean(sensg_d_k,na.rm = TRUE) + var(sensg_m_k,na.rm = TRUE))^0.5
    }
    if(sum(is.na(spcg_m_k)) > r_pseudo_threshold){
      spcg_m <- NA
      spcg_d <- NA
    }else{
      spcg_m_perRepPCD[,r] <- (spcg_m_k)
      spcg_v_perRepPCD[,r] <- (spcg_d_k)
      spcg_m <- mean(spcg_m_k,na.rm=TRUE)
      spcg_d <- (mean(spcg_d_k,na.rm = TRUE) + var(spcg_m_k,na.rm = TRUE))^0.5
    }
    if(sum(is.na(kappa_m_k)) > r_pseudo_threshold){
      kappa_m <- NA
      kappa_d <- NA
    }else{
      kappa_m_perRepPCD[,r] <- (kappa_m_k)
      kappa_v_perRepPCD[,r] <- (kappa_d_k)
      kappa_m <- mean(kappa_m_k,na.rm=TRUE)
      kappa_d <- (mean(kappa_d_k,na.rm = TRUE) + var(kappa_m_k,na.rm = TRUE))^0.5
    }


    res_vec <- c(acc_m,acc_d,auc_m,auc_d,sensg_m,sensg_d,spcg_m,spcg_d,kappa_m,kappa_d)
    res_matrix <- c(res_matrix,res_vec)
  }
  print(coefficients)
  coefficients <- coefficients %>% group_by(covariates) %>% summarise(mean=mean(Estimate), SD=sd(Estimate), SE = mean(`Std. Error`))
  res_matrix <- data.frame(matrix(res_matrix, nrow = repeated_folds_R, byrow = TRUE))
  res_roc_train <- aggregate(res_roc_train[,c("TPR","FPR")],by=list(res_roc_train$threshold),mean)
  acc_m_overall=mean(acc_m_perRepPCD, na.rm = TRUE)
  acc_v_overall=(mean(acc_v_perRepPCD, na.rm = TRUE)+ var(c(acc_m_perRepPCD), na.rm = TRUE)/(r_pseudo*repeated_folds_R)) ^ 0.5

  auc_m_overall=mean(auc_m_perRepPCD, na.rm = TRUE)
  auc_v_overall=(mean(auc_v_perRepPCD, na.rm = TRUE)+ var(c(auc_m_perRepPCD), na.rm = TRUE)/(r_pseudo*repeated_folds_R)) ^ 0.5

  sensg_m_overall=mean(sensg_m_perRepPCD)
  sensg_v_overall=(mean(sensg_v_perRepPCD)+ var(c(sensg_m_perRepPCD), na.rm = TRUE)/(r_pseudo*repeated_folds_R)) ^ 0.5

  spcg_m_overall=mean(spcg_m_perRepPCD, na.rm = TRUE)
  spcg_v_overall=(mean(spcg_v_perRepPCD, na.rm = TRUE)+ var(c(spcg_m_perRepPCD), na.rm = TRUE)/(r_pseudo*repeated_folds_R)) ^ 0.5

  kappa_m_overall=mean(kappa_m_perRepPCD, na.rm = TRUE)
  kappa_v_overall=(mean( kappa_v_perRepPCD, na.rm = TRUE)+ var(c(kappa_m_perRepPCD), na.rm = TRUE)/(r_pseudo*repeated_folds_R)) ^ 0.5
  res_matrix <- c(acc_m_overall, acc_v_overall,
                  auc_m_overall, auc_v_overall,
                  sensg_m_overall, sensg_v_overall,
                  spcg_m_overall, spcg_v_overall,
                  kappa_m_overall, kappa_v_overall)
  names(res_matrix) <- c('acc_m',
                         'acc_d',
                         'auc_m',
                         'auc_d',
                         'sensg_m',
                         'sensg_d',
                         'spcg_m',
                         'spcg_d',
                         'kappa_m',
                         'kappa_d')
  #res_matrix <- colMeans(res_matrix,na.rm = TRUE)
  ##results without CV
  accgmc_test=matrix(NA,1,r_pseudo)
  aucmc_test=matrix(NA,1,r_pseudo)
  sensgmc_test=matrix(NA,1,r_pseudo)
  spcgmc_test=matrix(NA,1,r_pseudo)
  kappamc_test=matrix(NA,1,r_pseudo)
  res_roc_test <- data.frame()
  dt_y_test <- data.frame()
  for(j in 1:r_pseudo){
    if(supervised_model == "glmnet"){
      metrics_j <- genGlmnetMetricsNoCV(y = pseudo_a_prob_train[,j],
                                        predictor_part = predictor_part,
                                        input_and_pp_and_var_df = input_and_pp_and_var_df[train_ind,],
                                        y_test = pseudo_a_prob_test[,j],
                                        input_and_pp_and_var_df_test =
                                          input_and_pp_and_var_df[!rownames(input_and_pp_and_var_df)%in%train_ind,],
                                        lr_maxiter = lr_maxiter,
                                        alpha = alpha,
                                        lambda = lambda
      )
    }else if(supervised_model %in% c("logistic regression", "logistic")){
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
    dt_y_test_tmp <- metrics_j[[3]]
    dt_y_test_tmp$pcd <- j
    dt_y_test <- rbind(dt_y_test,dt_y_test_tmp)
    res_roc_test <- rbind(res_roc_test,metrics_j[[2]])
    metrics_j <- metrics_j[[1]]

    accgmc_test[1,j] <- metrics_j[1]
    aucmc_test[1,j] <- metrics_j[2]
    sensgmc_test[1,j] <- metrics_j[3]
    spcgmc_test[1,j] <- metrics_j[4]
    kappamc_test[1,j] <- metrics_j[5]
  }
  res_roc_test <- aggregate(res_roc_test[,c("TPR","FPR")],by=list(res_roc_test$threshold),mean)

  if(sum(is.na(accgmc_test)) > r_pseudo_threshold){
    acc_m_test <- NA
    acc_d_test <- NA
  }else{
    acc_m_test <- mean(accgmc_test,na.rm = TRUE)
    acc_d_test <- (var(accgmc_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(accgmc_test[1,]))))^.5
  }
  if(sum(is.na(aucmc_test)) > r_pseudo_threshold){
    auc_m_test <- NA
    auc_d_test <- NA
  }else{
    auc_m_test <- mean(aucmc_test,na.rm = TRUE)
    auc_d_test <- (var(aucmc_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(aucmc_test[1,]))))^.5
  }

  if(sum(is.na(sensgmc_test)) > r_pseudo_threshold){
    sensg_m_test <- NA
    sensg_d_test <- NA
  }else{
    sensg_m_test <- mean(sensgmc_test,na.rm = TRUE)
    sensg_d_test <- (var(sensgmc_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(sensgmc_test[1,]))))^.5
  }

  if(sum(is.na(sensgmc_test)) > r_pseudo_threshold){
    spcg_m_test <- NA
    spcg_d_test <- NA
  }else{
    spcg_m_test <- mean(spcgmc_test,na.rm = TRUE)
    spcg_d_test <- (var(spcgmc_test[1,],na.rm = TRUE)/c(r_pseudo - sum(is.na(spcgmc_test[1,]))))^.5
  }

  if(sum(is.na(kappamc_test)) > r_pseudo_threshold){
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
  res_vec <- c(res_matrix,res_vec_test)
  res_roc_train$dataset <- "train"
  res_roc_test$dataset <- "test"
  return(list(res_vec,rbind(res_roc_train,res_roc_test),dt_y_test))
}
