#' @importFrom  stats aggregate var
calculateMetricsForAProb.validator_direct <- function(validator,
                                                      pseudo_a_prob,
                                                      r_pseudo,
                                                      K_fold,
                                                      repeated_folds_R,
                                                      input_and_pp_and_var_df,
                                                      seed_num,
                                                      lr_maxiter,
                                                      n,
                                                      pcd_dropping_pct
){
  ##handle some NAs
  alpha <- validator$alpha
  lambda <- validator$lambda
  supervised_model <- validator$supervised_model
  predictor_part <- predictors(validator)
  set.seed(seed_num['seed_num_kfold'])
  myfold <- cvTools::cvFolds(nrow(pseudo_a_prob), K_fold, R = repeated_folds_R)
  res_matrix <- c()
  if(length(pcd_dropping_pct) == 1){
    pcd_dropping_pct = rep(pcd_dropping_pct,3)
  }
  r_pseudo_threshold = round(pcd_dropping_pct[1]*r_pseudo)
  K_fold_threshold = round(pcd_dropping_pct[2]*K_fold)
  r_threshold = round(pcd_dropping_pct[3]*repeated_folds_R)
  roc_res <- data.frame()
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
                                         y = pseudo_a_prob[,j],
                                         predictor_part = predictor_part,
                                         cur_fold = i,
                                         cur_r = r,
                                         input_and_pp_and_var_df = input_and_pp_and_var_df,
                                         lr_maxiter = lr_maxiter,
                                         alpha = alpha,
                                         lambda = lambda)
        }else if(supervised_model %in% c("logistic regression", "logistic")){
          metrics_ij <- genLogisticMetrics(cvFolds_dt = myfold,
                                           y = pseudo_a_prob[,j],
                                           predictor_part = predictor_part,
                                           cur_fold = i,
                                           cur_r = r,
                                           input_and_pp_and_var_df = input_and_pp_and_var_df,
                                           lr_maxiter = lr_maxiter)
        }else if (supervised_model %in% c(NULL,"no model")){
          metrics_ij <- genMetricsABinaryX(cvFolds_dt = myfold,
                                           y = pseudo_a_prob[,j],
                                           predictor_part = predictor_part,
                                           cur_fold = i,
                                           cur_r = r,
                                           input_and_pp_and_var_df = input_and_pp_and_var_df,
                                           lr_maxiter = lr_maxiter)
        }

        coeff_df <- metrics_ij[[3]]
        coeff_df$covariates <- row.names(coeff_df)
        coefficients <- rbind(coefficients,coeff_df)

        roc_res <- rbind(roc_res,metrics_ij[[2]])
        metrics_ij <- metrics_ij[[1]]
        accgmc12[i,j] <- metrics_ij[1]
        aucmc12[i,j] <- metrics_ij[2]
        sensgmc12[i,j] <- metrics_ij[3]
        spcgmc12[i,j] <- metrics_ij[4]
        kappamc12[i,j] <- metrics_ij[5]
      }
    }

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
      auc_d <- NA
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
  roc_res <- aggregate(roc_res[,c("TPR","FPR")],by=list(roc_res$threshold),mean)

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

  print(colMeans(res_matrix,na.rm = TRUE))

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
  print(res_matrix)
  #res_matrix <- colMeans(res_matrix,na.rm = TRUE)
  return(list(res_matrix,roc_res,coefficients=coefficients))
}
