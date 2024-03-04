calculateMetricsForAProb.validator_continuous <- function(validator,
                                                    pseudo_a_prob,
                                                    r_pseudo,
                                                    K_fold,
                                                    repeated_folds_R,
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
  myfold <- cvTools::cvFolds(nrow(pseudo_a_prob), K_fold, R = repeated_folds_R)
  res_matrix <- c()
  if(length(pcd_dropping_pct) == 1){
    pcd_dropping_pct = rep(pcd_dropping_pct,3)
  }
  r_pseudo_threshold = round(pcd_dropping_pct[1]*r_pseudo)
  K_fold_threshold = round(pcd_dropping_pct[2]*K_fold)
  r_threshold = round(pcd_dropping_pct[3]*repeated_folds_R)

  if(!sjmisc::is_empty(seed_num['seed_num_regression_model'])){
    set.seed(seed_num['seed_num_regression_model'])
  }

  MSE_m_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)
  MSE_v_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)

  RMSE_m_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)
  RMSE_v_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)

  MAE_m_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)
  MAE_v_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)

  r_square_m_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)
  r_square_v_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)

  adj_r_square_m_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)
  adj_r_square_v_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)

  aic_m_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)
  aic_v_perRepPCD=matrix(NA,nrow=r_pseudo,ncol=repeated_folds_R)

  for(r in 1:repeated_folds_R){

    MSE = matrix(NA, K_fold, r_pseudo)
    RMSE = matrix(NA, K_fold, r_pseudo)
    MAE = matrix(NA, K_fold, r_pseudo)
    r_square = matrix(NA, K_fold, r_pseudo)
    adj_r_square = matrix(NA, K_fold, r_pseudo)
    aic = matrix(NA, K_fold, r_pseudo)


    for(i in 1:K_fold){
      for(j in 1:r_pseudo){
        input_and_pp_and_var_df$cluster <- pseudo_a_prob[,j]

        if (supervised_model %in% c("linear regression", "lr")) {
          metrics_ij <- genLinearRegression(
            cvFolds_dt = myfold,
            y = input_and_pp_and_var_df[, "variable_start_to_end"],
            predictor_part = predictor_part,
            cur_fold = i,
            cur_r = r,
            input_and_pp_and_var_df = input_and_pp_and_var_df
          )
        }

        metrics_ij <- metrics_ij[[1]]
        MSE[i, j] <- metrics_ij[1]
        RMSE[i, j] <- metrics_ij[2]
        MAE[i, j] <- metrics_ij[3]
        r_square[i, j] <- metrics_ij[4]
        adj_r_square[i, j] <- metrics_ij[5]
        aic[i, j] <- metrics_ij[6]
      }
    }

    # sensitivity_na_count_k  <- apply(sensgmc12, 2, FUN = function(x){sum(is.na(x))})
    # specification_na_count_k <- apply(spcgmc12, 2, FUN = function(x){sum(is.na(x))})
    # accuracy_na_count_k <- apply(accgmc12, 2, FUN = function(x){sum(is.na(x))})
    # auc_na_count_k <- apply(aucmc12, 2, FUN = function(x){sum(is.na(x))})
    # kappa_na_count_k <- apply(kappamc12, 2, FUN = function(x){sum(is.na(x))})
    #
    # sensitivity_na_count_k_larger_id <- which(sensitivity_na_count_k > K_fold_threshold)
    # specification_na_count_k_larger_id <- which(specification_na_count_k > K_fold_threshold)
    # accuracy_na_count_k_larger_id <- which(accuracy_na_count_k > K_fold_threshold)
    # auc_na_count_k_larger_id <- which(auc_na_count_k > K_fold_threshold)
    # kappa_na_count_k_larger_id <- which(kappa_na_count_k > K_fold_threshold)

    MSE_m_k <- colMeans(MSE,na.rm = TRUE)
    #MSE_m_k[accuracy_na_count_k_larger_id] <- NA
    MSE_d_k <- apply(MSE,2,var,na.rm = TRUE)/(K_fold - apply(MSE,2,FUN=function(x)sum(is.na(x))))
    #acc_d_k[accuracy_na_count_k_larger_id] <- NA

    RMSE_m_k <- colMeans(RMSE,na.rm = TRUE)
    #auc_m_k[auc_na_count_k_larger_id] <- NA
    RMSE_d_k <- apply(RMSE,2,var,na.rm = TRUE)/(K_fold - apply(RMSE,2,FUN=function(x)sum(is.na(x))))
    #auc_d_k[auc_na_count_k_larger_id] <- NA

    MAE_m_k <- colMeans(MAE,na.rm = TRUE)
    #sensg_m_k[sensitivity_na_count_k_larger_id] <- NA
    MAE_d_k <- apply(MAE,2,var,na.rm = TRUE)/(K_fold - apply(MAE,2,FUN=function(x)sum(is.na(x))))
    #sensg_d_k[sensitivity_na_count_k_larger_id] <- NA

    r_square_m_k <- colMeans(r_square,na.rm = TRUE)
    #spcg_m_k[specification_na_count_k_larger_id] <- NA
    r_square_d_k <- apply(r_square,2,var,na.rm = TRUE)/(K_fold - apply(r_square,2,FUN=function(x)sum(is.na(x))))
    #spcg_d_k[specification_na_count_k_larger_id] <- NA

    adj_r_square_m_k <- colMeans(adj_r_square,na.rm = TRUE)
    #kappa_m_k[kappa_na_count_k_larger_id] <- NA
    adj_r_square_d_k <- apply(adj_r_square,2,var,na.rm = TRUE)/(K_fold - apply(adj_r_square,2,FUN=function(x)sum(is.na(x))))
    #kappa_d_k[kappa_na_count_k_larger_id] <- NA
    aic_m_k <- colMeans(aic,na.rm = TRUE)
    #MSE_m_k[accuracy_na_count_k_larger_id] <- NA
    aic_d_k <- apply(aic,2,var,na.rm = TRUE)/(K_fold - apply(aic,2,FUN=function(x)sum(is.na(x))))


    if(sum(is.na(MSE_m_k)) > r_pseudo_threshold){
      MSE_m <- NA
      MSE_d <- NA
    }else{
      MSE_m_perRepPCD[,r] <- (MSE_m_k)
      MSE_v_perRepPCD[,r] <- (MSE_d_k)
      MSE_m <- mean(MSE_m_k,na.rm = TRUE)
      MSE_d <- (mean(MSE_d_k,na.rm = TRUE) + var(MSE_m_k,na.rm = TRUE))^0.5
    }
    if(sum(is.na(RMSE_m_k)) > r_pseudo_threshold){
      RMSE_m <- NA
      RMSE_d <- NA
    }else{
      RMSE_m_perRepPCD[,r] <- (RMSE_m_k)
      RMSE_v_perRepPCD[,r] <- (RMSE_d_k)
      RMSE_m <- mean(RMSE_m_k,na.rm = TRUE)
      RMSE_d <- (mean(RMSE_d_k,na.rm = TRUE) + var(RMSE_m_k,na.rm = TRUE))^0.5
    }
    if(sum(is.na(MAE_m_k)) > r_pseudo_threshold){
      MAE_m <- NA
      MAE_d <- NA
    }else{
      MAE_m_perRepPCD[,r] <- (MAE_m_k)
      MAE_v_perRepPCD[,r] <- (MAE_d_k)
      MAE_m <- mean(MAE_m_k,na.rm = TRUE)
      MAE_d <- (mean(MAE_d_k,na.rm = TRUE) + var(MAE_m_k,na.rm = TRUE))^0.5
    }
    if(sum(is.na(r_square_m_k)) > r_pseudo_threshold){
      r_square_m <- NA
      r_square_d <- NA
    }else{
      r_square_m_perRepPCD[,r] <- (r_square_m_k)
      r_square_v_perRepPCD[,r] <- (r_square_d_k)
      r_square_m <- mean(r_square_m_k,na.rm=TRUE)
      r_square_d <- (mean(r_square_d_k,na.rm = TRUE) + var(r_square_m_k,na.rm = TRUE))^0.5
    }
    if(sum(is.na(adj_r_square_m_k)) > r_pseudo_threshold){
      adj_r_square_m <- NA
      adj_r_square_d <- NA
    }else{
      adj_r_square_m_perRepPCD[,r] <- (adj_r_square_m_k)
      adj_r_square_v_perRepPCD[,r] <- (adj_r_square_d_k)
      adj_r_square_m <- mean(adj_r_square_m_k,na.rm=TRUE)
      adj_r_square_d <- (mean(adj_r_square_d_k,na.rm = TRUE) + var(adj_r_square_m_k,na.rm = TRUE))^0.5
    }
    if(sum(is.na(aic_m_k)) > r_pseudo_threshold){
      aic_m <- NA
      aic_d <- NA
    }else{
      aic_m_perRepPCD[,r] <- (aic_m_k)
      aic_v_perRepPCD[,r] <- (aic_d_k)
      aic_m <- mean(aic_m_k,na.rm = TRUE)
      aic_d <- (mean(aic_d_k,na.rm = TRUE) + var(aic_m_k,na.rm = TRUE))^0.5
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
    res_matrix <- c(res_matrix,res_vec)
  }
  res_matrix <- data.frame(matrix(res_matrix, nrow = repeated_folds_R, byrow = TRUE))

  MSE_m_overall=mean(MSE_m_perRepPCD, na.rm = TRUE)
  MSE_v_overall=(mean(MSE_v_perRepPCD, na.rm = TRUE)+ var(c(MSE_m_perRepPCD), na.rm = TRUE)/(r_pseudo*repeated_folds_R)) ^ 0.5

  RMSE_m_overall=mean(RMSE_m_perRepPCD, na.rm = TRUE)
  RMSE_v_overall=(mean(RMSE_v_perRepPCD, na.rm = TRUE)+ var(c(RMSE_m_perRepPCD), na.rm = TRUE)/(r_pseudo*repeated_folds_R)) ^ 0.5

  MAE_m_overall=mean(MAE_m_perRepPCD)
  MAE_v_overall=(mean(MAE_v_perRepPCD)+ var(c(MAE_m_perRepPCD), na.rm = TRUE)/(r_pseudo*repeated_folds_R)) ^ 0.5

  r_square_m_overall=mean(r_square_m_perRepPCD, na.rm = TRUE)
  r_square_v_overall=(mean(r_square_v_perRepPCD, na.rm = TRUE)+ var(c(r_square_m_perRepPCD), na.rm = TRUE)/(r_pseudo*repeated_folds_R)) ^ 0.5

  adj_r_square_m_overall=mean(adj_r_square_m_perRepPCD, na.rm = TRUE)
  adj_r_square_v_overall=(mean(adj_r_square_v_perRepPCD, na.rm = TRUE)+ var(c(adj_r_square_m_perRepPCD), na.rm = TRUE)/(r_pseudo*repeated_folds_R)) ^ 0.5

  aic_m_overall=mean(aic_m_perRepPCD, na.rm = TRUE)
  aic_v_overall=(mean( aic_v_perRepPCD, na.rm = TRUE)+ var(c(aic_m_perRepPCD), na.rm = TRUE)/(r_pseudo*repeated_folds_R)) ^ 0.5
  print(colMeans(res_matrix,na.rm = TRUE))
  res_matrix <- c(MSE_m_overall, MSE_v_overall,
                  RMSE_m_overall, RMSE_v_overall,
                  MAE_m_overall, MAE_v_overall,
                  r_square_m_overall, r_square_v_overall,
                  adj_r_square_m_overall, adj_r_square_v_overall,
                  aic_m_overall, aic_v_overall)

  names(res_matrix) <- c(
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
  print(res_matrix)
  #res_matrix <- colMeans(res_matrix,na.rm = TRUE)
  return(list(res_matrix))
}
