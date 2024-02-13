calculateMetricsForAProbSplitNoPCDCategory.validator_continuous <-
  function(validator,
           categoryVec,
           K_fold,
           repeated_folds_R,
           input_and_pp_and_var_df,
           seed_num,
           validation_data_fraction,
           train_ind,
           lr_maxiter,
           n,
           pcd_dropping_pct) {
    validator$n <- n
    alpha <- validator$alpha
    lambda <- validator$lambda
    supervised_model <- validator$supervised_model
    predictor_part <- predictors(validator)
    cat_vec_train <- categoryVec[train_ind, 1]
    cat_vec_test <-
      categoryVec[!rownames(categoryVec) %in% train_ind, 1]
    input_dt_train <- input_and_pp_and_var_df[train_ind, ]
    input_dt_test <-
      input_and_pp_and_var_df[!rownames(input_and_pp_and_var_df) %in% train_ind, ]
    #print(input_dt_train[,paste("P",1:n,sep = "")])
    ##results with CV
    set.seed(seed_num['seed_num_kfold'])
    myfold <- cvTools::cvFolds(length(train_ind), K_fold, R = repeated_folds_R)
    res_matrix <- c()
    if (length(pcd_dropping_pct) == 1) {
      pcd_dropping_pct = rep(pcd_dropping_pct, 3)
    }
    K_fold_threshold = round(pcd_dropping_pct[2] * K_fold)
    r_threshold = round(pcd_dropping_pct[3] * repeated_folds_R)
    input_dt_train$cluster <- cat_vec_train
    input_dt_test$cluster <- cat_vec_test
    res_roc_train <- data.frame()
    if(!sjmisc::is_empty(seed_num['seed_num_regression_model'])){
      set.seed(seed_num['seed_num_regression_model'])
    }

    for (r in 1:repeated_folds_R) {

      MSE = matrix(NA, K_fold, 1)
      RMSE = matrix(NA, K_fold, 1)
      MAE = matrix(NA, K_fold, 1)
      r_square = matrix(NA, K_fold, 1)
      adj_r_square = matrix(NA, K_fold, 1)
      aic = matrix(NA, K_fold, 1)

      for (i in 1:K_fold) {
        if (supervised_model %in% c("linear regression", "lr")) {
          metrics_ij <- genLinearRegression(
            cvFolds_dt = myfold,
            y = input_dt_train[, "variable_start_to_end"],
            predictor_part = predictor_part,
            cur_fold = i,
            cur_r = r,
            input_and_pp_and_var_df = input_dt_train
          )
        }
        metrics_ij <- metrics_ij[[1]]
        MSE[i, 1] <- metrics_ij[1]
        RMSE[i, 1] <- metrics_ij[2]
        MAE[i, 1] <- metrics_ij[3]
        r_square[i, 1] <- metrics_ij[4]
        adj_r_square[i, 1] <- metrics_ij[5]
        aic[i, 1] <- metrics_ij[6]
      }
      # print("sensitivity is")
      # print(sensgmc12)
      # print("specificity is")
      # print(spcgmc12)

      if (sum(is.na(MSE[, 1])) > K_fold_threshold){
        MSE_m <- NA
        MSE_d <- NA
      }else{
        MSE_m <- mean(MSE, na.rm = TRUE)
        MSE_d <-
          (var(MSE[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(MSE[, 1])))) ^
          .5
      }
      if (sum(is.na(RMSE[, 1])) > K_fold_threshold){
        RMSE_m <- NA
        RMSE_d <- NA
      }else{
        RMSE_m <- mean(RMSE, na.rm = TRUE)
        RMSE_d <-
          (var(RMSE[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(RMSE[, 1])))) ^
          .5
      }

      if (sum(is.na(MAE[, 1])) > K_fold_threshold){
        MAE_m <- NA
        MAE_d <- NA
      }else{
        MAE_m <- mean(MAE, na.rm = TRUE)
        MAE_d <-
          (var(MAE[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(MAE[, 1])))) ^
          .5
      }
      if (sum(is.na(r_square[, 1])) > K_fold_threshold){
        r_square_m <- NA
        r_square_d <- NA
      }else{
        r_square_m <- mean(r_square, na.rm = TRUE)
        r_square_d <-
          (var(r_square[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(r_square[, 1])))) ^
          .5
      }
      if (sum(is.na(adj_r_square[, 1])) > K_fold_threshold) {
        adj_r_square_m <- NA
        adj_r_square_d <- NA
      } else{
        adj_r_square_m <- mean(adj_r_square, na.rm = TRUE)
        adj_r_square_d <-
          (var(adj_r_square[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(adj_r_square[, 1])))) ^
          .5
      }
      if (sum(is.na(aic[, 1])) > K_fold_threshold) {
        aic_m <- NA
        aic_d <- NA
      } else{
        aic_m <- mean(aic, na.rm = TRUE)
        aic_d <-
          (var(aic[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(aic[, 1])))) ^
          .5
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
      res_matrix <- c(res_matrix, res_vec)
    }
    res_matrix <-
      data.frame(matrix(res_matrix, nrow = repeated_folds_R, byrow = TRUE))
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
    res_matrix <- colMeans(res_matrix, na.rm = TRUE)
    ##results without CV
    if (supervised_model %in% c("linear regression", "lr")) {
      metrics_j <-
        genLinearRegressionNoCV(
          y = input_dt_train[, "variable_start_to_end"],
          predictor_part = predictor_part,
          input_and_pp_and_var_df = input_dt_train,
          y_test = input_dt_test[, "variable_start_to_end"],
          input_and_pp_and_var_df_test = input_dt_test
        )
    }

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
    res_vec <- c(res_matrix, res_vec_test)
    return(list(res_vec, NULL, dt_y_test))
  }
