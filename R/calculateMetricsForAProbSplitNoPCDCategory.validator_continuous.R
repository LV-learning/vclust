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
    input_and_pp_and_var_df$cluster <- categoryVec
    res_roc_train <- data.frame()
    if(!sjmisc::is_empty(seed_num['seed_num_regression_model'])){
      set.seed(seed_num['seed_num_regression_model'])
    }
    mean_sd_dt_train <- data.frame()
    mean_sd_dt_test <- data.frame()
    coefficients = data.frame()

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

        gsd <- var(input_and_pp_and_var_df[input_and_pp_and_var_df$cluster == 1,validator$contVarName], na.rm=TRUE)^0.5
        mean_sd_dt_train <- rbind(mean_sd_dt_train, data.frame(repeated = r,
                                                   kfold = i,
                                                   mean = metrics_ij[[2]],
                                                   sd = gsd,
                                                   n = length(input_and_pp_and_var_df[input_and_pp_and_var_df$cluster == 1,validator$contVarName]),
                                                   train_or_test = "train"
                                                   ))

        coeff_df <- metrics_ij[[3]]
        coeff_df$covariates <- row.names(coeff_df)
        coefficients <- rbind(coefficients,coeff_df)
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
          (var(MSE[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(MSE[, 1]))))
      }
      if (sum(is.na(RMSE[, 1])) > K_fold_threshold){
        RMSE_m <- NA
        RMSE_d <- NA
      }else{
        RMSE_m <- mean(RMSE, na.rm = TRUE)
        RMSE_d <-
          (var(RMSE[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(RMSE[, 1]))))
      }

      if (sum(is.na(MAE[, 1])) > K_fold_threshold){
        MAE_m <- NA
        MAE_d <- NA
      }else{
        MAE_m <- mean(MAE, na.rm = TRUE)
        MAE_d <-
          (var(MAE[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(MAE[, 1]))))
      }
      if (sum(is.na(r_square[, 1])) > K_fold_threshold){
        r_square_m <- NA
        r_square_d <- NA
      }else{
        r_square_m <- mean(r_square, na.rm = TRUE)
        r_square_d <-
          (var(r_square[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(r_square[, 1]))))
      }
      if (sum(is.na(adj_r_square[, 1])) > K_fold_threshold) {
        adj_r_square_m <- NA
        adj_r_square_d <- NA
      } else{
        adj_r_square_m <- mean(adj_r_square, na.rm = TRUE)
        adj_r_square_d <-
          (var(adj_r_square[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(adj_r_square[, 1]))))
      }
      if (sum(is.na(aic[, 1])) > K_fold_threshold) {
        aic_m <- NA
        aic_d <- NA
      } else{
        aic_m <- mean(aic, na.rm = TRUE)
        aic_d <-
          (var(aic[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(aic[, 1]))))
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
    print(coefficients)
    coefficients <- coefficients %>% group_by(covariates) %>% summarise(mean=mean(Estimate), SD=sd(Estimate), SE = mean(`Std. Error`))
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
    #res_matrix <- colMeans(res_matrix, na.rm = TRUE)
    print('####')
    print(colMeans(res_matrix, na.rm = TRUE))
    print('####')
    #acc_d <- (mean(acc_d_k,na.rm = TRUE) + var(acc_m_k,na.rm = TRUE))^0.5
    res_matrix <- res_matrix %>% summarise(MSE_m_m = mean(MSE_m), MSE_d_d = (mean(MSE_d) + if_else(is.na(var(MSE_m)), 0, var(MSE_m)))^0.5,
                             RMSE_m_m = mean(RMSE_m), RMSE_d_d = (mean(RMSE_d) + if_else(is.na(var(RMSE_m)), 0, var(RMSE_m)))^0.5,
                             MAE_m_m = mean(MAE_m), MAE_d_d = (mean(MAE_d) + if_else(is.na(var(MAE_m)), 0, var(MAE_m)))^0.5,
                             r_square_m_m = mean(r_square_m), r_square_d_d = (mean(r_square_d) + if_else(is.na(var(r_square_m)), 0, var(r_square_m)))^0.5,
                             adj_r_square_m_m = mean(adj_r_square_m), adj_r_square_d_d = (mean(adj_r_square_d) + if_else(is.na(var(adj_r_square_m)), 0, var(adj_r_square_m)))^0.5,
                             aic_m_m = mean(aic_m), aic_d_d = (mean(aic_d) + if_else(is.na(var(aic_m)), 0, var(aic_m)))^0.5,
                             )
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
    res_vec <- c(res_matrix, res_vec_test)
    return(list(list(res_matrix=as.data.frame(t(res_vec)),
                     mean_sd_dt=rbind(mean_sd_dt_train, mean_sd_dt_test),
                     coefficients=coefficients),
                NULL,
                dt_y_test))
  }
