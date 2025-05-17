#' @importFrom  stats aggregate var
calculateMetricsForAProbNoPCDCategory.validator_direct <-
  function(validator,
           categoryVec,
           K_fold,
           repeated_folds_R,
           input_and_pp_and_var_df,
           seed_num,
           lr_maxiter,
           n,
           pcd_dropping_pct) {
    #categoryVec <- as.factor(categoryVec)
    ##handle some NAs
    alpha <- validator$alpha
    lambda <- validator$lambda
    supervised_model <- validator$supervised_model
    predictor_part <- predictors(validator)
    set.seed(seed_num['seed_num_kfold'])
    myfold <-
      cvTools::cvFolds(length(categoryVec), K_fold, R = repeated_folds_R)
    res_matrix <- c()
    if (length(pcd_dropping_pct) == 1) {
      pcd_dropping_pct = rep(pcd_dropping_pct, 3)
    }
    K_fold_threshold = round(pcd_dropping_pct[2] * K_fold)
    r_threshold = round(pcd_dropping_pct[3] * repeated_folds_R)
    roc_res <- data.frame()
    if(!sjmisc::is_empty(seed_num['seed_num_supervised_model'])){
      set.seed(seed_num['seed_num_supervised_model'])
    }
    coefficients = data.frame()
    for (r in 1:repeated_folds_R) {
      accgmc12 = matrix(NA, K_fold, 1)
      aucmc12 = matrix(NA, K_fold, 1)
      sensgmc12 = matrix(NA, K_fold, 1)
      spcgmc12 = matrix(NA, K_fold, 1)
      kappamc12 = matrix(NA, K_fold, 1)

      for (i in 1:K_fold) {
        if (supervised_model == "glmnet") {
          metrics_ij <- genGlmnetMetrics(
            cvFolds_dt = myfold,
            y = categoryVec,
            predictor_part = predictor_part,
            cur_fold = i,
            cur_r = r,
            input_and_pp_and_var_df = input_and_pp_and_var_df,
            lr_maxiter = lr_maxiter,
            alpha = alpha,
            lambda = lambda
          )
        } else if (supervised_model %in% c("logistic regression", "logistic")) {
          metrics_ij <- genLogisticMetrics(
            cvFolds_dt = myfold,
            y = categoryVec,
            predictor_part = predictor_part,
            cur_fold = i,
            cur_r = r,
            input_and_pp_and_var_df = input_and_pp_and_var_df,
            lr_maxiter = lr_maxiter
          )
        }else if (supervised_model %in% c(NULL,"no model")){
          metrics_ij <- genMetricsABinaryX(
            cvFolds_dt = myfold,
            y = categoryVec,
            predictor_part = predictor_part,
            cur_fold = i,
            cur_r = r,
            input_and_pp_and_var_df = input_and_pp_and_var_df,
            lr_maxiter = lr_maxiter
          )
        }
        coeff_df <- metrics_ij[[3]]
        coeff_df$covariates <- row.names(coeff_df)
        coefficients <- rbind(coefficients,coeff_df)
        roc_res <- rbind(roc_res, metrics_ij[[2]])
        metrics_ij <- metrics_ij[[1]]
        accgmc12[i, 1] <- metrics_ij[1]
        aucmc12[i, 1] <- metrics_ij[2]
        sensgmc12[i, 1] <- metrics_ij[3]
        spcgmc12[i, 1] <- metrics_ij[4]
        kappamc12[i, 1] <- metrics_ij[5]

      }

      if (sum(is.na(accgmc12[, 1])) > K_fold_threshold){
        acc_m <- NA
        acc_d <- NA
      }else{
        acc_m <- mean(accgmc12, na.rm = TRUE)
        acc_d <-
          (var(accgmc12[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(accgmc12[, 1])))) ^
          .5
      }
      if (sum(is.na(aucmc12[, 1])) > K_fold_threshold){
        auc_m <- NA
        auc_d <- NA
      }else{
        auc_m <- mean(aucmc12, na.rm = TRUE)
        auc_d <-
          (var(aucmc12[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(aucmc12[, 1])))) ^
          .5
      }

      if (sum(is.na(sensgmc12[, 1])) > K_fold_threshold){
        sensg_m <- NA
        sensg_d <- NA
      }else{
        sensg_m <- mean(sensgmc12, na.rm = TRUE)
        sensg_d <-
          (var(sensgmc12[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(sensgmc12[, 1])))) ^
          .5
      }
      if (sum(is.na(spcgmc12[, 1])) > K_fold_threshold){
        spcg_m <- NA
        spcg_d <- NA
      }else{
        spcg_m <- mean(spcgmc12, na.rm = TRUE)
        spcg_d <-
          (var(spcgmc12[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(spcgmc12[, 1])))) ^
          .5
      }
      if (sum(is.na(kappamc12[, 1])) > K_fold_threshold) {
        kappa_m <- NA
        kappa_d <- NA
      } else{
        kappa_m <- mean(kappamc12, na.rm = TRUE)
        kappa_d <-
          (var(kappamc12[, 1], na.rm = TRUE) / c(K_fold - sum(is.na(kappamc12[, 1])))) ^
          .5
      }
      res_vec <-
        c(acc_m,
          acc_d,
          auc_m,
          auc_d,
          sensg_m,
          sensg_d,
          spcg_m,
          spcg_d,
          kappa_m,
          kappa_d)
      res_matrix <- c(res_matrix, res_vec)
    }
    print(coefficients)
    coefficients <- coefficients %>% group_by(covariates) %>% summarise(mean=mean(Estimate), SD=sd(Estimate), SE = mean(`Std. Error`))
    res_matrix <-
      data.frame(matrix(res_matrix, nrow = repeated_folds_R, byrow = TRUE))
    roc_res <-
      aggregate(roc_res[, c("TPR", "FPR")], by = list(roc_res$threshold), mean)
    names(res_matrix) <- c(
      'acc_m',
      'acc_d',
      'auc_m',
      'auc_d',
      'sensg_m',
      'sensg_d',
      'spcg_m',
      'spcg_d',
      'kappa_m',
      'kappa_d'
    )
    res_matrix <- colMeans(res_matrix, na.rm = TRUE)
    return(list(res_matrix, roc_res,coefficients=coefficients))
  }
