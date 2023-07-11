dichPseudoBestZAModelCategory <- function(folder_path,
                                          ##model classes
                                          n,
                                          input_dt,
                                          validators,
                                          seed_num,
                                          kappa_filter_threshold,
                                          if_listwise_deletion,
                                          y_names,
                                          lr_maxiter,
                                          optimize_prob_thresh = 0.5,
                                          pp_dt = NULL) {
  ##impute missing data
  if(is.null(pp_dt)){
    pp_dt <- getAProbFromResultPath(folder_path, n)
  }
  validator1 <- validators[[1]]
  ##impute missing data
  dt_rmna <- dichPseudoPrepare(
    validator = validator1,
    pp_dt = pp_dt,
    ##model classes
    n = n,
    input_dt = input_dt,
    if_listwise_deletion = if_listwise_deletion,
    y_names = y_names
  )
  dt_rnames <- rownames(dt_rmna)

  ##calculate combination of probabilities
  if(!is.null(pp_dt)){
    dt_comb <- allCombOfAModelFromCategoryOpt(dt_rmna, n)
  }else{
    dt_comb <- list()
    for(i in 1:(n-1)){
      tmp_list <- chooseMClassFromNOpt(class_cols_dt=dt_rmna[,c((ncol(dt_rmna)-2),(ncol(dt_rmna) - 1),ncol(dt_rmna))],m=i,n=n)
      dt_comb[[i]] <- tmp_list
    }
  }

  kappas_no_cv_no_pcd <- c()
  print('###start to filter####')
  s <- Sys.time()
  ##get kappas without cv and pcd
  for (mComb in dt_comb) {
    #print("################")
    rownames(mComb) <- dt_rnames
    cur_kappa <- apply(
      mComb,
      2,
      FUN = function(x) {
        dt_rmna$cluster <- ifelse(x >= optimize_prob_thresh, 1, 0)
        if (class(validator1) %in% c("validator_flip")) {
          validator1$n <- n
          genLogisticMetricsOpt(
            y = dt_rmna[, "variable_start_to_end"],
            predictor_part = predictors(validator1),
            input_and_pp_and_var_df = dt_rmna,
            lr_maxiter = lr_maxiter
          )[[1]][5]
        } else{
          genLogisticMetricsOpt(
            y = dt_rmna$cluster,
            predictor_part = predictors(validator1),
            input_and_pp_and_var_df = dt_rmna,
            lr_maxiter = lr_maxiter
          )[[1]][5]

        }
      }
    )
    kappas_no_cv_no_pcd <- append(kappas_no_cv_no_pcd, cur_kappa)
  }
  kappas_no_cv_no_pcd <- sort(kappas_no_cv_no_pcd, decreasing = TRUE)
  if (!is.null(kappa_filter_threshold)) {
    if (length(kappas_no_cv_no_pcd) >= kappa_filter_threshold) {
      kappas_no_cv_no_pcd <- kappas_no_cv_no_pcd[1:kappa_filter_threshold]
    }
  }
  print('####end filter#####')
  e <- Sys.time()
  print("filter time is: ")
  print(e - s)

  use_combinations <- data.frame(
    "kappas" = kappas_no_cv_no_pcd,
    "comb_name" = names(kappas_no_cv_no_pcd),
    "n" = n
  )
  return(use_combinations)
}
