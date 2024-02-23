dichPseudoBestZAModel_Cont <- function(folder_path,
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
  dt_comb <- allCombOfAModelOpt(dt_rmna, n)

  kappas_no_cv_no_pcd <- c()
  print('###start to filter####')
  s <- Sys.time()
  ##get kappas without cv and pcd
  for (mComb in dt_comb) {
    #print("################")

    cur_kappa <- apply(
      mComb,
      2,
      FUN = function(x) {
        dt_rmna$cluster <- ifelse(x >= optimize_prob_thresh, 1, 0)
        if (class(validator1) %in% c("validator_flip", "validator_continuous")) {
          validator1$n <- n
          genLinearRegressionOpt(
            y = dt_rmna[, "variable_start_to_end"],
            predictor_part = predictors(validator1),
            input_and_pp_and_var_df = dt_rmna
          )[[1]][1]
        } else{
          genLinearRegressionOpt(
            y = dt_rmna$cluster,
            predictor_part = predictors(validator1),
            input_and_pp_and_var_df = dt_rmna
          )[[1]][1]

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

  #use_combinations <- kappas_no_cv_no_pcd
  use_combinations <- data.frame(
    "kappas" = kappas_no_cv_no_pcd, # kappa here is MSE
    "comb_name" = names(kappas_no_cv_no_pcd),
    "n" = n
  )
  return(use_combinations)
}
